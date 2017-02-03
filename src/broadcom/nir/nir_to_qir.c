/*
 * Copyright © 2016 Broadcom
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include <inttypes.h>
#include "util/u_format.h"
#include "util/u_math.h"
#include "util/u_memory.h"
#include "util/ralloc.h"
#include "util/hash_table.h"
#include "tgsi/tgsi_dump.h"
#include "tgsi/tgsi_parse.h"
#include "compiler/nir/nir.h"
#include "compiler/nir/nir_builder.h"
#include "nir/tgsi_to_nir.h"
#include "vc5_compiler.h"
#include "mesa/state_tracker/st_glsl_types.h"

/* We don't do any address packing. */
#define __gen_user_data void
#define __gen_address_type uint32_t
#define __gen_address_offset(reloc) (*reloc)
#define __gen_emit_reloc(cl, reloc)
#include "cle/v3d_packet_v33_pack.h"

static struct qreg
ntq_get_src(struct vc5_compile *c, nir_src src, int i);
static void
ntq_emit_cf_list(struct vc5_compile *c, struct exec_list *list);

static void
resize_qreg_array(struct vc5_compile *c,
                  struct qreg **regs,
                  uint32_t *size,
                  uint32_t decl_size)
{
        if (*size >= decl_size)
                return;

        uint32_t old_size = *size;
        *size = MAX2(*size * 2, decl_size);
        *regs = reralloc(c, *regs, struct qreg, *size);
        if (!*regs) {
                fprintf(stderr, "Malloc failure\n");
                abort();
        }

        for (uint32_t i = old_size; i < *size; i++)
                (*regs)[i] = c->undef;
}

static struct qreg
indirect_uniform_load(struct vc5_compile *c, nir_intrinsic_instr *intr)
{
        struct qreg indirect_offset = ntq_get_src(c, intr->src[0], 0);
        uint32_t offset = nir_intrinsic_base(intr);
        struct vc5_compiler_ubo_range *range = NULL;
        unsigned i;
        for (i = 0; i < c->num_uniform_ranges; i++) {
                range = &c->ubo_ranges[i];
                if (offset >= range->src_offset &&
                    offset < range->src_offset + range->size) {
                        break;
                }
        }
        /* The driver-location-based offset always has to be within a declared
         * uniform range.
         */
        assert(range);
        if (!range->used) {
                range->used = true;
                range->dst_offset = c->next_ubo_dst_offset;
                c->next_ubo_dst_offset += range->size;
                c->num_ubo_ranges++;
        }

        offset -= range->src_offset;

        /* Adjust for where we stored the TGSI register base. */
        qir_ADD_dest(c,
                     qir_reg(QFILE_TMUA, 0),
                     qir_uniform(c, QUNIFORM_UBO_ADDR, 0),
                     qir_ADD(c,
                             indirect_offset,
                             qir_uniform_ui(c, range->dst_offset + offset)));

        c->num_texture_samples++;
        return qir_LDTMU(c);
}

static struct qreg *
ntq_init_ssa_def(struct vc5_compile *c, nir_ssa_def *def)
{
        struct qreg *qregs = ralloc_array(c->def_ht, struct qreg,
                                          def->num_components);
        _mesa_hash_table_insert(c->def_ht, def, qregs);
        return qregs;
}

/**
 * This function is responsible for getting QIR results into the associated
 * storage for a NIR instruction.
 *
 * If it's a NIR SSA def, then we just set the associated hash table entry to
 * the new result.
 *
 * If it's a NIR reg, then we need to update the existing qreg assigned to the
 * NIR destination with the incoming value.  To do that without introducing
 * new MOVs, we require that the incoming qreg either be a uniform, or be
 * SSA-defined by the previous QIR instruction in the block and rewritable by
 * this function.  That lets us sneak ahead and insert the SF flag beforehand
 * (knowing that the previous instruction doesn't depend on flags) and rewrite
 * its destination to be the NIR reg's destination
 */
static void
ntq_store_dest(struct vc5_compile *c, nir_dest *dest, int chan,
               struct qreg result)
{
        struct qinst *last_inst = NULL;
        if (!list_empty(&c->cur_block->instructions))
                last_inst = (struct qinst *)c->cur_block->instructions.prev;

        assert(result.file == QFILE_UNIF ||
               (result.file == QFILE_TEMP &&
                last_inst && last_inst == c->defs[result.index]));

        if (dest->is_ssa) {
                assert(chan < dest->ssa.num_components);

                struct qreg *qregs;
                struct hash_entry *entry =
                        _mesa_hash_table_search(c->def_ht, &dest->ssa);

                if (entry)
                        qregs = entry->data;
                else
                        qregs = ntq_init_ssa_def(c, &dest->ssa);

                qregs[chan] = result;
        } else {
                nir_register *reg = dest->reg.reg;
                assert(dest->reg.base_offset == 0);
                assert(reg->num_array_elems == 0);
                struct hash_entry *entry =
                        _mesa_hash_table_search(c->def_ht, reg);
                struct qreg *qregs = entry->data;

                /* Insert a MOV if the source wasn't an SSA def in the
                 * previous instruction.
                 */
                if (result.file == QFILE_UNIF) {
                        result = qir_MOV(c, result);
                        last_inst = c->defs[result.index];
                }

                /* We know they're both temps, so just rewrite index. */
                c->defs[last_inst->dst.index] = NULL;
                last_inst->dst.index = qregs[chan].index;

                /* If we're in control flow, then make this update of the reg
                 * conditional on the execution mask.
                 */
                if (c->execute.file != QFILE_NULL) {
                        last_inst->dst.index = qregs[chan].index;

                        /* Set the flags to the current exec mask.  To insert
                         * the flags push, we temporarily remove our SSA
                         * instruction.
                         */
                        list_del(&last_inst->link);
                        qir_PF(c, c->execute, V3D_QPU_PF_PUSHZ);
                        list_addtail(&last_inst->link,
                                     &c->cur_block->instructions);

                        last_inst->cond.alu = V3D_QPU_COND_IFA;
                        last_inst->cond_is_exec_mask = true;
                }
        }
}

static struct qreg
ntq_get_src(struct vc5_compile *c, nir_src src, int i)
{
        struct hash_entry *entry;
        if (src.is_ssa) {
                entry = _mesa_hash_table_search(c->def_ht, src.ssa);
                assert(i < src.ssa->num_components);
        } else {
                nir_register *reg = src.reg.reg;
                entry = _mesa_hash_table_search(c->def_ht, reg);
                assert(reg->num_array_elems == 0);
                assert(src.reg.base_offset == 0);
                assert(i < reg->num_components);
        }

        struct qreg *qregs = entry->data;
        return qregs[i];
}

static struct qreg
ntq_get_alu_src(struct vc5_compile *c, nir_alu_instr *instr,
                unsigned src)
{
        assert(util_is_power_of_two(instr->dest.write_mask));
        unsigned chan = ffs(instr->dest.write_mask) - 1;
        struct qreg r = ntq_get_src(c, instr->src[src].src,
                                    instr->src[src].swizzle[chan]);

        assert(!instr->src[src].abs);
        assert(!instr->src[src].negate);

        return r;
};

static inline struct qreg
qir_SAT(struct vc5_compile *c, struct qreg val)
{
        return qir_FMAX(c,
                        qir_FMIN(c, val, qir_uniform_f(c, 1.0)),
                        qir_uniform_f(c, 0.0));
}

static struct qreg
ntq_umul(struct vc5_compile *c, struct qreg src0, struct qreg src1)
{
        qir_MULTOP(c, src0, src1);
        return qir_UMUL24(c, src0, src1);
}

static void
ntq_emit_tex(struct vc5_compile *c, nir_tex_instr *instr)
{
        unsigned unit = instr->texture_index;

        struct V3D33_TEXTURE_UNIFORM_PARAMETER_0_CFG_MODE1 p0_unpacked = {
                V3D33_TEXTURE_UNIFORM_PARAMETER_0_CFG_MODE1_header,
        };

        switch (instr->sampler_dim) {
        case GLSL_SAMPLER_DIM_1D:
                if (instr->is_array)
                        p0_unpacked.lookup_type = TEXTURE_1D_ARRAY;
                else
                        p0_unpacked.lookup_type = TEXTURE_1D;
                break;
        case GLSL_SAMPLER_DIM_2D:
        case GLSL_SAMPLER_DIM_RECT:
                if (instr->is_array)
                        p0_unpacked.lookup_type = TEXTURE_2D_ARRAY;
                else
                        p0_unpacked.lookup_type = TEXTURE_2D;
                break;
        case GLSL_SAMPLER_DIM_3D:
                p0_unpacked.lookup_type = TEXTURE_3D;
                break;
        case GLSL_SAMPLER_DIM_CUBE:
                p0_unpacked.lookup_type = TEXTURE_CUBE_MAP;
                break;
        default:
                unreachable("Bad sampler type");
        }

        struct qreg coords[5];
        int next_coord = 0;
        for (unsigned i = 0; i < instr->num_srcs; i++) {
                switch (instr->src[i].src_type) {
                case nir_tex_src_coord:
                        for (int j = 0; j < instr->coord_components; j++) {
                                coords[next_coord++] =
                                        ntq_get_src(c, instr->src[i].src, j);
                        }
                        if (instr->coord_components < 2)
                                coords[next_coord++] = qir_uniform_f(c, 0.5);
                        break;
                case nir_tex_src_bias:
                        coords[next_coord++] =
                                ntq_get_src(c, instr->src[i].src, 0);

                        p0_unpacked.bias_supplied = true;
                        break;
                case nir_tex_src_lod:
                        coords[next_coord++] =
                                ntq_get_src(c, instr->src[i].src, 0);

                        p0_unpacked.disable_autolod_use_bias_only = true;
                        break;
                case nir_tex_src_comparator:
                        coords[next_coord++] =
                                ntq_get_src(c, instr->src[i].src, 0);

                        p0_unpacked.shadow = true;
                        break;
                default:
                        unreachable("unknown texture source");
                }
        }

        uint32_t p0_packed;
        V3D33_TEXTURE_UNIFORM_PARAMETER_0_CFG_MODE1_pack(NULL,
                                                         (uint8_t *)&p0_packed,
                                                         &p0_unpacked);

        /* There is no native support for GL texture rectangle coordinates, so
         * we have to rescale from ([0, width], [0, height]) to ([0, 1], [0,
         * 1]).
         */
        if (instr->sampler_dim == GLSL_SAMPLER_DIM_RECT) {
                coords[0] = qir_FMUL(c, coords[0],
                                     qir_uniform(c, QUNIFORM_TEXRECT_SCALE_X,
                                                 unit));
                coords[1] = qir_FMUL(c, coords[1],
                                     qir_uniform(c, QUNIFORM_TEXRECT_SCALE_Y,
                                                 unit));
        }

        struct qreg texture_u[] = {
                qir_uniform(c, QUNIFORM_TEXTURE_CONFIG_P0_0 + unit, p0_packed),
                qir_uniform(c, QUNIFORM_TEXTURE_CONFIG_P1, unit),
        };
        uint32_t next_texture_u = 0;

        for (int i = 0; i < next_coord; i++) {
                enum qfile file;

                if (i == next_coord - 1) {
                        if (i < 2)
                                file = QFILE_TMUUL;
                        else
                                file = QFILE_TMUL;
                } else {
                        if (i < 2)
                                file = QFILE_TMUU;
                        else
                                file = QFILE_TMU;
                }

                struct qinst *tmu = qir_MOV_dest(c, qir_reg(file, 0), coords[i]);

                if (file == QFILE_TMUU || file == QFILE_TMUUL) {
                        tmu->src[qir_get_implicit_uniform_src(tmu)] =
                                texture_u[next_texture_u++];
                }
        }

        c->num_texture_samples++;

        bool return_16 = (c->key->tex[unit].return_size == 16 ||
                          p0_unpacked.shadow);

        struct qreg return_values[4];
        for (int i = 0; i < c->key->tex[unit].return_channels; i++)
                return_values[i] = qir_LDTMU(c);

        for (int i = 0; i < nir_tex_instr_dest_size(instr); i++) {
                int swiz = c->key->tex[unit].swizzle[i];
                struct qreg chan;

                switch (swiz) {
                default:
                case PIPE_SWIZZLE_NONE:
                        unreachable("unknown swizzle");
                        break;
                case PIPE_SWIZZLE_0:
                        chan = qir_uniform_f(c, 0.0);
                        break;
                case PIPE_SWIZZLE_1:
                        chan = qir_uniform_f(c, 1.0);
                        break;
                case PIPE_SWIZZLE_X:
                case PIPE_SWIZZLE_Y:
                case PIPE_SWIZZLE_Z:
                case PIPE_SWIZZLE_W:
                        if (return_16) {
                                STATIC_ASSERT(PIPE_SWIZZLE_X == 0);
                                chan = return_values[swiz / 2];
                                if (swiz & 1)
                                        chan.pack = V3D_QPU_UNPACK_H;
                                else
                                        chan.pack = V3D_QPU_UNPACK_L;
                                chan = qir_FMOV(c, chan);
                        } else {
                                chan = qir_MOV(c, return_values[swiz]);
                        }
                        break;
                }
                ntq_store_dest(c, &instr->dest, i, chan);
        }
}

static struct qreg
ntq_fsincos(struct vc5_compile *c, struct qreg src, bool is_cos)
{
        struct qreg input = qir_FMUL(c, src, qir_uniform_f(c, 1.0f / M_PI));
        if (is_cos)
                input = qir_FADD(c, input, qir_uniform_f(c, 0.5));

        struct qreg periods = qir_FROUND(c, input);
        struct qreg sin_output = qir_SIN(c, qir_FSUB(c, input, periods));
        return qir_XOR(c, sin_output, qir_SHL(c,
                                              qir_FTOIN(c, periods),
                                              qir_uniform_ui(c, -1)));
}

static struct qreg
ntq_fsign(struct vc5_compile *c, struct qreg src)
{
        struct qreg t = qir_get_temp(c);

        qir_MOV_dest(c, t, qir_uniform_f(c, 0.0));
        qir_PF(c, qir_FMOV(c, src), V3D_QPU_PF_PUSHZ);
        qir_MOV_cond(c, V3D_QPU_COND_IFNA, t, qir_uniform_f(c, 1.0));
        qir_PF(c, qir_FMOV(c, src), V3D_QPU_PF_PUSHN);
        qir_MOV_cond(c, V3D_QPU_COND_IFA, t, qir_uniform_f(c, -1.0));
        return qir_MOV(c, t);
}

static struct qreg
ntq_isign(struct vc5_compile *c, struct qreg src)
{
        struct qreg t = qir_get_temp(c);

        qir_MOV_dest(c, t, qir_uniform_ui(c, 0));
        qir_PF(c, qir_MOV(c, src), V3D_QPU_PF_PUSHZ);
        qir_MOV_cond(c, V3D_QPU_COND_IFNA, t, qir_uniform_ui(c, 1));
        qir_PF(c, qir_MOV(c, src), V3D_QPU_PF_PUSHN);
        qir_MOV_cond(c, V3D_QPU_COND_IFA, t, qir_uniform_ui(c, -1));
        return qir_MOV(c, t);
}

static void
emit_fragcoord_input(struct vc5_compile *c, int attr)
{
        c->inputs[attr * 4 + 0] = qir_FXCD(c);
        c->inputs[attr * 4 + 1] = qir_FYCD(c);
        c->inputs[attr * 4 + 2] =
                qir_FMUL(c,
                         qir_ITOF(c, qir_FRAG_Z(c)),
                         qir_uniform_f(c, 1.0 / 0xffffff));
        c->inputs[attr * 4 + 3] = qir_RCP(c, qir_FRAG_W(c));
}

static struct qreg
emit_fragment_varying(struct vc5_compile *c, nir_variable *var,
                      uint8_t swizzle)
{
        uint32_t i = c->num_input_slots++;
        struct qreg vary = qir_reg(QFILE_VARY, i);

        if (c->num_input_slots >= c->input_slots_array_size) {
                c->input_slots_array_size =
                        MAX2(4, c->input_slots_array_size * 2);

                c->input_slots = reralloc(c, c->input_slots,
                                          struct vc5_varying_slot,
                                          c->input_slots_array_size);
        }

        /* For gl_PointCoord input or distance along a line, we'll be
         * called with no nir_variable.
         */
        if (!var) {
                c->input_slots[i].slot = 0xff;
                c->input_slots[i].swizzle = swizzle;
                return qir_VARY_ADD_C(c, qir_FMUL(c, vary, qir_FRAG_W(c)));
        }

        c->input_slots[i].slot = var ? var->data.location : 0xff;
        c->input_slots[i].swizzle = swizzle;

        switch (var->data.interpolation) {
        case INTERP_MODE_NONE:
        case INTERP_MODE_SMOOTH:
                if (var->data.centroid) {
                        return qir_VARY_ADD_C(c,
                                              qir_FMUL(c, vary,
                                                       qir_FRAG_W_CENTROID(c)));
                } else {
                        return qir_VARY_ADD_C(c,
                                              qir_FMUL(c, vary,
                                                       qir_FRAG_W(c)));
                }
        case INTERP_MODE_NOPERSPECTIVE:
                return qir_VARY_ADD_C(c, qir_MOV(c, vary));
        case INTERP_MODE_FLAT:
                BITSET_SET(c->flat_shade_flags, i);
                return qir_VARY_MOV_C(c, vary);
        default:
                unreachable("Bad interp mode");
        }
}

static void
emit_fragment_input(struct vc5_compile *c, int attr, nir_variable *var)
{
        for (int i = 0; i < 4; i++) {
                c->inputs[attr * 4 + i] =
                        emit_fragment_varying(c, var, i);
                c->num_inputs++;
        }
}

static void
add_output(struct vc5_compile *c,
           uint32_t decl_offset,
           uint8_t slot,
           uint8_t swizzle)
{
        uint32_t old_array_size = c->outputs_array_size;
        resize_qreg_array(c, &c->outputs, &c->outputs_array_size,
                          decl_offset + 1);

        if (old_array_size != c->outputs_array_size) {
                c->output_slots = reralloc(c,
                                           c->output_slots,
                                           struct vc5_varying_slot,
                                           c->outputs_array_size);
        }

        c->output_slots[decl_offset].slot = slot;
        c->output_slots[decl_offset].swizzle = swizzle;
}

static void
declare_uniform_range(struct vc5_compile *c, uint32_t start, uint32_t size)
{
        unsigned array_id = c->num_uniform_ranges++;
        if (array_id >= c->ubo_ranges_array_size) {
                c->ubo_ranges_array_size = MAX2(c->ubo_ranges_array_size * 2,
                                                array_id + 1);
                c->ubo_ranges = reralloc(c, c->ubo_ranges,
                                         struct vc5_compiler_ubo_range,
                                         c->ubo_ranges_array_size);
        }

        c->ubo_ranges[array_id].dst_offset = 0;
        c->ubo_ranges[array_id].src_offset = start;
        c->ubo_ranges[array_id].size = size;
        c->ubo_ranges[array_id].used = false;
}

/**
 * If compare_instr is a valid comparison instruction, emits the
 * compare_instr's comparison and returns the sel_instr's return value based
 * on the compare_instr's result.
 */
static bool
ntq_emit_comparison(struct vc5_compile *c, struct qreg *dest,
                    nir_alu_instr *compare_instr,
                    nir_alu_instr *sel_instr)
{
        enum v3d_qpu_cond cond;
        enum v3d_qpu_pf pf;

        switch (compare_instr->op) {
        case nir_op_feq:
        case nir_op_ieq:
        case nir_op_seq:
                pf = V3D_QPU_PF_PUSHZ;
                cond = V3D_QPU_COND_IFA;
                break;
        case nir_op_fne:
        case nir_op_ine:
        case nir_op_sne:
                pf = V3D_QPU_PF_PUSHZ;
                cond = V3D_QPU_COND_IFNA;
                break;
        case nir_op_fge:
        case nir_op_ige:
        case nir_op_uge:
        case nir_op_sge:
                pf = V3D_QPU_PF_PUSHN;
                cond = V3D_QPU_COND_IFNA;
                break;
        case nir_op_flt:
        case nir_op_ilt:
        case nir_op_slt:
                pf = V3D_QPU_PF_PUSHN;
                cond = V3D_QPU_COND_IFA;
                break;

        case nir_op_ult:
                pf = V3D_QPU_PF_PUSHC;
                cond = V3D_QPU_COND_IFNA;
                break;

        default:
                return false;
        }

        struct qreg src0 = ntq_get_alu_src(c, compare_instr, 0);
        struct qreg src1 = ntq_get_alu_src(c, compare_instr, 1);

        unsigned unsized_type =
                nir_alu_type_get_base_type(nir_op_infos[compare_instr->op].input_types[0]);
        if (unsized_type == nir_type_float)
                qir_PF(c, qir_FSUB(c, src0, src1), pf);
        else
                qir_PF(c, qir_SUB(c, src0, src1), pf);

        switch (sel_instr->op) {
        case nir_op_seq:
        case nir_op_sne:
        case nir_op_sge:
        case nir_op_slt:
                *dest = qir_SEL(c, cond,
                                qir_uniform_f(c, 1.0), qir_uniform_f(c, 0.0));
                break;

        case nir_op_bcsel:
                *dest = qir_SEL(c, cond,
                                ntq_get_alu_src(c, sel_instr, 1),
                                ntq_get_alu_src(c, sel_instr, 2));
                break;

        default:
                *dest = qir_SEL(c, cond,
                                qir_uniform_ui(c, ~0), qir_uniform_ui(c, 0));
                break;
        }

        /* Make the temporary for nir_store_dest(). */
        *dest = qir_MOV(c, *dest);

        return true;
}

/**
 * Attempts to fold a comparison generating a boolean result into the
 * condition code for selecting between two values, instead of comparing the
 * boolean result against 0 to generate the condition code.
 */
static struct qreg ntq_emit_bcsel(struct vc5_compile *c, nir_alu_instr *instr,
                                  struct qreg *src)
{
        if (!instr->src[0].src.is_ssa)
                goto out;
        if (instr->src[0].src.ssa->parent_instr->type != nir_instr_type_alu)
                goto out;
        nir_alu_instr *compare =
                nir_instr_as_alu(instr->src[0].src.ssa->parent_instr);
        if (!compare)
                goto out;

        struct qreg dest;
        if (ntq_emit_comparison(c, &dest, compare, instr))
                return dest;

out:
        qir_PF(c, src[0], V3D_QPU_PF_PUSHZ);
        return qir_MOV(c, qir_SEL(c, V3D_QPU_COND_IFNA, src[1], src[2]));
}


static void
ntq_emit_alu(struct vc5_compile *c, nir_alu_instr *instr)
{
        /* This should always be lowered to ALU operations for VC5. */
        assert(!instr->dest.saturate);

        /* Vectors are special in that they have non-scalarized writemasks,
         * and just take the first swizzle channel for each argument in order
         * into each writemask channel.
         */
        if (instr->op == nir_op_vec2 ||
            instr->op == nir_op_vec3 ||
            instr->op == nir_op_vec4) {
                struct qreg srcs[4];
                for (int i = 0; i < nir_op_infos[instr->op].num_inputs; i++)
                        srcs[i] = ntq_get_src(c, instr->src[i].src,
                                              instr->src[i].swizzle[0]);
                for (int i = 0; i < nir_op_infos[instr->op].num_inputs; i++)
                        ntq_store_dest(c, &instr->dest.dest, i,
                                       qir_MOV(c, srcs[i]));
                return;
        }

        /* General case: We can just grab the one used channel per src. */
        struct qreg src[nir_op_infos[instr->op].num_inputs];
        for (int i = 0; i < nir_op_infos[instr->op].num_inputs; i++) {
                src[i] = ntq_get_alu_src(c, instr, i);
        }

        struct qreg result;

        switch (instr->op) {
        case nir_op_fmov:
        case nir_op_imov:
                result = qir_MOV(c, src[0]);
                break;
        case nir_op_fmul:
                result = qir_FMUL(c, src[0], src[1]);
                break;
        case nir_op_fadd:
                result = qir_FADD(c, src[0], src[1]);
                break;
        case nir_op_fsub:
                result = qir_FSUB(c, src[0], src[1]);
                break;
        case nir_op_fmin:
                result = qir_FMIN(c, src[0], src[1]);
                break;
        case nir_op_fmax:
                result = qir_FMAX(c, src[0], src[1]);
                break;

        case nir_op_f2i32:
                result = qir_FTOIZ(c, src[0]);
                break;
        case nir_op_f2u32:
                result = qir_FTOUZ(c, src[0]);
                break;
        case nir_op_i2f32:
                result = qir_ITOF(c, src[0]);
                break;
        case nir_op_u2f32:
                result = qir_UTOF(c, src[0]);
                break;
        case nir_op_b2f:
                result = qir_AND(c, src[0], qir_uniform_f(c, 1.0));
                break;
        case nir_op_b2i:
                result = qir_AND(c, src[0], qir_uniform_ui(c, 1));
                break;
        case nir_op_i2b:
        case nir_op_f2b:
                qir_PF(c, src[0], V3D_QPU_PF_PUSHZ);
                result = qir_MOV(c, qir_SEL(c, V3D_QPU_COND_IFNA,
                                            qir_uniform_ui(c, ~0),
                                            qir_uniform_ui(c, 0)));
                break;

        case nir_op_iadd:
                result = qir_ADD(c, src[0], src[1]);
                break;
        case nir_op_ushr:
                result = qir_SHR(c, src[0], src[1]);
                break;
        case nir_op_isub:
                result = qir_SUB(c, src[0], src[1]);
                break;
        case nir_op_ishr:
                result = qir_ASR(c, src[0], src[1]);
                break;
        case nir_op_ishl:
                result = qir_SHL(c, src[0], src[1]);
                break;
        case nir_op_imin:
                result = qir_MIN(c, src[0], src[1]);
                break;
        case nir_op_umin:
                result = qir_UMIN(c, src[0], src[1]);
                break;
        case nir_op_imax:
                result = qir_MAX(c, src[0], src[1]);
                break;
        case nir_op_umax:
                result = qir_UMAX(c, src[0], src[1]);
                break;
        case nir_op_iand:
                result = qir_AND(c, src[0], src[1]);
                break;
        case nir_op_ior:
                result = qir_OR(c, src[0], src[1]);
                break;
        case nir_op_ixor:
                result = qir_XOR(c, src[0], src[1]);
                break;
        case nir_op_inot:
                result = qir_NOT(c, src[0]);
                break;

        case nir_op_imul:
                result = ntq_umul(c, src[0], src[1]);
                break;

        case nir_op_seq:
        case nir_op_sne:
        case nir_op_sge:
        case nir_op_slt:
        case nir_op_feq:
        case nir_op_fne:
        case nir_op_fge:
        case nir_op_flt:
        case nir_op_ieq:
        case nir_op_ine:
        case nir_op_ige:
        case nir_op_uge:
        case nir_op_ilt:
        case nir_op_ult:
                if (!ntq_emit_comparison(c, &result, instr, instr)) {
                        fprintf(stderr, "Bad comparison instruction\n");
                }
                break;

        case nir_op_bcsel:
                result = ntq_emit_bcsel(c, instr, src);
                break;
        case nir_op_fcsel:
                qir_PF(c, src[0], V3D_QPU_PF_PUSHZ);
                result = qir_MOV(c, qir_SEL(c, V3D_QPU_COND_IFNA,
                                            src[1], src[2]));
                break;

        case nir_op_frcp:
                result = qir_RCP(c, src[0]);
                break;
        case nir_op_frsq:
                result = qir_RSQ(c, src[0]);
                break;
        case nir_op_fexp2:
                result = qir_EXP2(c, src[0]);
                break;
        case nir_op_flog2:
                result = qir_LOG2(c, src[0]);
                break;

        case nir_op_fceil:
                result = qir_FCEIL(c, src[0]);
                break;
        case nir_op_ffloor:
                result = qir_FFLOOR(c, src[0]);
                break;
        case nir_op_fround_even:
                result = qir_FROUND(c, src[0]);
                break;
        case nir_op_ftrunc:
                result = qir_FTRUNC(c, src[0]);
                break;
        case nir_op_ffract:
                result = qir_FSUB(c, src[0], qir_FFLOOR(c, src[0]));
                break;

        case nir_op_fsin:
                result = ntq_fsincos(c, src[0], false);
                break;
        case nir_op_fcos:
                result = ntq_fsincos(c, src[0], true);
                break;

        case nir_op_fsign:
                result = ntq_fsign(c, src[0]);
                break;
        case nir_op_isign:
                result = ntq_isign(c, src[0]);
                break;

        case nir_op_fabs: {
                src[0].pack = V3D_QPU_UNPACK_ABS;
                result = qir_FMOV(c, src[0]);
                break;
        }

        case nir_op_iabs:
                result = qir_MAX(c, src[0],
                                qir_SUB(c, qir_uniform_ui(c, 0), src[0]));
                break;

        case nir_op_fddx:
        case nir_op_fddx_coarse:
        case nir_op_fddx_fine:
                result = qir_FDX(c, src[0]);
                break;

        case nir_op_fddy:
        case nir_op_fddy_coarse:
        case nir_op_fddy_fine:
                result = qir_FDY(c, src[0]);
                break;

        default:
                fprintf(stderr, "unknown NIR ALU inst: ");
                nir_print_instr(&instr->instr, stderr);
                fprintf(stderr, "\n");
                abort();
        }

        /* We have a scalar result, so the instruction should only have a
         * single channel written to.
         */
        assert(util_is_power_of_two(instr->dest.write_mask));
        ntq_store_dest(c, &instr->dest.dest,
                       ffs(instr->dest.write_mask) - 1, result);
}

static void
emit_frag_end(struct vc5_compile *c)
{
        uint32_t discard_cond = V3D_QPU_COND_NONE;
        if (c->s->info.fs.uses_discard) {
                qir_PF(c, qir_MOV(c, c->discard), V3D_QPU_PF_PUSHZ);
                discard_cond = V3D_QPU_COND_IFA;
        }

        /* XXX
        if (c->output_sample_mask_index != -1) {
                qir_MS_MASK(c, c->outputs[c->output_sample_mask_index]);
        }
        */

        if (c->output_position_index != -1) {
                struct qinst *inst = qir_MOV_dest(c,
                                                  qir_reg(QFILE_TLBU, 0),
                                                  c->outputs[c->output_position_index]);

                inst->src[qir_get_implicit_uniform_src(inst)] =
                        qir_uniform_ui(c,
                                       (1 << 2) | /* per pixel */
                                       (2 << 6) /* type */ |
                                       0xffffff00);
        }

        /* XXX: Performance improvement: Merge Z write and color writes TLB
         * uniform setup
         */

        if (c->output_color_var) {
                nir_variable *var = c->output_color_var;
                struct qreg *color = &c->outputs[var->data.driver_location * 4];
                int num_components = glsl_get_vector_elements(var->type);
                uint32_t conf = ~0;
                struct qinst *inst;

                assert(num_components != 0);
                switch (glsl_get_base_type(var->type)) {
                case GLSL_TYPE_UINT:
                case GLSL_TYPE_INT:
                        conf = ((1 << 2) | /* per pixel */
                                ((7 - 0) << 3) | /* rt */
                                (1 << 6) /* type */ |
                                (num_components - 1) |
                                0xffffff00);


                        inst = qir_MOV_dest(c, qir_reg(QFILE_TLBU, 0), color[0]);
                        inst->cond.alu = discard_cond;
                        inst->src[qir_get_implicit_uniform_src(inst)] =
                                qir_uniform_ui(c, conf);

                        for (int i = 1; i < num_components; i++) {
                                qir_MOV_dest(c, qir_reg(QFILE_TLB, 0),
                                             color[i])->cond.alu = discard_cond;;
                        }
                        break;

                default: {
                        struct qreg r = color[0];
                        struct qreg g = color[1];
                        struct qreg b = color[2];
                        struct qreg a = color[3];

                        if (c->fs_key->swap_color_rb)  {
                                r = color[2];
                                b = color[0];
                        }

                        qir_VFPACK_dest(c, qir_reg(QFILE_TLB, 0), r, g)->cond.alu = discard_cond;
                        qir_VFPACK_dest(c, qir_reg(QFILE_TLB, 0), b, a)->cond.alu = discard_cond;
                        break;
                }
                }
        }
}

static void
emit_scaled_viewport_write(struct vc5_compile *c, struct qreg rcp_w)
{
        for (int i = 0; i < 2; i++) {
                struct qreg coord = c->outputs[c->output_position_index + i];
                coord = qir_FMUL(c, coord,
                                 qir_uniform(c, QUNIFORM_VIEWPORT_X_SCALE + i,
                                             0));
                coord = qir_FMUL(c, coord, rcp_w);
                qir_FTOIN_dest(c, qir_reg(QFILE_VPM, 0), coord);
        }

}

static void
emit_zs_write(struct vc5_compile *c, struct qreg rcp_w)
{
        struct qreg zscale = qir_uniform(c, QUNIFORM_VIEWPORT_Z_SCALE, 0);
        struct qreg zoffset = qir_uniform(c, QUNIFORM_VIEWPORT_Z_OFFSET, 0);

        qir_FADD_dest(c, qir_reg(QFILE_VPM, 0),
                      qir_FMUL(c, qir_FMUL(c,
                                           c->outputs[c->output_position_index + 2],
                                           zscale),
                               rcp_w),
                      zoffset);
}

static void
emit_rcp_wc_write(struct vc5_compile *c, struct qreg rcp_w)
{
        qir_VPM_WRITE(c, rcp_w);
}

static void
emit_point_size_write(struct vc5_compile *c)
{
        struct qreg point_size;

        if (c->output_point_size_index != -1)
                point_size = c->outputs[c->output_point_size_index];
        else
                point_size = qir_uniform_f(c, 1.0);

        /* Workaround: HW-2726 PTB does not handle zero-size points (BCM2835,
         * BCM21553).
         */
        point_size = qir_FMAX(c, point_size, qir_uniform_f(c, .125));

        qir_VPM_WRITE(c, point_size);
}

static void
emit_vpm_write_setup(struct vc5_compile *c)
{
        uint32_t packed;
        struct V3D33_VPM_GENERIC_BLOCK_WRITE_SETUP unpacked = {
                V3D33_VPM_GENERIC_BLOCK_WRITE_SETUP_header,

                .horiz = true,
                .laned = false,
                .segs = true,
                .stride = 1,
                .size = VPM_SETUP_SIZE_32_BIT,
                .addr = 0,
        };

        V3D33_VPM_GENERIC_BLOCK_WRITE_SETUP_pack(NULL,
                                                (uint8_t *)&packed,
                                                &unpacked);
        qir_VPMSETUP_WRITE(c, qir_uniform_ui(c, packed));
}

static void
emit_vert_end(struct vc5_compile *c,
              struct vc5_varying_slot *fs_inputs,
              uint32_t num_fs_inputs)
{
        struct qreg rcp_w = qir_RCP(c, c->outputs[c->output_position_index + 3]);

        emit_vpm_write_setup(c);

        emit_scaled_viewport_write(c, rcp_w);
        emit_zs_write(c, rcp_w);
        emit_rcp_wc_write(c, rcp_w);
        if (c->vs_key->per_vertex_point_size)
                emit_point_size_write(c);

        for (int i = 0; i < num_fs_inputs; i++) {
                struct vc5_varying_slot *input = &fs_inputs[i];
                int j;

                for (j = 0; j < c->num_outputs; j++) {
                        struct vc5_varying_slot *output =
                                &c->output_slots[j];

                        if (input->slot == output->slot &&
                            input->swizzle == output->swizzle) {
                                qir_VPM_WRITE(c, c->outputs[j]);
                                break;
                        }
                }
                /* Emit padding if we didn't find a declared VS output for
                 * this FS input.
                 */
                if (j == c->num_outputs)
                        qir_VPM_WRITE(c, qir_uniform_f(c, 0.0));
        }
}

static void
emit_coord_end(struct vc5_compile *c)
{
        struct qreg rcp_w = qir_RCP(c, c->outputs[c->output_position_index + 3]);

        emit_vpm_write_setup(c);

        for (int i = 0; i < 4; i++)
                qir_VPM_WRITE(c, c->outputs[c->output_position_index + i]);

        emit_scaled_viewport_write(c, rcp_w);
        if (c->vs_key->per_vertex_point_size) {
                emit_point_size_write(c);
                /* emit_rcp_wc_write(c, rcp_w); */
        }
/* XXX: Z-only rendering
        if (0)
                emit_zs_write(c, rcp_w);
*/
}

void
vc5_optimize_nir(struct nir_shader *s)
{
        bool progress;

        do {
                progress = false;

                NIR_PASS_V(s, nir_lower_vars_to_ssa);
                NIR_PASS(progress, s, nir_lower_alu_to_scalar);
                NIR_PASS(progress, s, nir_lower_phis_to_scalar);
                NIR_PASS(progress, s, nir_copy_prop);
                NIR_PASS(progress, s, nir_opt_remove_phis);
                NIR_PASS(progress, s, nir_opt_dce);
                NIR_PASS(progress, s, nir_opt_dead_cf);
                NIR_PASS(progress, s, nir_opt_cse);
                NIR_PASS(progress, s, nir_opt_peephole_select, 8);
                NIR_PASS(progress, s, nir_opt_algebraic);
                NIR_PASS(progress, s, nir_opt_constant_folding);
                NIR_PASS(progress, s, nir_opt_undef);
        } while (progress);
}

static int
driver_location_compare(const void *in_a, const void *in_b)
{
        const nir_variable *const *a = in_a;
        const nir_variable *const *b = in_b;

        return (*a)->data.driver_location - (*b)->data.driver_location;
}

static struct qreg
ntq_emit_vpm_read(struct vc5_compile *c,
                  uint32_t *num_components_queued,
                  uint32_t *remaining,
                  uint32_t vpm_index)
{
        struct qreg vpm = qir_reg(QFILE_VPM, vpm_index);

        if (*num_components_queued != 0) {
                (*num_components_queued)--;
                c->num_inputs++;
                return qir_MOV(c, vpm);
        }

        uint32_t num_components = MIN2(*remaining, 32);

        struct V3D33_VPM_GENERIC_BLOCK_READ_SETUP unpacked = {
                V3D33_VPM_GENERIC_BLOCK_READ_SETUP_header,

                .horiz = true,
                .laned = false,
                /* If the field is 0, that means a read count of 32. */
                .num = num_components & 31,
                .segs = true,
                .stride = 1,
                .size = VPM_SETUP_SIZE_32_BIT,
                .addr = c->num_inputs,
        };

        uint32_t packed;
        V3D33_VPM_GENERIC_BLOCK_READ_SETUP_pack(NULL,
                                                (uint8_t *)&packed,
                                                &unpacked);
        qir_VPMSETUP_READ(c, qir_uniform_ui(c, packed));

        *num_components_queued = num_components - 1;
        *remaining -= num_components;
        c->num_inputs++;

        return qir_MOV(c, vpm);
}

static void
ntq_setup_inputs(struct vc5_compile *c)
{
        unsigned num_entries = 0;
        unsigned num_components = 0;
        nir_foreach_variable(var, &c->s->inputs) {
                num_entries++;
                num_components += glsl_get_components(var->type);
        }

        nir_variable *vars[num_entries];

        unsigned i = 0;
        nir_foreach_variable(var, &c->s->inputs)
                vars[i++] = var;

        /* Sort the variables so that we emit the input setup in
         * driver_location order.  This is required for VPM reads, whose data
         * is fetched into the VPM in driver_location (TGSI register index)
         * order.
         */
        qsort(&vars, num_entries, sizeof(*vars), driver_location_compare);

        uint32_t vpm_components_queued = 0;
        if (c->s->stage == MESA_SHADER_VERTEX) {
                bool uses_iid = c->s->info.system_values_read &
                        (1ull << SYSTEM_VALUE_INSTANCE_ID);
                bool uses_vid = c->s->info.system_values_read &
                        (1ull << SYSTEM_VALUE_VERTEX_ID);

                num_components += uses_iid;
                num_components += uses_vid;

                if (uses_iid) {
                        c->iid = ntq_emit_vpm_read(c, &vpm_components_queued,
                                                   &num_components, ~0);
                }

                if (uses_vid) {
                        c->vid = ntq_emit_vpm_read(c, &vpm_components_queued,
                                                   &num_components, ~0);
                }
        }

        for (unsigned i = 0; i < num_entries; i++) {
                nir_variable *var = vars[i];
                unsigned array_len = MAX2(glsl_get_length(var->type), 1);
                unsigned loc = var->data.driver_location;

                assert(array_len == 1);
                (void)array_len;
                resize_qreg_array(c, &c->inputs, &c->inputs_array_size,
                                  (loc + 1) * 4);

                if (c->s->stage == MESA_SHADER_FRAGMENT) {
                        if (var->data.location == VARYING_SLOT_POS) {
                                emit_fragcoord_input(c, loc);
                        } else if (var->data.location == VARYING_SLOT_PNTC ||
                                   (var->data.location >= VARYING_SLOT_VAR0 &&
                                    (c->fs_key->point_sprite_mask &
                                     (1 << (var->data.location -
                                            VARYING_SLOT_VAR0))))) {
                                c->inputs[loc * 4 + 0] = c->point_x;
                                c->inputs[loc * 4 + 1] = c->point_y;
                        } else {
                                emit_fragment_input(c, loc, var);
                        }
                } else {
                        int var_components = glsl_get_components(var->type);

                        for (int i = 0; i < var_components; i++) {
                                c->inputs[loc * 4 + i] =
                                        ntq_emit_vpm_read(c,
                                                          &vpm_components_queued,
                                                          &num_components,
                                                          loc * 4 + i);

                        }
                        c->vattr_sizes[loc] = var_components;
                }
        }

        if (c->s->stage == MESA_SHADER_VERTEX) {
                assert(vpm_components_queued == 0);
                assert(num_components == 0);
        }
}

static void
ntq_setup_outputs(struct vc5_compile *c)
{
        nir_foreach_variable(var, &c->s->outputs) {
                unsigned array_len = MAX2(glsl_get_length(var->type), 1);
                unsigned loc = var->data.driver_location * 4;

                assert(array_len == 1);
                (void)array_len;

                for (int i = 0; i < 4; i++)
                        add_output(c, loc + i, var->data.location, i);

                if (c->s->stage == MESA_SHADER_FRAGMENT) {
                        switch (var->data.location) {
                        case FRAG_RESULT_COLOR:
                        case FRAG_RESULT_DATA0:
                                c->output_color_var = var;
                                break;
                        case FRAG_RESULT_DEPTH:
                                c->output_position_index = loc;
                                break;
                        case FRAG_RESULT_SAMPLE_MASK:
                                c->output_sample_mask_index = loc;
                                break;
                        }
                } else {
                        switch (var->data.location) {
                        case VARYING_SLOT_POS:
                                c->output_position_index = loc;
                                break;
                        case VARYING_SLOT_PSIZ:
                                c->output_point_size_index = loc;
                                break;
                        }
                }
        }
}

static void
ntq_setup_uniforms(struct vc5_compile *c)
{
        nir_foreach_variable(var, &c->s->uniforms) {
                uint32_t vec4_count = st_glsl_type_size(var->type);
                unsigned vec4_size = 4 * sizeof(float);

                declare_uniform_range(c, var->data.driver_location * vec4_size,
                                      vec4_count * vec4_size);

        }
}

/**
 * Sets up the mapping from nir_register to struct qreg *.
 *
 * Each nir_register gets a struct qreg per 32-bit component being stored.
 */
static void
ntq_setup_registers(struct vc5_compile *c, struct exec_list *list)
{
        foreach_list_typed(nir_register, nir_reg, node, list) {
                unsigned array_len = MAX2(nir_reg->num_array_elems, 1);
                struct qreg *qregs = ralloc_array(c->def_ht, struct qreg,
                                                  array_len *
                                                  nir_reg->num_components);

                _mesa_hash_table_insert(c->def_ht, nir_reg, qregs);

                for (int i = 0; i < array_len * nir_reg->num_components; i++)
                        qregs[i] = qir_get_temp(c);
        }
}

static void
ntq_emit_load_const(struct vc5_compile *c, nir_load_const_instr *instr)
{
        struct qreg *qregs = ntq_init_ssa_def(c, &instr->def);
        for (int i = 0; i < instr->def.num_components; i++)
                qregs[i] = qir_uniform_ui(c, instr->value.u32[i]);

        _mesa_hash_table_insert(c->def_ht, &instr->def, qregs);
}

static void
ntq_emit_ssa_undef(struct vc5_compile *c, nir_ssa_undef_instr *instr)
{
        struct qreg *qregs = ntq_init_ssa_def(c, &instr->def);

        /* QIR needs there to be *some* value, so pick 0 (same as for
         * ntq_setup_registers().
         */
        for (int i = 0; i < instr->def.num_components; i++)
                qregs[i] = qir_uniform_ui(c, 0);
}

static void
ntq_emit_intrinsic(struct vc5_compile *c, nir_intrinsic_instr *instr)
{
        nir_const_value *const_offset;
        unsigned offset;

        switch (instr->intrinsic) {
        case nir_intrinsic_load_uniform:
                assert(instr->num_components == 1);
                const_offset = nir_src_as_const_value(instr->src[0]);
                if (const_offset) {
                        offset = nir_intrinsic_base(instr) + const_offset->u32[0];
                        assert(offset % 4 == 0);
                        /* We need dwords */
                        offset = offset / 4;
                        ntq_store_dest(c, &instr->dest, 0,
                                       qir_uniform(c, QUNIFORM_UNIFORM,
                                                   offset));
                } else {
                        ntq_store_dest(c, &instr->dest, 0,
                                       indirect_uniform_load(c, instr));
                }
                break;

        case nir_intrinsic_load_ubo:
                for (int i = 0; i < instr->num_components; i++) {
                        int ubo = nir_src_as_const_value(instr->src[0])->u32[0];

                        /* Adjust for where we stored the TGSI register base. */
                        qir_ADD_dest(c,
                                     qir_reg(QFILE_TMUA, 0),
                                     qir_uniform(c, QUNIFORM_UBO_ADDR, 1 + ubo),
                                     qir_ADD(c,
                                             ntq_get_src(c, instr->src[1], 0),
                                             qir_uniform_ui(c, i * 4)));

                        c->num_texture_samples++;
                        ntq_store_dest(c, &instr->dest, i, qir_LDTMU(c));
                }
                break;

                const_offset = nir_src_as_const_value(instr->src[0]);
                if (const_offset) {
                        offset = nir_intrinsic_base(instr) + const_offset->u32[0];
                        assert(offset % 4 == 0);
                        /* We need dwords */
                        offset = offset / 4;
                        ntq_store_dest(c, &instr->dest, 0,
                                       qir_uniform(c, QUNIFORM_UNIFORM,
                                                   offset));
                } else {
                        ntq_store_dest(c, &instr->dest, 0,
                                       indirect_uniform_load(c, instr));
                }
                break;

        case nir_intrinsic_load_user_clip_plane:
                for (int i = 0; i < instr->num_components; i++) {
                        ntq_store_dest(c, &instr->dest, i,
                                       qir_uniform(c, QUNIFORM_USER_CLIP_PLANE,
                                                   nir_intrinsic_ucp_id(instr) *
                                                   4 + i));
                }
                break;

        case nir_intrinsic_load_alpha_ref_float:
                ntq_store_dest(c, &instr->dest, 0,
                               qir_uniform(c, QUNIFORM_ALPHA_REF, 0));
                break;

        case nir_intrinsic_load_sample_mask_in:
                ntq_store_dest(c, &instr->dest, 0,
                               qir_uniform(c, QUNIFORM_SAMPLE_MASK, 0));
                break;

        case nir_intrinsic_load_front_face:
                /* The register contains 0 (front) or 1 (back), and we need to
                 * turn it into a NIR bool where true means front.
                 */
                ntq_store_dest(c, &instr->dest, 0,
                               qir_ADD(c,
                                       qir_uniform_ui(c, -1),
                                       qir_REVF(c)));
                break;

        case nir_intrinsic_load_instance_id:
                ntq_store_dest(c, &instr->dest, 0, qir_MOV(c, c->iid));
                break;

        case nir_intrinsic_load_vertex_id:
                ntq_store_dest(c, &instr->dest, 0, qir_MOV(c, c->vid));
                break;

        case nir_intrinsic_load_input:
                const_offset = nir_src_as_const_value(instr->src[0]);
                assert(const_offset && "vc5 doesn't support indirect inputs");
                for (int i = 0; i < instr->num_components; i++) {
                        offset = nir_intrinsic_base(instr) + const_offset->u32[0];
                        int comp = nir_intrinsic_component(instr) + i;
                        ntq_store_dest(c, &instr->dest, i,
                                       qir_MOV(c, c->inputs[offset * 4 + comp]));
                }
                break;

        case nir_intrinsic_store_output:
                const_offset = nir_src_as_const_value(instr->src[1]);
                assert(const_offset && "vc5 doesn't support indirect outputs");
                offset = ((nir_intrinsic_base(instr) +
                           const_offset->u32[0]) * 4 +
                          nir_intrinsic_component(instr));

                for (int i = 0; i < instr->num_components; i++) {
                        c->outputs[offset + i] =
                                qir_MOV(c, ntq_get_src(c, instr->src[0], i));
                }
                c->num_outputs = MAX2(c->num_outputs,
                                      offset + instr->num_components);
                break;

        case nir_intrinsic_discard:
                if (c->execute.file != QFILE_NULL) {
                        qir_PF(c, c->execute, V3D_QPU_PF_PUSHZ);
                        qir_MOV_cond(c, V3D_QPU_COND_IFA, c->discard,
                                     qir_uniform_ui(c, ~0));
                } else {
                        qir_MOV_dest(c, c->discard, qir_uniform_ui(c, ~0));
                }
                break;

        case nir_intrinsic_discard_if: {
                /* true (~0) if we're discarding */
                struct qreg cond = ntq_get_src(c, instr->src[0], 0);

                if (c->execute.file != QFILE_NULL) {
                        /* execute == 0 means the channel is active.  Invert
                         * the condition so that we can use zero as "executing
                         * and discarding."
                         */
                        qir_PF(c, qir_AND(c, c->execute, qir_NOT(c, cond)),
                               V3D_QPU_PF_PUSHZ);
                        qir_MOV_cond(c, V3D_QPU_COND_IFA, c->discard, cond);
                } else {
                        qir_OR_dest(c, c->discard, c->discard, cond);
                }

                break;
        }

        default:
                fprintf(stderr, "Unknown intrinsic: ");
                nir_print_instr(&instr->instr, stderr);
                fprintf(stderr, "\n");
                break;
        }
}

/* Clears (activates) the execute flags for any channels whose jump target
 * matches this block.
 */
static void
ntq_activate_execute_for_block(struct vc5_compile *c)
{
        qir_PF(c, qir_SUB(c, c->execute, qir_uniform_ui(c, c->cur_block->index)),
               V3D_QPU_PF_PUSHZ);

        qir_MOV_cond(c, V3D_QPU_COND_IFA, c->execute, qir_uniform_ui(c, 0));
}

static void
ntq_emit_if(struct vc5_compile *c, nir_if *if_stmt)
{
        nir_block *nir_else_block = nir_if_first_else_block(if_stmt);
        bool empty_else_block =
                (nir_else_block == nir_if_last_else_block(if_stmt) &&
                 exec_list_is_empty(&nir_else_block->instr_list));

        struct qblock *then_block = qir_new_block(c);
        struct qblock *after_block = qir_new_block(c);
        struct qblock *else_block;
        if (empty_else_block)
                else_block = after_block;
        else
                else_block = qir_new_block(c);

        bool was_top_level = false;
        if (c->execute.file == QFILE_NULL) {
                c->execute = qir_MOV(c, qir_uniform_ui(c, 0));
                was_top_level = true;
        }

        /* Set A for executing (execute == 0) and jumping (if->condition ==
         * 0) channels, and then update execute flags for those to point to
         * the ELSE block.
         */
        qir_PF(c, qir_OR(c,
                         c->execute,
                         ntq_get_src(c, if_stmt->condition, 0)),
                V3D_QPU_PF_PUSHZ);
        qir_MOV_cond(c, V3D_QPU_COND_IFA,
                     c->execute,
                     qir_uniform_ui(c, else_block->index));

        /* Jump to ELSE if nothing is active for THEN, otherwise fall
         * through.
         */
        qir_PF(c, c->execute, V3D_QPU_PF_PUSHZ);
        qir_BRANCH(c, V3D_QPU_BRANCH_COND_ALLNA);
        qir_link_blocks(c->cur_block, else_block);
        qir_link_blocks(c->cur_block, then_block);

        /* Process the THEN block. */
        qir_set_emit_block(c, then_block);
        ntq_emit_cf_list(c, &if_stmt->then_list);

        if (!empty_else_block) {
                /* Handle the end of the THEN block.  First, all currently
                 * active channels update their execute flags to point to
                 * ENDIF
                 */
                qir_PF(c, c->execute, V3D_QPU_PF_PUSHZ);
                qir_MOV_cond(c, V3D_QPU_COND_IFA, c->execute,
                             qir_uniform_ui(c, after_block->index));

                /* If everything points at ENDIF, then jump there immediately. */
                qir_PF(c, qir_SUB(c, c->execute,
                                  qir_uniform_ui(c, after_block->index)),
                       V3D_QPU_PF_PUSHZ);
                qir_BRANCH(c, V3D_QPU_BRANCH_COND_ALLA);
                qir_link_blocks(c->cur_block, after_block);
                qir_link_blocks(c->cur_block, else_block);

                qir_set_emit_block(c, else_block);
                ntq_activate_execute_for_block(c);
                ntq_emit_cf_list(c, &if_stmt->else_list);
        }

        qir_link_blocks(c->cur_block, after_block);

        qir_set_emit_block(c, after_block);
        if (was_top_level)
                c->execute = c->undef;
        else
                ntq_activate_execute_for_block(c);
}

static void
ntq_emit_jump(struct vc5_compile *c, nir_jump_instr *jump)
{
        switch (jump->type) {
        case nir_jump_break:
                qir_PF(c, c->execute, V3D_QPU_PF_PUSHZ);
                qir_MOV_cond(c, V3D_QPU_COND_IFA, c->execute,
                             qir_uniform_ui(c, c->loop_break_block->index));
                break;

        case nir_jump_continue:
                qir_PF(c, c->execute, V3D_QPU_PF_PUSHZ);
                qir_MOV_cond(c, V3D_QPU_COND_IFA, c->execute,
                             qir_uniform_ui(c, c->loop_cont_block->index));
                break;

        case nir_jump_return:
                unreachable("All returns shouold be lowered\n");
        }
}

static void
ntq_emit_instr(struct vc5_compile *c, nir_instr *instr)
{
        switch (instr->type) {
        case nir_instr_type_alu:
                ntq_emit_alu(c, nir_instr_as_alu(instr));
                break;

        case nir_instr_type_intrinsic:
                ntq_emit_intrinsic(c, nir_instr_as_intrinsic(instr));
                break;

        case nir_instr_type_load_const:
                ntq_emit_load_const(c, nir_instr_as_load_const(instr));
                break;

        case nir_instr_type_ssa_undef:
                ntq_emit_ssa_undef(c, nir_instr_as_ssa_undef(instr));
                break;

        case nir_instr_type_tex:
                ntq_emit_tex(c, nir_instr_as_tex(instr));
                break;

        case nir_instr_type_jump:
                ntq_emit_jump(c, nir_instr_as_jump(instr));
                break;

        default:
                fprintf(stderr, "Unknown NIR instr type: ");
                nir_print_instr(instr, stderr);
                fprintf(stderr, "\n");
                abort();
        }
}

static void
ntq_emit_block(struct vc5_compile *c, nir_block *block)
{
        nir_foreach_instr(instr, block) {
                ntq_emit_instr(c, instr);
        }
}

static void ntq_emit_cf_list(struct vc5_compile *c, struct exec_list *list);

static void
ntq_emit_loop(struct vc5_compile *c, nir_loop *loop)
{
        bool was_top_level = false;
        if (c->execute.file == QFILE_NULL) {
                c->execute = qir_MOV(c, qir_uniform_ui(c, 0));
                was_top_level = true;
        }

        struct qblock *save_loop_cont_block = c->loop_cont_block;
        struct qblock *save_loop_break_block = c->loop_break_block;

        c->loop_cont_block = qir_new_block(c);
        c->loop_break_block = qir_new_block(c);

        qir_link_blocks(c->cur_block, c->loop_cont_block);
        qir_set_emit_block(c, c->loop_cont_block);
        ntq_activate_execute_for_block(c);

        ntq_emit_cf_list(c, &loop->body);

        /* Re-enable any previous continues now, so our ANYA check below
         * works.
         *
         * XXX: Use the .ORZ flags update, instead.
         */
        qir_PF(c, qir_SUB(c,
                          c->execute,
                          qir_uniform_ui(c, c->loop_cont_block->index)),
               V3D_QPU_PF_PUSHZ);
        qir_MOV_cond(c, V3D_QPU_COND_IFA, c->execute, qir_uniform_ui(c, 0));

        qir_PF(c, c->execute, V3D_QPU_PF_PUSHZ);

        qir_BRANCH(c, V3D_QPU_BRANCH_COND_ANYA);
        qir_link_blocks(c->cur_block, c->loop_cont_block);
        qir_link_blocks(c->cur_block, c->loop_break_block);

        qir_set_emit_block(c, c->loop_break_block);
        if (was_top_level)
                c->execute = c->undef;
        else
                ntq_activate_execute_for_block(c);

        c->loop_break_block = save_loop_break_block;
        c->loop_cont_block = save_loop_cont_block;
}

static void
ntq_emit_function(struct vc5_compile *c, nir_function_impl *func)
{
        fprintf(stderr, "FUNCTIONS not handled.\n");
        abort();
}

static void
ntq_emit_cf_list(struct vc5_compile *c, struct exec_list *list)
{
        foreach_list_typed(nir_cf_node, node, node, list) {
                switch (node->type) {
                case nir_cf_node_block:
                        ntq_emit_block(c, nir_cf_node_as_block(node));
                        break;

                case nir_cf_node_if:
                        ntq_emit_if(c, nir_cf_node_as_if(node));
                        break;

                case nir_cf_node_loop:
                        ntq_emit_loop(c, nir_cf_node_as_loop(node));
                        break;

                case nir_cf_node_function:
                        ntq_emit_function(c, nir_cf_node_as_function(node));
                        break;

                default:
                        fprintf(stderr, "Unknown NIR node type\n");
                        abort();
                }
        }
}

static void
ntq_emit_impl(struct vc5_compile *c, nir_function_impl *impl)
{
        ntq_setup_registers(c, &impl->registers);
        ntq_emit_cf_list(c, &impl->body);
}

static void
nir_to_qir(struct vc5_compile *c)
{
        if (c->s->stage == MESA_SHADER_FRAGMENT && c->s->info.fs.uses_discard)
                c->discard = qir_MOV(c, qir_uniform_ui(c, 0));

        ntq_setup_inputs(c);
        ntq_setup_outputs(c);
        ntq_setup_uniforms(c);
        ntq_setup_registers(c, &c->s->registers);

        /* Find the main function and emit the body. */
        nir_foreach_function(function, c->s) {
                assert(strcmp(function->name, "main") == 0);
                assert(function->impl);
                ntq_emit_impl(c, function->impl);
        }
}

const nir_shader_compiler_options vc5_nir_options = {
        .lower_extract_byte = true,
        .lower_extract_word = true,
        .lower_bitfield_insert = true,
        .lower_bitfield_extract = true,
        .lower_ffma = true,
        .lower_flrp32 = true,
        .lower_fpow = true,
        .lower_fsat = true,
        .lower_fsqrt = true,
        .lower_negate = true,
        .native_integers = true,
};


#if 0
static int
count_nir_instrs(nir_shader *nir)
{
        int count = 0;
        nir_foreach_function(function, nir) {
                if (!function->impl)
                        continue;
                nir_foreach_block(block, function->impl) {
                        nir_foreach_instr(instr, block)
                                count++;
                }
        }
        return count;
}
#endif

struct vc5_compile *
vc5_nir_shader_compile(const struct v3d_chip *chip,
                       struct vc5_key *key, nir_shader *s,
                       int debug)
{
        struct vc5_compile *c = qir_compile_init(chip, /* XXX */rzalloc(s, struct vc5_compiler_state));

        c->debug = debug;
        s = nir_shader_clone(c, s);

        c->s = s;
        /* XXX
        c->shader_state = &key->shader_state->base;
        c->program_id = key->shader_state->program_id;
        c->variant_id =
                p_atomic_inc_return(&key->shader_state->compiled_variant_count);
        */

        c->key = key;
        switch (c->s->stage) {
        case MESA_SHADER_FRAGMENT:
                c->fs_key = (struct vc5_fs_key *)key;
                if (c->fs_key->is_points) {
                        c->point_x = emit_fragment_varying(c, NULL, 0);
                        c->point_y = emit_fragment_varying(c, NULL, 0);
                } else if (c->fs_key->is_lines) {
                        c->line_x = emit_fragment_varying(c, NULL, 0);
                }
                break;
        case MESA_SHADER_VERTEX:
                c->vs_key = (struct vc5_vs_key *)key;
                break;
        default:
                unreachable("bad stage");
        }

        struct nir_lower_tex_options tex_options = {
                .lower_rect = false, /* XXX */
                .lower_txp = ~0,
        };

        NIR_PASS_V(c->s, nir_lower_tex, &tex_options);

        if (c->fs_key) {
                if (c->fs_key->light_twoside)
                        NIR_PASS_V(c->s, nir_lower_two_sided_color);

                if (c->fs_key->clamp_color)
                        NIR_PASS_V(c->s, nir_lower_clamp_color_outputs);

                if (c->fs_key->alpha_test) {
                        NIR_PASS_V(c->s, nir_lower_alpha_test,
                                   c->fs_key->alpha_test_func,
                                   false);
                }
        }

        if (c->vs_key && c->vs_key->clamp_color)
                NIR_PASS_V(c->s, nir_lower_clamp_color_outputs);

        if (c->key->ucp_enables) {
                if (c->s->stage == MESA_SHADER_FRAGMENT) {
                        NIR_PASS_V(c->s, nir_lower_clip_fs, c->key->ucp_enables);
                } else {
                        NIR_PASS_V(c->s, nir_lower_clip_vs, c->key->ucp_enables);
                        NIR_PASS_V(c->s, nir_lower_io_to_scalar,
                                   nir_var_shader_out);
                }
        }

        /* FS input scalarizing must happen after nir_lower_two_sided_color,
         * which only handles a vec4 at a time.  Similarly, VS output
         * scalarizing must happen after nir_lower_clip_vs.
         */
        if (c->s->stage == MESA_SHADER_FRAGMENT)
                NIR_PASS_V(c->s, nir_lower_io_to_scalar, nir_var_shader_in);
        else
                NIR_PASS_V(c->s, nir_lower_io_to_scalar, nir_var_shader_out);

        NIR_PASS_V(c->s, vc5_nir_lower_io, c);
        NIR_PASS_V(c->s, nir_lower_idiv);

        vc5_optimize_nir(c->s);

        NIR_PASS_V(c->s, nir_convert_from_ssa, true);

        if (c->debug & VC5_DEBUG_NIR) {
                fprintf(stderr, "%s prog %d/%d NIR:\n",
                        qir_get_stage_name(c),
                        c->program_id, c->variant_id);
                nir_print_shader(c->s, stderr);
        }

        nir_to_qir(c);

        switch (c->s->stage) {
        case MESA_SHADER_FRAGMENT:
                emit_frag_end(c);
                break;
        case MESA_SHADER_VERTEX:
                if (c->vs_key->is_coord) {
                        emit_coord_end(c);
                } else {
                        emit_vert_end(c,
                                      c->vs_key->fs_inputs->input_slots,
                                      c->vs_key->fs_inputs->num_inputs);
                }
                break;
        default:
                unreachable("bad stage");
        }

        if (c->debug & VC5_DEBUG_QIR) {
                fprintf(stderr, "%s prog %d/%d pre-opt QIR:\n",
                        qir_get_stage_name(c),
                        c->program_id, c->variant_id);
                qir_dump(c);
                fprintf(stderr, "\n");
        }

        qir_optimize(c);
        qir_lower_uniforms(c);

        /* XXX: qir_schedule_instructions(c); */
        /* qir_emit_uniform_stream_resets(c); */

        if (c->debug & VC5_DEBUG_QIR) {
                fprintf(stderr, "%s prog %d/%d QIR:\n",
                        qir_get_stage_name(c),
                        c->program_id, c->variant_id);
                qir_dump(c);
                fprintf(stderr, "\n");
        }

        /* XXX qir_reorder_uniforms(c); */
        vc5_qir_to_qpu(c);

        if (c->debug & VC5_DEBUG_SHADERDB) {
                fprintf(stderr, "SHADER-DB: %s prog %d/%d: %d instructions\n",
                        qir_get_stage_name(c),
                        c->program_id, c->variant_id,
                        c->qpu_inst_count);
                fprintf(stderr, "SHADER-DB: %s prog %d/%d: %d uniforms\n",
                        qir_get_stage_name(c),
                        c->program_id, c->variant_id,
                        c->num_uniforms);
        }

        ralloc_free(c->s);

        return c;
}
