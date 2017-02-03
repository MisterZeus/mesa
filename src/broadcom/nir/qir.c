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

#include "vc5_compiler.h"

struct qir_op_info {
        bool is_add;
        bool is_mul;
        union {
                enum v3d_qpu_add_op add;
                enum v3d_qpu_mul_op mul;
                struct {
                        const char *name;
                        uint8_t ndst, nsrc;
                        bool has_side_effects;
                } special;
        };
};

#define ADD_OP(op)                                      \
        [QOP_ ## op] = {                                \
                .is_add = true,                         \
                .add = V3D_QPU_A_ ## op,                \
        }

#define MUL_OP(op)                                      \
        [QOP_ ## op] = {                                \
                .is_mul = true,                         \
                .mul = V3D_QPU_M_ ## op,                \
        }

static const struct qir_op_info qir_op_info[] = {
        ADD_OP(FADD),
        /* XXX ADD_OP(FADDNF), */
        ADD_OP(VFPACK),
        ADD_OP(ADD),
        ADD_OP(SUB),
        ADD_OP(FSUB),
        ADD_OP(MIN),
        ADD_OP(MAX),
        ADD_OP(UMIN),
        ADD_OP(UMAX),
        ADD_OP(SHL),
        ADD_OP(SHR),
        ADD_OP(ASR),
        ADD_OP(ROR),
        ADD_OP(FMIN),
        ADD_OP(FMAX),
        ADD_OP(AND),
        ADD_OP(OR),
        ADD_OP(XOR),
        ADD_OP(VADD),
        ADD_OP(VSUB),
        ADD_OP(NOT),
        ADD_OP(NEG),
        ADD_OP(FLAPUSH),
        ADD_OP(FLBPUSH),
        ADD_OP(FLBPOP),
        ADD_OP(SETMSF),
        ADD_OP(SETREVF),
        ADD_OP(EIDX),
        ADD_OP(TIDX),

        ADD_OP(FXCD),
        ADD_OP(XCD),
        ADD_OP(FYCD),
        ADD_OP(YCD),
        ADD_OP(MSF),
        ADD_OP(REVF),
        ADD_OP(FCMP),
        ADD_OP(VFMAX),

        ADD_OP(FROUND),
        ADD_OP(FTOIN),
        ADD_OP(FTRUNC),
        ADD_OP(FTOIZ),
        ADD_OP(FFLOOR),
        ADD_OP(FTOUZ),
        ADD_OP(FCEIL),
        ADD_OP(FTOC),
        ADD_OP(FDX),
        ADD_OP(FDY),

        ADD_OP(ITOF),
        ADD_OP(CLZ),
        ADD_OP(UTOF),

        MUL_OP(UMUL24),
        MUL_OP(VFMUL),
        MUL_OP(SMUL24),
        MUL_OP(MULTOP),
        MUL_OP(FMOV),
        MUL_OP(MOV),
        MUL_OP(FMUL),

        [QOP_VPMSETUP_READ] = { .special = { "vpmsetup_read", 0, 1, true } },
        [QOP_VPMSETUP_WRITE] = { .special = { "vpmsetup_write", 0, 1, true } },

        [QOP_RCP] = { .special = { "rcp", 1, 1 } },
        [QOP_RSQ] = { .special = { "rsq", 1, 1 } },
        [QOP_EXP2] = { .special = { "exp2", 1, 1 } },
        [QOP_LOG2] = { .special = { "log2", 1, 1 } },
        [QOP_SIN] = { .special = { "sin", 1, 1 } },

        /* XXX [QOP_TLB_COLOR_READ] = { "tlb_color_read", 1, 0 }, */
        [QOP_VARY_ADD_C] = { .special = { "vary_add_c", 1, 1 } },
        [QOP_VARY_MOV_C] = { .special = { "vary_mov_c", 1, 1 } },

        [QOP_FRAG_Z] = { .special = { "frag_z", 1, 0 } },
        [QOP_FRAG_W] = { .special = { "frag_w", 1, 0 } },

        [QOP_TEX_DIRECT] = { .special = { "tex_direct", 0, 2, true } },
        [QOP_LDTMU] = { .special = { "ldtmu", 1, 0, true } },

        [QOP_LOAD_IMM] = { .special = { "load_imm", 1, 0 } },
        [QOP_LOAD_IMM_U2] = { .special = { "load_imm_u2", 1, 0 } },
        [QOP_LOAD_IMM_I2] = { .special = { "load_imm_i2", 1, 0 } },

        [QOP_BRANCH] = { .special = { "branch", 0, 0, true } },
};

static const char *
qir_get_op_name(enum qop qop)
{
        assert(qop < ARRAY_SIZE(qir_op_info));

        if (qir_op_info[qop].is_add)
                return v3d_qpu_add_op_name(qir_op_info[qop].add);
        else if (qir_op_info[qop].is_mul)
                return v3d_qpu_mul_op_name(qir_op_info[qop].mul);
        else {
                assert(qir_op_info[qop].special.name);
                return qir_op_info[qop].special.name;
        }
}

int
qir_get_non_sideband_nsrc(struct qinst *inst)
{
        assert(inst->op < ARRAY_SIZE(qir_op_info));

        if (qir_op_info[inst->op].is_add)
                return v3d_qpu_add_op_num_src(qir_op_info[inst->op].add);
        else if (qir_op_info[inst->op].is_mul)
                return v3d_qpu_mul_op_num_src(qir_op_info[inst->op].mul);
        else {
                assert(qir_op_info[inst->op].special.name);
                return qir_op_info[inst->op].special.nsrc;
        }
}

int
qir_get_nsrc(struct qinst *inst)
{
        int nsrc = qir_get_non_sideband_nsrc(inst);

        if (qir_has_implicit_uniform(inst))
                nsrc++;

        return nsrc;
}

bool
qir_has_implicit_uniform(struct qinst *inst)
{
        switch (inst->op) {
        case QOP_BRANCH:
                return true;
        default:
                break;
        }

        switch (inst->dst.file) {
        case QFILE_TLBU:
        case QFILE_TMUU:
        case QFILE_TMUUL:
                return true;
        default:
                return false;
        }
}

/* The sideband uniform for textures gets stored after the normal ALU
 * arguments.
 */
int
qir_get_implicit_uniform_src(struct qinst *inst)
{
        return qir_get_nsrc(inst) - 1;
}

/**
 * Returns whether the instruction has any side effects that must be
 * preserved.
 */
bool
qir_has_side_effects(struct vc5_compile *c, struct qinst *inst)
{
#if 0 /* XXX */
        switch (inst->dst.file) {
        case QFILE_TLB_Z_WRITE:
        case QFILE_TLB_COLOR_WRITE:
        case QFILE_TLB_COLOR_WRITE_MS:
        case QFILE_TLB_STENCIL_SETUP:
                return true;
        default:
                break;
        }
#endif

        switch (inst->op) {
        case QOP_SETREVF:
        case QOP_SETMSF:
        case QOP_MULTOP:
                return true;
        default:
                if (!qir_op_info[inst->op].is_add &&
                    !qir_op_info[inst->op].is_mul) {
                        return qir_op_info[inst->op].special.has_side_effects;
                } else {
                        return false;
                }
        }
}

bool
qir_has_side_effect_reads(struct vc5_compile *c, struct qinst *inst)
{
        /* We can dead-code eliminate varyings, because we only tell the VS
         * about the live ones at the end.  But we have to preserve the
         * point/line coordinates reads, because they're generated by
         * fixed-function hardware.
         */
        for (int i = 0; i < qir_get_nsrc(inst); i++) {
                if (inst->src[i].file == QFILE_VARY &&
                    c->input_slots[inst->src[i].index].slot == 0xff) {
                        return true;
                }

                if (inst->src[i].file == QFILE_VPM)
                        return true;
        }

        if (inst->dst.file == QFILE_VPM)
                return true;

        return false;
}

bool
qir_get_add_op(struct qinst *inst, enum v3d_qpu_add_op *op)
{
        if (qir_op_info[inst->op].is_add) {
                *op = qir_op_info[inst->op].add;
                return true;
        } else {
                return false;
        }
}

bool
qir_get_mul_op(struct qinst *inst, enum v3d_qpu_mul_op *op)
{
        if (qir_op_info[inst->op].is_mul) {
                *op = qir_op_info[inst->op].mul;
                return true;
        } else {
                return false;
        }
}

bool
qir_is_float_input(struct qinst *inst)
{
        switch (inst->op) {
        case QOP_FMOV:
        case QOP_FMUL:
        case QOP_FADD:
        case QOP_FSUB:
        case QOP_FMIN:
        case QOP_FMAX:
        case QOP_FTOIN:
                /* XXX */
                return true;
        default:
                return false;
        }
}

bool
qir_is_raw_mov(struct qinst *inst)
{
        return ((inst->op == QOP_MOV ||
                 inst->op == QOP_FMOV) &&
                inst->cond.alu == V3D_QPU_COND_NONE &&
                !inst->dst.pack &&
                !inst->src[0].pack);
}

bool
qir_is_tex(struct qinst *inst)
{
        switch (inst->dst.file) {
        case QFILE_TMU:
        case QFILE_TMUU:
        case QFILE_TMUL:
        case QFILE_TMUUL:
        case QFILE_TMUA:
                return true;
        default:
                return inst->op == QOP_TEX_DIRECT;
        }

}

bool
qir_depends_on_flags(struct qinst *inst)
{
        if (inst->op == QOP_BRANCH) {
                return (inst->cond.branch != V3D_QPU_BRANCH_COND_ALWAYS);
        } else {
                return (inst->cond.alu != V3D_QPU_COND_NONE);
        }
}

bool
qir_writes_r3(struct qinst *inst)
{
        for (int i = 0; i < qir_get_nsrc(inst); i++) {
                switch (inst->src[i].file) {
                case QFILE_VARY:
                case QFILE_VPM:
                        return true;
                default:
                        break;
                }
        }

        return false;
}

bool
qir_writes_r4(struct qinst *inst)
{
        switch (inst->op) {
        case QOP_LDTMU:
        case QOP_TLB_COLOR_READ:
        case QOP_RCP:
        case QOP_RSQ:
        case QOP_EXP2:
        case QOP_LOG2:
        case QOP_SIN:
                return true;
        default:
                return false;
        }
}

#if 0
uint8_t
qir_channels_written(struct qinst *inst)
{
        if (qir_is_mul(inst)) {
                switch (inst->dst.pack) {
                case QPU_PACK_MUL_NOP:
                case QPU_PACK_MUL_8888:
                        return 0xf;
                case QPU_PACK_MUL_8A:
                        return 0x1;
                case QPU_PACK_MUL_8B:
                        return 0x2;
                case QPU_PACK_MUL_8C:
                        return 0x4;
                case QPU_PACK_MUL_8D:
                        return 0x8;
                }
        } else {
                switch (inst->dst.pack) {
                case QPU_PACK_A_NOP:
                case QPU_PACK_A_8888:
                case QPU_PACK_A_8888_SAT:
                case QPU_PACK_A_32_SAT:
                        return 0xf;
                case QPU_PACK_A_8A:
                case QPU_PACK_A_8A_SAT:
                        return 0x1;
                case QPU_PACK_A_8B:
                case QPU_PACK_A_8B_SAT:
                        return 0x2;
                case QPU_PACK_A_8C:
                case QPU_PACK_A_8C_SAT:
                        return 0x4;
                case QPU_PACK_A_8D:
                case QPU_PACK_A_8D_SAT:
                        return 0x8;
                case QPU_PACK_A_16A:
                case QPU_PACK_A_16A_SAT:
                        return 0x3;
                case QPU_PACK_A_16B:
                case QPU_PACK_A_16B_SAT:
                        return 0xc;
                }
        }
        unreachable("Bad pack field");
}
#endif

static void
qir_print_reg(struct vc5_compile *c, struct qreg reg, bool write)
{
        static const char *files[] = {
                [QFILE_TEMP] = "t",
                [QFILE_VARY] = "v",
                [QFILE_UNIF] = "u",
                [QFILE_TLB] = "tlb",
                [QFILE_TLBU] = "tlbu",
                [QFILE_TMU] = "tmu",
                [QFILE_TMUU] = "tmu",
                [QFILE_TMUUL] = "tmul",
                [QFILE_TMUL] = "tmul",
                [QFILE_TMUA] = "tmua",
        };
        static const char *quniform_names[] = {
                [QUNIFORM_VIEWPORT_X_SCALE] = "vp_x_scale",
                [QUNIFORM_VIEWPORT_Y_SCALE] = "vp_y_scale",
                [QUNIFORM_VIEWPORT_Z_OFFSET] = "vp_z_offset",
                [QUNIFORM_VIEWPORT_Z_SCALE] = "vp_z_scale",
        };

        switch (reg.file) {

        case QFILE_NULL:
                fprintf(stderr, "null");
                break;

        case QFILE_LOAD_IMM:
                fprintf(stderr, "0x%08x (%f)", reg.index, uif(reg.index));
                break;

        case QFILE_SMALL_IMM:
                if ((int)reg.index >= -16 && (int)reg.index <= 15)
                        fprintf(stderr, "%d", reg.index);
                else
                        fprintf(stderr, "%f", uif(reg.index));
                break;

        case QFILE_VPM:
                if (write) {
                        fprintf(stderr, "vpm");
                } else {
                        fprintf(stderr, "vpm%d.%d",
                                reg.index / 4, reg.index % 4);
                }
                break;

        case QFILE_TLB:
        case QFILE_TMU:
        case QFILE_TMUU:
        case QFILE_TMUL:
        case QFILE_TMUUL:
        case QFILE_TMUA:
                fprintf(stderr, "%s", files[reg.file]);
                break;

        case QFILE_UNIF: {
                enum quniform_contents contents = c->uniform_contents[reg.index];

                fprintf(stderr, "%s%d", files[reg.file], reg.index);

                switch (contents) {
                case QUNIFORM_CONSTANT:
                        fprintf(stderr, " (0x%08x / %f)",
                                c->uniform_data[reg.index],
                                uif(c->uniform_data[reg.index]));
                        break;

                case QUNIFORM_UNIFORM:
                        fprintf(stderr, " (push[%d])",
                                c->uniform_data[reg.index]);
                        break;

                case QUNIFORM_TEXTURE_CONFIG_P1:
                        fprintf(stderr, " (tex[%d].p1)",
                                c->uniform_data[reg.index]);
                        break;

                default:
                        if (quniform_contents_is_texture_p0(contents)) {
                                fprintf(stderr, " (tex[%d].p0: 0x%08x)",
                                        contents - QUNIFORM_TEXTURE_CONFIG_P0_0,
                                        c->uniform_data[reg.index]);
                        } else if (contents < ARRAY_SIZE(quniform_names)) {
                                fprintf(stderr, " (%s)",
                                        quniform_names[contents]);
                        } else {
                                fprintf(stderr, " (%d / 0x%08x)", contents,
                                        c->uniform_data[reg.index]);
                        }
                }

                break;
        }

        default:
                fprintf(stderr, "%s%d", files[reg.file], reg.index);
                break;
        }
}

void
qir_dump_inst(struct vc5_compile *c, struct qinst *inst)
{
        fprintf(stderr, "%s", qir_get_op_name(inst->op));
        if (inst->op == QOP_BRANCH) {
                fprintf(stderr, "%s",
                        v3d_qpu_branch_cond_name(inst->cond.branch));
        } else {
                fprintf(stderr, "%s", v3d_qpu_cond_name(inst->cond.alu));
        }
        fprintf(stderr, "%s", v3d_qpu_pf_name(inst->pf));
        fprintf(stderr, " ");

        qir_print_reg(c, inst->dst, true);
        fprintf(stderr, "%s", v3d_qpu_pack_name(inst->dst.pack));

        for (int i = 0; i < qir_get_nsrc(inst); i++) {
                fprintf(stderr, ", ");
                qir_print_reg(c, inst->src[i], false);
                fprintf(stderr, "%s", v3d_qpu_unpack_name(inst->src[i].pack));
        }
}

void
qir_dump(struct vc5_compile *c)
{
        int ip = 0;

        qir_for_each_block(block, c) {
                fprintf(stderr, "BLOCK %d:\n", block->index);
                qir_for_each_inst(inst, block) {
                        if (c->temp_start) {
                                bool first = true;

                                for (int i = 0; i < c->num_temps; i++) {
                                        if (c->temp_start[i] != ip)
                                                continue;

                                        if (first) {
                                                first = false;
                                        } else {
                                                fprintf(stderr, ", ");
                                        }
                                        fprintf(stderr, "S%4d", i);
                                }

                                if (first)
                                        fprintf(stderr, "      ");
                                else
                                        fprintf(stderr, " ");
                        }

                        if (c->temp_end) {
                                bool first = true;

                                for (int i = 0; i < c->num_temps; i++) {
                                        if (c->temp_end[i] != ip)
                                                continue;

                                        if (first) {
                                                first = false;
                                        } else {
                                                fprintf(stderr, ", ");
                                        }
                                        fprintf(stderr, "E%4d", i);
                                }

                                if (first)
                                        fprintf(stderr, "      ");
                                else
                                        fprintf(stderr, " ");
                        }

                        qir_dump_inst(c, inst);
                        fprintf(stderr, "\n");
                        ip++;
                }
                if (block->successors[1]) {
                        fprintf(stderr, "-> BLOCK %d, %d\n",
                                block->successors[0]->index,
                                block->successors[1]->index);
                } else if (block->successors[0]) {
                        fprintf(stderr, "-> BLOCK %d\n",
                                block->successors[0]->index);
                }
        }
}

struct qreg
qir_get_temp(struct vc5_compile *c)
{
        struct qreg reg;

        reg.file = QFILE_TEMP;
        reg.index = c->num_temps++;
        reg.pack = 0;

        if (c->num_temps > c->defs_array_size) {
                uint32_t old_size = c->defs_array_size;
                c->defs_array_size = MAX2(old_size * 2, 16);
                c->defs = reralloc(c, c->defs, struct qinst *,
                                   c->defs_array_size);
                memset(&c->defs[old_size], 0,
                       sizeof(c->defs[0]) * (c->defs_array_size - old_size));
        }

        return reg;
}

struct qinst *
qir_inst(enum qop op, struct qreg dst, struct qreg src0, struct qreg src1)
{
        struct qinst *inst = calloc(1, sizeof(*inst));

        inst->op = op;
        inst->dst = dst;
        inst->src[0] = src0;
        inst->src[1] = src1;
        if (op == QOP_BRANCH)
                inst->cond.branch = V3D_QPU_BRANCH_COND_ALWAYS;
        else
                inst->cond.alu = V3D_QPU_COND_NONE;

        return inst;
}

static void
qir_emit(struct vc5_compile *c, struct qinst *inst)
{
        list_addtail(&inst->link, &c->cur_block->instructions);

        if (inst->dst.file == QFILE_VPM)
                c->num_vpm_writes++;
}

/* Updates inst to write to a new temporary, emits it, and notes the def. */
struct qreg
qir_emit_def(struct vc5_compile *c, struct qinst *inst)
{
        assert(inst->dst.file == QFILE_NULL);

        inst->dst = qir_get_temp(c);

        if (inst->dst.file == QFILE_TEMP)
                c->defs[inst->dst.index] = inst;

        qir_emit(c, inst);

        return inst->dst;
}

struct qinst *
qir_emit_nondef(struct vc5_compile *c, struct qinst *inst)
{
        if (inst->dst.file == QFILE_TEMP)
                c->defs[inst->dst.index] = NULL;

        qir_emit(c, inst);

        return inst;
}

bool
qir_reg_equals(struct qreg a, struct qreg b)
{
        return a.file == b.file && a.index == b.index && a.pack == b.pack;
}

struct qblock *
qir_new_block(struct vc5_compile *c)
{
        struct qblock *block = rzalloc(c, struct qblock);

        list_inithead(&block->instructions);
        list_inithead(&block->qpu_inst_list);

        block->predecessors = _mesa_set_create(block,
                                               _mesa_hash_pointer,
                                               _mesa_key_pointer_equal);

        block->index = c->next_block_index++;

        return block;
}

void
qir_set_emit_block(struct vc5_compile *c, struct qblock *block)
{
        c->cur_block = block;
        list_addtail(&block->link, &c->blocks);
}

struct qblock *
qir_entry_block(struct vc5_compile *c)
{
        return list_first_entry(&c->blocks, struct qblock, link);
}

struct qblock *
qir_exit_block(struct vc5_compile *c)
{
        return list_last_entry(&c->blocks, struct qblock, link);
}

void
qir_link_blocks(struct qblock *predecessor, struct qblock *successor)
{
        _mesa_set_add(successor->predecessors, predecessor);
        if (predecessor->successors[0]) {
                assert(!predecessor->successors[1]);
                predecessor->successors[1] = successor;
        } else {
                predecessor->successors[0] = successor;
        }
}

struct vc5_compile *
qir_compile_init(const struct v3d_chip *chip,
                 struct vc5_compiler_state *compiler_state)
{
        struct vc5_compile *c = rzalloc(NULL, struct vc5_compile);

        c->chip = chip;
        c->compiler_state = compiler_state;
        list_inithead(&c->blocks);
        qir_set_emit_block(c, qir_new_block(c));

        c->output_position_index = -1;
        c->output_point_size_index = -1;
        c->output_sample_mask_index = -1;

        c->def_ht = _mesa_hash_table_create(c, _mesa_hash_pointer,
                                            _mesa_key_pointer_equal);

        return c;
}

void
qir_remove_instruction(struct vc5_compile *c, struct qinst *qinst)
{
        if (qinst->dst.file == QFILE_TEMP)
                c->defs[qinst->dst.index] = NULL;

        list_del(&qinst->link);
        free(qinst);
}

struct qreg
qir_follow_movs(struct vc5_compile *c, struct qreg reg)
{
        int pack = reg.pack;

        while (reg.file == QFILE_TEMP &&
               c->defs[reg.index] &&
               (c->defs[reg.index]->op == QOP_MOV ||
                c->defs[reg.index]->op == QOP_FMOV) &&
               !c->defs[reg.index]->dst.pack &&
               !c->defs[reg.index]->src[0].pack) {
                reg = c->defs[reg.index]->src[0];
        }

        reg.pack = pack;
        return reg;
}

void
qir_compile_destroy(struct vc5_compile *c)
{
        qir_for_each_block(block, c) {
                while (!list_empty(&block->instructions)) {
                        struct qinst *qinst =
                                list_first_entry(&block->instructions,
                                                 struct qinst, link);
                        qir_remove_instruction(c, qinst);
                }
        }

        ralloc_free(c);
}

struct qreg
qir_uniform(struct vc5_compile *c,
            enum quniform_contents contents,
            uint32_t data)
{
        for (int i = 0; i < c->num_uniforms; i++) {
                if (c->uniform_contents[i] == contents &&
                    c->uniform_data[i] == data) {
                        return qir_reg(QFILE_UNIF, i);
                }
        }

        uint32_t uniform = c->num_uniforms++;

        if (uniform >= c->uniform_array_size) {
                c->uniform_array_size = MAX2(MAX2(16, uniform + 1),
                                             c->uniform_array_size * 2);

                c->uniform_data = reralloc(c, c->uniform_data,
                                           uint32_t,
                                           c->uniform_array_size);
                c->uniform_contents = reralloc(c, c->uniform_contents,
                                               enum quniform_contents,
                                               c->uniform_array_size);
        }

        c->uniform_contents[uniform] = contents;
        c->uniform_data[uniform] = data;

        return qir_reg(QFILE_UNIF, uniform);
}

void
qir_PF(struct vc5_compile *c, struct qreg src, enum v3d_qpu_pf pf)
{
        struct qinst *last_inst = NULL;

        if (!list_empty(&c->cur_block->instructions))
                last_inst = (struct qinst *)c->cur_block->instructions.prev;

        if (src.file != QFILE_TEMP ||
            !c->defs[src.index] ||
            last_inst != c->defs[src.index]) {
                /* XXX: Make the MOV be the appropriate type */
                last_inst = qir_MOV_dest(c, qir_reg(QFILE_NULL, 0), src);
                last_inst = (struct qinst *)c->cur_block->instructions.prev;
        }
        last_inst->pf = pf;
}

#define OPTPASS(func)                                                   \
        do {                                                            \
                bool stage_progress = func(c);                          \
                if (stage_progress) {                                   \
                        progress = true;                                \
                        if (print_opt_debug) {                          \
                                fprintf(stderr,                         \
                                        "QIR opt pass %2d: %s progress\n", \
                                        pass, #func);                   \
                        }                                               \
                        /*XXX qir_validate(c);*/                        \
                }                                                       \
        } while (0)

void
qir_optimize(struct vc5_compile *c)
{
        bool print_opt_debug = false;
        int pass = 1;

        while (true) {
                bool progress = false;

                OPTPASS(qir_opt_copy_propagate);
                OPTPASS(qir_opt_dead_code);

                if (!progress)
                        break;

                pass++;
        }
}

const char *
qir_get_stage_name(struct vc5_compile *c)
{
        if (c->vs_key && c->vs_key->is_coord)
                return "MESA_SHADER_COORD";
        else
                return gl_shader_stage_name(c->s->stage);
}
