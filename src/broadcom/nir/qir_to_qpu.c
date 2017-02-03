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

#include "nir/vc5_compiler.h"
#include "qpu/qpu_instr.h"
#include "qpu/vc5_qpu_disasm.h"

static inline struct qpu_reg
qpu_reg(int index)
{
        struct qpu_reg reg = {
                .magic = false,
                .index = index,
        };
        return reg;
}

static inline struct qpu_reg
qpu_magic(enum vc5_qpu_waddr waddr)
{
        struct qpu_reg reg = {
                .magic = true,
                .index = waddr,
        };
        return reg;
}

static inline struct qpu_reg
qpu_acc(int acc)
{
        return qpu_magic(VC5_QPU_WADDR_R0 + acc);
}

static struct v3d_qpu_instr
v3d_qpu_nop(void)
{
        struct v3d_qpu_instr instr = {
                .type = V3D_QPU_INSTR_TYPE_ALU,
                .alu = {
                        .add = {
                                .op = V3D_QPU_A_NOP,
                                .waddr = VC5_QPU_WADDR_NOP,
                                .magic_write = true,
                        },
                        .mul = {
                                .op = V3D_QPU_M_NOP,
                                .waddr = VC5_QPU_WADDR_NOP,
                                .magic_write = true,
                        },
                }
        };

        return instr;
}

static struct v3d_qpu_instr *
new_qpu_instr(struct qblock *block)
{
        struct queued_qpu_inst *q = rzalloc(block, struct queued_qpu_inst);

        q->instr = v3d_qpu_nop();
        q->uniform = ~0;

        list_addtail(&q->link, &block->qpu_inst_list);

        return &q->instr;
}

static void
set_instr_container_uniform(struct v3d_qpu_instr *instr, uint32_t index)
{
        struct queued_qpu_inst *q = container_of(instr, q, instr);
        q->uniform = index;
}

static void
new_ldunif_instr(struct qblock *block, int index)
{
        struct v3d_qpu_instr *instr = new_qpu_instr(block);
        instr->sig.ldunif = true;

        set_instr_container_uniform(instr, index);
}

/**
 * Allocates the src register (accumulator or register file) into the RADDR
 * fields of the instruction.
 */
static void
set_src(struct v3d_qpu_instr *instr, uint8_t *mux, struct qpu_reg src)
{
        if (src.magic) {
                assert(src.index >= VC5_QPU_WADDR_R0 &&
                       src.index <= VC5_QPU_WADDR_R5);
                *mux = src.index - VC5_QPU_WADDR_R0 + VC5_MUX_R0;
                return;
        }

        if (instr->alu.add.a != VC5_MUX_A &&
            instr->alu.add.b != VC5_MUX_A &&
            instr->alu.mul.a != VC5_MUX_A &&
            instr->alu.mul.b != VC5_MUX_A) {
                instr->raddr_a = src.index;
                *mux = VC5_MUX_A;
        } else {
                if (instr->raddr_a == src.index) {
                        *mux = VC5_MUX_A;
                } else {
                        assert(!(instr->alu.add.a == VC5_MUX_B &&
                                 instr->alu.add.b == VC5_MUX_B &&
                                 instr->alu.mul.a == VC5_MUX_B &&
                                 instr->alu.mul.b == VC5_MUX_B) ||
                               src.index == instr->raddr_b);

                        instr->raddr_b = src.index;
                        *mux = VC5_MUX_B;
                }
        }
}

static bool
emit_add_op(struct qblock *block,
            struct qinst *qinst, struct qpu_reg dst, struct qpu_reg *src)
{
        enum v3d_qpu_add_op op;

        switch (qinst->op) {
        case QOP_VPMSETUP_WRITE:
        case QOP_VPMSETUP_READ:
                /* These two are the same instruction in the HW, with
                 * different bits in the argument.  We treat them separately
                 * in QIR for scheduling purposes.
                 */
                op = V3D_QPU_A_VPMSETUP;
                break;

                /* Special QIR ops for getting the r5 reference. */
        case QOP_VARY_ADD_C:
                op = V3D_QPU_A_FADD;
                break;
        case QOP_VARY_MOV_C:
                /* R5 appears 2 instructions after ldvary. */
                new_qpu_instr(block);
                op = V3D_QPU_A_OR;
                break;

        default:
                if (!qir_get_add_op(qinst, &op))
                        return false;
                break;
        }

        struct v3d_qpu_instr *instr = new_qpu_instr(block);
        instr->alu.add.op = op;

        instr->alu.add.a_unpack = qinst->src[0].pack;
        if (qinst->op == QOP_VARY_ADD_C) {
                set_src(instr, &instr->alu.add.a, src[0]);
                set_src(instr, &instr->alu.add.b, qpu_acc(5));
        } if (qinst->op == QOP_VARY_MOV_C) {
                set_src(instr, &instr->alu.add.a, qpu_acc(5));
                set_src(instr, &instr->alu.add.b, qpu_acc(5));
        } else if (qir_get_non_sideband_nsrc(qinst) > 0) {
                set_src(instr, &instr->alu.add.a, src[0]);
                if (qir_get_non_sideband_nsrc(qinst) > 1) {
                        set_src(instr, &instr->alu.add.b, src[1]);
                } else {
                        set_src(instr, &instr->alu.add.a, src[0]);
                }
                instr->alu.add.b_unpack = qinst->src[1].pack;
        }
        instr->alu.add.waddr = dst.index;
        instr->alu.add.magic_write = dst.magic;

        instr->flags.apf = qinst->pf;
        instr->flags.ac = qinst->cond.alu;

        if (qinst->op == QOP_VPMSETUP_WRITE ||
            qinst->op == QOP_VPMSETUP_READ) {
                new_qpu_instr(block);
                new_qpu_instr(block);
                new_qpu_instr(block);
        }

        if (qir_has_implicit_uniform(qinst)) {
                int src = qir_get_implicit_uniform_src(qinst);
                assert(qinst->src[src].file == QFILE_UNIF);
                set_instr_container_uniform(instr, qinst->src[src].index);
        }

        return true;
}

static bool
emit_mul_op(struct qblock *block,
            struct qinst *qinst, struct qpu_reg dst, struct qpu_reg *src)
{
        enum v3d_qpu_mul_op op;

        if (!qir_get_mul_op(qinst, &op))
                return false;

        struct v3d_qpu_instr *instr = new_qpu_instr(block);
        instr->alu.mul.op = op;

        set_src(instr, &instr->alu.mul.a, src[0]);
        instr->alu.mul.a_unpack = qinst->src[0].pack;

        if (qir_get_non_sideband_nsrc(qinst) > 1) {
                set_src(instr, &instr->alu.mul.b, src[1]);
                instr->alu.mul.b_unpack = qinst->src[1].pack;
        } else {
                set_src(instr, &instr->alu.mul.b, src[0]);
        }
        instr->alu.mul.waddr = dst.index;
        instr->alu.mul.magic_write = dst.magic;

        instr->flags.mpf = qinst->pf;
        instr->flags.mc = qinst->cond.alu;

        if (qir_has_implicit_uniform(qinst)) {
                int src = qir_get_implicit_uniform_src(qinst);
                assert(qinst->src[src].file == QFILE_UNIF);
                set_instr_container_uniform(instr, qinst->src[src].index);
        }

        return true;
}

/* No TLB as first instruction of the program or after a thrsw. */
static void
emit_tlb_nop_at_program_start(struct qblock *block)
{
        if (list_empty(&block->qpu_inst_list))
                new_qpu_instr(block);
}

static void
vc5_generate_code_block(struct vc5_compile *c,
                        struct qblock *block,
                        struct qpu_reg *temp_registers)
{
        int last_vpm_read_index = -1;

        qir_for_each_inst(qinst, block) {
#if 0
                fprintf(stderr, "translating qinst to qpu: ");
                qir_dump_inst(c, qinst);
                fprintf(stderr, "\n");
#endif

                struct v3d_qpu_instr *instr;

                struct qpu_reg src[ARRAY_SIZE(qinst->src)];
                for (int i = 0; i < qir_get_non_sideband_nsrc(qinst); i++) {
                        int index = qinst->src[i].index;
                        switch (qinst->src[i].file) {
                        case QFILE_NULL:
                        case QFILE_LOAD_IMM:
                                src[i] = qpu_acc(0);
                                break;
                        case QFILE_TEMP:
                                src[i] = temp_registers[index];
                                break;
                        case QFILE_UNIF:
                                new_ldunif_instr(block, qinst->src[i].index);

                                src[i] = qpu_acc(5);
                                break;
                        case QFILE_VARY:
                                instr = new_qpu_instr(block);
                                instr->sig.ldvary = true;

                                src[i] = qpu_acc(3);
                                break;
                        case QFILE_SMALL_IMM:
                                abort(); /* XXX */
#if 0
                                src[i].mux = QPU_MUX_SMALL_IMM;
                                src[i].addr = qpu_encode_small_immediate(qinst->src[i].index);
                                /* This should only have returned a valid
                                 * small immediate field, not ~0 for failure.
                                 */
                                assert(src[i].addr <= 47);
#endif
                                break;

                        case QFILE_VPM:
                                assert((int)qinst->src[i].index >=
                                       last_vpm_read_index);
                                (void)last_vpm_read_index;
                                last_vpm_read_index = qinst->src[i].index;

                                instr = new_qpu_instr(block);
                                instr->sig.ldvpm = true;

                                src[i] = qpu_acc(3);
                                break;

                        case QFILE_TLB:
                        case QFILE_TLBU:
                        case QFILE_TMU:
                        case QFILE_TMUU:
                        case QFILE_TMUL:
                        case QFILE_TMUUL:
                        case QFILE_TMUA:
                                unreachable("bad qir src file");
                        }
                }

                struct qpu_reg dst;
                switch (qinst->dst.file) {
                case QFILE_NULL:
                        dst = qpu_magic(VC5_QPU_WADDR_NOP);
                        break;

                case QFILE_TEMP:
                        dst = temp_registers[qinst->dst.index];
                        break;

                case QFILE_VPM:
                        dst = qpu_magic(VC5_QPU_WADDR_VPM);
                        break;

                case QFILE_TLB:
                        emit_tlb_nop_at_program_start(block);
                        dst = qpu_magic(VC5_QPU_WADDR_TLB);
                        break;

                case QFILE_TLBU:
                        emit_tlb_nop_at_program_start(block);
                        dst = qpu_magic(VC5_QPU_WADDR_TLBU);
                        break;

                case QFILE_TMU:
                case QFILE_TMUU:
                        dst = qpu_magic(VC5_QPU_WADDR_TMU);
                        break;

                case QFILE_TMUL:
                case QFILE_TMUUL:
                        dst = qpu_magic(VC5_QPU_WADDR_TMUL);
                        break;

                case QFILE_TMUA:
                        dst = qpu_magic(VC5_QPU_WADDR_TMUA);
                        break;

                case QFILE_VARY:
                case QFILE_UNIF:
                case QFILE_SMALL_IMM:
                case QFILE_LOAD_IMM:
                        assert(!"not reached");
                        break;
                }

                switch (qinst->op) {
                case QOP_RCP:
                case QOP_RSQ:
                case QOP_EXP2:
                case QOP_LOG2:
                case QOP_SIN: {
                        int magic = ~0;
                        switch (qinst->op) {
                        case QOP_RCP:
                                magic = VC5_QPU_WADDR_RECIP;
                                break;
                        case QOP_RSQ:
                                magic = VC5_QPU_WADDR_RSQRT;
                                break;
                        case QOP_EXP2:
                                magic = VC5_QPU_WADDR_EXP;
                                break;
                        case QOP_LOG2:
                                magic = VC5_QPU_WADDR_LOG;
                                break;
                        case QOP_SIN:
                                magic = VC5_QPU_WADDR_SIN;
                                break;
                        default:
                                unreachable("Bad op");
                        }

                        /* XXX */
                        instr = new_qpu_instr(block);
                        instr->alu.mul.op = V3D_QPU_M_MOV;
                        instr->alu.mul.waddr = magic;
                        set_src(instr, &instr->alu.mul.a, src[0]);

                        /* XXX: NOPs that should be handled by scheduling. */
                        new_qpu_instr(block);
                        new_qpu_instr(block);

                        if (!dst.magic || dst.index != VC5_QPU_WADDR_R4) {
                                instr = new_qpu_instr(block);
                                instr->alu.mul.op = V3D_QPU_M_MOV;
                                instr->alu.mul.magic_write = dst.magic;
                                instr->alu.mul.waddr = dst.index;

                                set_src(instr, &instr->alu.mul.a, qpu_acc(4));
                        }

                        break;
                }

                case QOP_LDTMU:
                        new_qpu_instr(block)->sig.ldtmu = true;

                        if (!dst.magic || dst.index != VC5_QPU_WADDR_R4) {
                                instr = new_qpu_instr(block);
                                instr->alu.mul.op = V3D_QPU_M_FMOV;
                                instr->alu.mul.waddr = dst.index;
                                instr->alu.mul.magic_write = dst.magic;
                                set_src(instr, &instr->alu.mul.a, qpu_acc(4));
                        }

                        break;

#if 0 /* XXX */
                case QOP_LOAD_IMM:
                        assert(qinst->src[0].file == QFILE_LOAD_IMM);
                        queue(block, qpu_load_imm_ui(dst, qinst->src[0].index));
                        break;

                case QOP_LOAD_IMM_U2:
                        queue(block, qpu_load_imm_u2(dst, qinst->src[0].index));
                        break;

                case QOP_LOAD_IMM_I2:
                        queue(block, qpu_load_imm_i2(dst, qinst->src[0].index));
                        break;
#endif

#if 0
                case QOP_MS_MASK:
                        src[1] = qpu_ra(QPU_R_MS_REV_FLAGS);
                        queue(block, qpu_a_AND(qpu_ra(QPU_W_MS_FLAGS),
                                               src[0], src[1]) | unpack);
                        break;
#endif

                case QOP_FRAG_Z:
                case QOP_FRAG_W:
                case QOP_FRAG_W_CENTROID:
                        /* QOP_FRAG_Z/W don't emit instructions, just allocate
                         * the register to the Z/W payload.
                         */
                        break;

                        /*
                case QOP_TLB_COLOR_READ:
                        queue(block, qpu_NOP());
                        *last_inst(block) = qpu_set_sig(*last_inst(block),
                                                        QPU_SIG_COLOR_LOAD);
                        handle_r4_qpu_write(block, qinst, dst);
                        break;
                        */

                case QOP_BRANCH:
                        /* The branch target will be updated at QPU scheduling
                         * time.
                         */
                        instr = new_qpu_instr(block);
                        instr->type = V3D_QPU_INSTR_TYPE_BRANCH;
                        instr->branch.cond = qinst->cond.branch;
                        instr->branch.msfign = V3D_QPU_MSFIGN_NONE;
                        instr->branch.bdi = V3D_QPU_BRANCH_DEST_REL;

                        instr->branch.ub = true;
                        instr->branch.bdu = V3D_QPU_BRANCH_DEST_REL;

                        /* Set up the uniform branch as well. */
                        if (qir_has_implicit_uniform(qinst)) {
                                int src = qir_get_implicit_uniform_src(qinst);
                                assert(qinst->src[src].file == QFILE_UNIF);
                                set_instr_container_uniform(instr,
                                                           qinst->src[src].index);
                        } else {
                                unreachable("branch missing its uniform update slot");
                        }

                        /* XXX: delay slots for now */
                        new_qpu_instr(block);
                        new_qpu_instr(block);
                        new_qpu_instr(block);

                        break;

#if 0 /* XXX */
                case QOP_UNIFORMS_RESET:
                        queue(block, qpu_a_ADD(qpu_ra(QPU_W_UNIFORMS_ADDRESS),
                                               src[0], src[1]));
                        break;
#endif

                        break;

                default:
                        if (!(emit_add_op(block, qinst, dst, src) ||
                              emit_mul_op(block, qinst, dst, src))) {
                                fprintf(stderr, "Failed to convert:\n");
                                qir_dump_inst(c, qinst);
                                fprintf(stderr, "\n");
                                abort();
                        }
                }
        }
}

uint32_t
qpu_schedule_instructions(struct vc5_compile *c)
{
        int num_insts = 0;

        c->num_uniforms = 0;

        qir_for_each_block(block, c) {
                block->start_qpu_ip = num_insts;
                block->start_uniform = c->num_uniforms;

                list_for_each_entry(struct queued_qpu_inst, inst,
                                    &block->qpu_inst_list, link) {
                        if (inst->instr.type == V3D_QPU_INSTR_TYPE_BRANCH) {
                                block->branch_qpu_ip = num_insts;
                        }

                        num_insts++;
                        if (inst->uniform != ~0)
                                c->num_uniforms++;
                }

                block->end_qpu_ip = num_insts;
        }

        num_insts += 2;

        c->qpu_insts = rzalloc_array(c, uint64_t, num_insts);

        uint32_t *uniform_data = c->uniform_data;
        enum quniform_contents *uniform_contents = c->uniform_contents;
        c->uniform_contents = ralloc_array(c, enum quniform_contents,
                                           c->num_uniforms);
        c->uniform_data = ralloc_array(c, uint32_t, c->num_uniforms);
        c->uniform_array_size = c->num_uniforms;

        int i = 0;
        int unif = 0;
        qir_for_each_block(block, c) {
                list_for_each_entry(struct queued_qpu_inst, inst,
                                    &block->qpu_inst_list, link) {
                        if (inst->instr.type == V3D_QPU_INSTR_TYPE_BRANCH) {
                                /* Make sure that the if-we-don't-jump
                                 * successor was scheduled just after the
                                 * delay slots.
                                 */
                                assert(!block->successors[1] ||
                                       block->successors[1]->start_qpu_ip ==
                                       block->branch_qpu_ip + 4);

                                inst->instr.branch.offset =
                                        ((block->successors[0]->start_qpu_ip -
                                          block->branch_qpu_ip + 4) *
                                         sizeof(uint64_t));

                                /* Set up the relative offset to jump in the
                                 * uniform stream.
                                 */
                                uniform_data[inst->uniform] =
                                        (block->successors[0]->start_uniform -
                                         (unif + 1)) * 4;
                        }

                        bool ok = v3d_qpu_instr_pack(c->chip, &inst->instr,
                                                     &c->qpu_insts[i++]);
                        assert(ok); (void) ok;

                        if (inst->uniform != ~0) {
                                c->uniform_contents[unif] =
                                        uniform_contents[inst->uniform];
                                c->uniform_data[unif] =
                                        uniform_data[inst->uniform];
                                unif++;
                        }
                }

        }

        struct v3d_qpu_instr nop = v3d_qpu_nop();
        uint64_t packed_nop;
        bool ok = v3d_qpu_instr_pack(c->chip, &nop, &packed_nop);
        assert(ok); (void) ok;

        c->qpu_insts[i++] = packed_nop;
        c->qpu_insts[i++] = packed_nop;
        assert(i == num_insts);
        c->qpu_inst_count = num_insts;

        return num_insts;
}

static void
vc5_dump_qpu(struct vc5_compile *c)
{
        fprintf(stderr, "%s prog %d/%d QPU:\n",
                qir_get_stage_name(c),
                c->program_id, c->variant_id);

        for (int i = 0; i < c->qpu_inst_count; i++) {
                const char *str = vc5_qpu_disasm(c->chip, c->qpu_insts[i]);
                fprintf(stderr, "0x%016"PRIx64" %s\n", c->qpu_insts[i], str);
        }
        fprintf(stderr, "\n");
}

void
vc5_qir_to_qpu(struct vc5_compile *c)
{
        struct qpu_reg *temp_registers = vc5_register_allocate(c);
        /*struct qblock *start_block = list_first_entry(&c->blocks,
                                                      struct qblock, link);
        */
        struct qblock *end_block = list_last_entry(&c->blocks,
                                                   struct qblock, link);

        /* XXX
        if (c->s->stage == MESA_SHADER_VERTEX) {
                c->num_inputs_remaining = c->num_inputs;
                queue(start_block, qpu_load_imm_ui(qpu_vwsetup(), 0x00001a00));
        }
        */

        qir_for_each_block(block, c)
                vc5_generate_code_block(c, block, temp_registers);

        new_qpu_instr(end_block)->sig.thrsw = true;

        uint32_t cycles = qpu_schedule_instructions(c);
        uint32_t inst_count_at_schedule_time = c->qpu_inst_count;

        cycles += c->qpu_inst_count - inst_count_at_schedule_time;

        if (c->debug & VC5_DEBUG_SHADERDB) {
                fprintf(stderr, "SHADER-DB: %s prog %d/%d: %d estimated cycles\n",
                        qir_get_stage_name(c),
                        c->program_id, c->variant_id,
                        cycles);
        }

        if (c->debug & VC5_DEBUG_QPU)
                vc5_dump_qpu(c);

        vc5_qpu_validate(c->chip, c->qpu_insts, c->qpu_inst_count);

        free(temp_registers);
}
