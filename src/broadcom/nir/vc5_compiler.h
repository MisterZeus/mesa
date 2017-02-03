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

#ifndef VC5_COMPILER_H
#define VC5_COMPILER_H

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include "util/macros.h"
#include "compiler/nir/nir.h"
#include "util/list.h"
#include "util/u_math.h"

#include "qpu/qpu_instr.h"
#include "qpu/vc5_qpu_defines.h"
#include "pipe/p_state.h"

#define VC5_MAX_TEXTURE_SAMPLERS 32
#define VC5_MAX_SAMPLES 4
#define VC5_MAX_FS_INPUTS 64
#define VC5_MAX_VS_INPUTS 64

#define VC5_DEBUG_SHADERDB		(1 << 0)
#define VC5_DEBUG_NIR			(1 << 1)
#define VC5_DEBUG_QIR			(1 << 2)
#define VC5_DEBUG_QPU			(1 << 3)

struct nir_builder;

struct vc5_fs_inputs {
        /**
         * Array of the meanings of the VPM inputs this shader needs.
         *
         * It doesn't include those that aren't part of the VPM, like
         * point/line coordinates.
         */
        struct vc5_varying_slot *input_slots;
        uint32_t num_inputs;
};

enum qfile {
        QFILE_NULL,
        QFILE_TEMP,
        QFILE_VARY,
        QFILE_UNIF,
        QFILE_TLB,
        QFILE_TLBU,

        /**
         * Files for TMU texture lookups
         *
         * At the hardware level there is TMU and TMUL, where the first two
         * addresses contain an implicit config uniform that gets shifted in.
         * In QIR, we emit the first TMU/TMUL as TMUU/TMUUL, to note whether
         * they're the ones with the implicit uniform (otherwise, while
         * processing QIR we'd always have to track how many TMU writes are
         * outstanding since the last TMUL).
         */
        QFILE_TMU,
        QFILE_TMUU,
        QFILE_TMUL,
        QFILE_TMUUL,

        /** General memory access TMU lookups. */
        QFILE_TMUA,

        /** VPM writes use this as the destination */
        QFILE_VPM,

        /**
         * Stores an immediate value in the index field that will be used
         * directly by qpu_load_imm().
         */
        QFILE_LOAD_IMM,

        /**
         * Stores an immediate value in the index field that can be turned
         * into a small immediate field by qpu_encode_small_immediate().
         */
        QFILE_SMALL_IMM,
};

/**
 * A reference to a QPU register or a virtual temp register.
 */
struct qreg {
        enum qfile file;
        uint32_t index;
        int pack;
};

static inline struct qreg qir_reg(enum qfile file, uint32_t index)
{
        return (struct qreg){file, index};
}

/**
 * A reference to an actual register at the QPU level, for register
 * allocation.
 */
struct qpu_reg {
        bool magic;
        int index;
};

enum qop {
        QOP_UNDEF,

        /* Add ops */
        QOP_FADD,
        QOP_FADDNF,
        QOP_VFPACK,
        QOP_FSUB,
        QOP_FMIN,
        QOP_FMAX,
        QOP_ADD,
        QOP_SUB,
        QOP_SHL,
        QOP_SHR,
        QOP_ASR,
        QOP_ROR,
        QOP_MIN,
        QOP_MAX,
        QOP_UMIN,
        QOP_UMAX,
        QOP_AND,
        QOP_OR,
        QOP_XOR,
        QOP_VADD,
        QOP_VSUB,
        QOP_NOT,
        QOP_NEG,

        QOP_FLAPUSH,
        QOP_FLBPUSH,
        QOP_FLBPOP,
        QOP_SETMSF,
        QOP_SETREVF,

        QOP_TIDX,
        QOP_EIDX,

        QOP_FXCD,
        QOP_XCD,
        QOP_FYCD,
        QOP_YCD,
        QOP_MSF,
        QOP_REVF,

        QOP_FCMP,
        QOP_VFMAX,

        QOP_FROUND,
        QOP_FTOIN,
        QOP_FTRUNC,
        QOP_FTOIZ,
        QOP_FFLOOR,
        QOP_FTOUZ,
        QOP_FCEIL,
        QOP_FTOC,

        QOP_FDX,
        QOP_FDY,

        QOP_ITOF,
        QOP_CLZ,
        QOP_UTOF,

        QOP_UMUL24,
        QOP_VFMUL,
        QOP_SMUL24,
        QOP_MULTOP,
        QOP_FMOV,
        QOP_MOV,
        QOP_FMUL,

        /* Special ops */
        QOP_RCP,
        QOP_RSQ,
        QOP_EXP2,
        QOP_LOG2,
        QOP_SIN,

        QOP_TLB_COLOR_READ,
        QOP_VARY_ADD_C,
        QOP_VARY_MOV_C,

        QOP_FRAG_Z,
        QOP_FRAG_W,
        QOP_FRAG_W_CENTROID,

        QOP_VPMSETUP_READ,
        QOP_VPMSETUP_WRITE,

        /**
         * Texture-unit 4-byte read with address provided direct in S
         * cooordinate.
         *
         * The first operand is the offset from the start of the UBO, and the
         * second is the uniform that has the UBO's base pointer.
         */
        QOP_TEX_DIRECT,

        /**
         * Signal of texture read being necessary and then reading r4 into
         * the destination
         */
        QOP_LDTMU,

        /* 32-bit immediate loaded to each SIMD channel */
        QOP_LOAD_IMM,

        /* 32-bit immediate divided into 16 2-bit unsigned int values and
         * loaded to each corresponding SIMD channel.
         */
        QOP_LOAD_IMM_U2,
        /* 32-bit immediate divided into 16 2-bit signed int values and
         * loaded to each corresponding SIMD channel.
         */
        QOP_LOAD_IMM_I2,

        /* Jumps to block->successor[0] if the qinst->cond (as a
         * QPU_COND_BRANCH_*) passes, or block->successor[1] if not.  Note
         * that block->successor[1] may be unset if the condition is ALWAYS.
         */
        QOP_BRANCH,
};

struct queued_qpu_inst {
        struct list_head link;
        struct v3d_qpu_instr instr;

        /* If instr reads a uniform, which uniform from the uncompiled stream
         * it is.
         */
        int uniform;
};

struct qinst {
        struct list_head link;

        enum qop op;
        struct qreg dst;
        struct qreg src[3];
        bool cond_is_exec_mask;
        union {
                enum v3d_qpu_cond alu;
                enum v3d_qpu_branch_cond branch;
        } cond;
        uint8_t pf;
};

enum quniform_contents {
        /**
         * Indicates that a constant 32-bit value is copied from the program's
         * uniform contents.
         */
        QUNIFORM_CONSTANT,
        /**
         * Indicates that the program's uniform contents are used as an index
         * into the GL uniform storage.
         */
        QUNIFORM_UNIFORM,

        /** @{
         * Scaling factors from clip coordinates to relative to the viewport
         * center.
         *
         * This is used by the coordinate and vertex shaders to produce the
         * 32-bit entry consisting of 2 16-bit fields with 12.4 signed fixed
         * point offsets from the viewport ccenter.
         */
        QUNIFORM_VIEWPORT_X_SCALE,
        QUNIFORM_VIEWPORT_Y_SCALE,
        /** @} */

        QUNIFORM_VIEWPORT_Z_OFFSET,
        QUNIFORM_VIEWPORT_Z_SCALE,

        QUNIFORM_USER_CLIP_PLANE,

        /**
         * A reference to a texture config parameter 0 uniform.
         *
         * This is a uniform implicitly loaded with a QPU_W_TMU* write, which
         * defines texture type, miplevels, and such.  It will be found as a
         * parameter to the first QOP_TEX_[STRB] instruction in a sequence.
         */
        QUNIFORM_TEXTURE_CONFIG_P0_0,
        QUNIFORM_TEXTURE_CONFIG_P0_1,
        QUNIFORM_TEXTURE_CONFIG_P0_2,
        QUNIFORM_TEXTURE_CONFIG_P0_3,
        QUNIFORM_TEXTURE_CONFIG_P0_4,
        QUNIFORM_TEXTURE_CONFIG_P0_5,
        QUNIFORM_TEXTURE_CONFIG_P0_6,
        QUNIFORM_TEXTURE_CONFIG_P0_7,
        QUNIFORM_TEXTURE_CONFIG_P0_8,
        QUNIFORM_TEXTURE_CONFIG_P0_9,
        QUNIFORM_TEXTURE_CONFIG_P0_10,
        QUNIFORM_TEXTURE_CONFIG_P0_11,
        QUNIFORM_TEXTURE_CONFIG_P0_12,
        QUNIFORM_TEXTURE_CONFIG_P0_13,
        QUNIFORM_TEXTURE_CONFIG_P0_14,
        QUNIFORM_TEXTURE_CONFIG_P0_15,
        QUNIFORM_TEXTURE_CONFIG_P0_16,
        QUNIFORM_TEXTURE_CONFIG_P0_17,
        QUNIFORM_TEXTURE_CONFIG_P0_18,
        QUNIFORM_TEXTURE_CONFIG_P0_19,
        QUNIFORM_TEXTURE_CONFIG_P0_20,
        QUNIFORM_TEXTURE_CONFIG_P0_21,
        QUNIFORM_TEXTURE_CONFIG_P0_22,
        QUNIFORM_TEXTURE_CONFIG_P0_23,
        QUNIFORM_TEXTURE_CONFIG_P0_24,
        QUNIFORM_TEXTURE_CONFIG_P0_25,
        QUNIFORM_TEXTURE_CONFIG_P0_26,
        QUNIFORM_TEXTURE_CONFIG_P0_27,
        QUNIFORM_TEXTURE_CONFIG_P0_28,
        QUNIFORM_TEXTURE_CONFIG_P0_29,
        QUNIFORM_TEXTURE_CONFIG_P0_30,
        QUNIFORM_TEXTURE_CONFIG_P0_31,
        QUNIFORM_TEXTURE_CONFIG_P0_32,

        /**
         * A reference to a texture config parameter 1 uniform.
         *
         * This is a uniform implicitly loaded with a QPU_W_TMU* write, which
         * defines texture width, height, filters, and wrap modes.  It will be
         * found as a parameter to the second QOP_TEX_[STRB] instruction in a
         * sequence.
         */
        QUNIFORM_TEXTURE_CONFIG_P1,

        QUNIFORM_TEXTURE_FIRST_LEVEL,

        QUNIFORM_TEXTURE_MSAA_ADDR,

        QUNIFORM_UBO_ADDR,

        QUNIFORM_TEXRECT_SCALE_X,
        QUNIFORM_TEXRECT_SCALE_Y,

        QUNIFORM_TEXTURE_BORDER_COLOR,

        QUNIFORM_STENCIL,

        QUNIFORM_ALPHA_REF,
        QUNIFORM_SAMPLE_MASK,
};

struct vc5_varying_slot {
        uint8_t slot;
        uint8_t swizzle;
};

struct vc5_compiler_ubo_range {
        /**
         * offset in bytes from the start of the ubo where this range is
         * uploaded.
         *
         * Only set once used is set.
         */
        uint32_t dst_offset;

        /**
         * offset in bytes from the start of the gallium uniforms where the
         * data comes from.
         */
        uint32_t src_offset;

        /** size in bytes of this ubo range */
        uint32_t size;

        /**
         * Set if this range is used by the shader for indirect uniforms
         * access.
         */
        bool used;
};

struct vc5_key {
        struct vc5_uncompiled_shader *shader_state;
        struct {
                uint8_t swizzle[4];
                uint8_t return_size;
                uint8_t return_channels;
                union {
                        struct {
                                unsigned compare_mode:1;
                                unsigned compare_func:3;
                                unsigned wrap_s:3;
                                unsigned wrap_t:3;
                        };
                        struct {
                                uint16_t msaa_width, msaa_height;
                        };
                };
        } tex[VC5_MAX_TEXTURE_SAMPLERS];
        uint8_t ucp_enables;
};

struct vc5_fs_key {
        struct vc5_key base;
        bool depth_enabled;
        bool is_points;
        bool is_lines;
        bool alpha_test;
        bool point_coord_upper_left;
        bool light_twoside;
        bool msaa;
        bool sample_coverage;
        bool sample_alpha_to_coverage;
        bool sample_alpha_to_one;
        bool clamp_color;
        bool swap_color_rb;
        uint8_t alpha_test_func;
        uint8_t logicop_func;
        uint32_t point_sprite_mask;

        struct pipe_rt_blend_state blend;
};

struct vc5_vs_key {
        struct vc5_key base;

        const struct vc5_fs_inputs *fs_inputs;
        bool is_coord;
        bool per_vertex_point_size;
        bool clamp_color;
};

/** A basic block of QIR intructions. */
struct qblock {
        struct list_head link;

        struct list_head instructions;
        struct list_head qpu_inst_list;

        struct set *predecessors;
        struct qblock *successors[2];

        int index;

        /* Instruction IPs for the first and last instruction of the block.
         * Set by vc5_qpu_schedule.c.
         */
        uint32_t start_qpu_ip;
        uint32_t end_qpu_ip;

        /* Instruction IP for the branch instruction of the block.  Set by
         * vc5_qpu_schedule.c.
         */
        uint32_t branch_qpu_ip;

        /** Offset within the uniform stream at the start of the block. */
        uint32_t start_uniform;

        /** @{ used by vc5_qir_live_variables.c */
        BITSET_WORD *def;
        BITSET_WORD *use;
        BITSET_WORD *live_in;
        BITSET_WORD *live_out;
        int start_ip, end_ip;
        /** @} */
};

/**
 * Compiler state saved across compiler invocations, for any expensive global
 * setup.
 */
struct vc5_compiler_state {
        struct ra_regs *regs;
        unsigned int reg_class[3];
};

struct vc5_compile {
        const struct v3d_chip *chip;
        nir_shader *s;
        nir_function_impl *impl;
        struct exec_list *cf_node_list;
        struct vc5_compiler_state *compiler_state;

        /**
         * Mapping from nir_register * or nir_ssa_def * to array of struct
         * qreg for the values.
         */
        struct hash_table *def_ht;

        /* For each temp, the instruction generating its value. */
        struct qinst **defs;
        uint32_t defs_array_size;

        /**
         * Inputs to the shader, arranged by TGSI declaration order.
         *
         * Not all fragment shader QFILE_VARY reads are present in this array.
         */
        struct qreg *inputs;
        struct qreg *outputs;
        bool msaa_per_sample_output;
        struct qreg color_reads[VC5_MAX_SAMPLES];
        struct qreg sample_colors[VC5_MAX_SAMPLES];
        uint32_t inputs_array_size;
        uint32_t outputs_array_size;
        uint32_t uniforms_array_size;

        /* Booleans for whether the corresponding QFILE_VARY[i] is
         * flat-shaded.  This doesn't count gl_FragColor flat-shading, which is
         * controlled by shader->color_inputs and rasterizer->flatshade in the
         * gallium driver.
         */
        BITSET_WORD flat_shade_flags[BITSET_WORDS(VC5_MAX_FS_INPUTS)];

        struct vc5_compiler_ubo_range *ubo_ranges;
        uint32_t ubo_ranges_array_size;
        /** Number of uniform areas declared in ubo_ranges. */
        uint32_t num_uniform_ranges;
        /** Number of uniform areas used for indirect addressed loads. */
        uint32_t num_ubo_ranges;
        uint32_t next_ubo_dst_offset;

        /* State for whether we're executing on each channel currently.  0 if
         * yes, otherwise a block number + 1 that the channel jumped to.
         */
        struct qreg execute;

        struct qreg line_x, point_x, point_y;

        /**
         * Instance ID, which comes in before the vertex attribute payload if
         * the shader record requests it.
         */
        struct qreg iid;

        /**
         * Vertex ID, which comes in before the vertex attribute payload
         * (after Instance ID) if the shader record requests it.
         */
        struct qreg vid;

        /** boolean (~0 -> true) if the fragment has been discarded. */
        struct qreg discard;
        struct qreg payload_FRAG_Z;
        struct qreg payload_FRAG_W;
        struct qreg payload_FRAG_W_CENTROID;

        uint8_t vattr_sizes[VC5_MAX_VS_INPUTS];
        uint32_t num_vpm_writes;

        /**
         * Array of the VARYING_SLOT_* of all FS QFILE_VARY reads.
         *
         * This includes those that aren't part of the VPM varyings, like
         * point/line coordinates.
         */
        struct vc5_varying_slot *input_slots;
        uint32_t num_input_slots;
        uint32_t input_slots_array_size;

        /**
         * An entry per outputs[] in the VS indicating what the VARYING_SLOT_*
         * of the output is.  Used to emit from the VS in the order that the
         * FS needs.
         */
        struct vc5_varying_slot *output_slots;

        struct pipe_shader_state *shader_state;
        struct vc5_key *key;
        struct vc5_fs_key *fs_key;
        struct vc5_vs_key *vs_key;

        uint32_t debug;

        /* Live ranges of temps. */
        int *temp_start, *temp_end;

        uint32_t *uniform_data;
        enum quniform_contents *uniform_contents;
        uint32_t uniform_array_size;
        uint32_t num_uniforms;
        uint32_t num_outputs;
        uint32_t num_texture_samples;
        uint32_t output_position_index;
        nir_variable *output_color_var;
        uint32_t output_point_size_index;
        uint32_t output_sample_mask_index;

        struct qreg undef;
        uint32_t num_temps;

        struct list_head blocks;
        int next_block_index;
        struct qblock *cur_block;
        struct qblock *loop_cont_block;
        struct qblock *loop_break_block;

        struct list_head qpu_inst_list;

        uint64_t *qpu_insts;
        uint32_t qpu_inst_count;
        uint32_t qpu_inst_size;
        uint32_t num_inputs;

        /**
         * Number of inputs from num_inputs remaining to be queued to the read
         * FIFO in the VS/CS.
         */
        uint32_t num_inputs_remaining;

        /* Number of inputs currently in the read FIFO for the VS/CS */
        uint32_t num_inputs_in_fifo;

        /** Next offset in the VPM to read from in the VS/CS */
        uint32_t vpm_read_offset;

        uint32_t program_id;
        uint32_t variant_id;

        /* Set to compile program in threaded FS mode, where SIG_THREAD_SWITCH
         * is used to hide texturing latency at the cost of limiting ourselves
         * to the bottom half of physical reg space.
         */
        bool fs_threaded;

        bool last_thrsw_at_top_level;

        bool failed;
};

/* Special nir_load_input intrinsic index for loading the current TLB
 * destination color.
 */
#define VC5_NIR_TLB_COLOR_READ_INPUT		2000000000

#define VC5_NIR_MS_MASK_OUTPUT			2000000000

extern const nir_shader_compiler_options vc5_nir_options;

struct vc5_compile *qir_compile_init(const struct v3d_chip *chip,
                                     struct vc5_compiler_state *compiler_state);
void vc5_optimize_nir(struct nir_shader *s);
struct vc5_compile *vc5_nir_shader_compile(const struct v3d_chip *chip,
                                           struct vc5_key *key, nir_shader *s,
                                           int debug);
void qir_compile_destroy(struct vc5_compile *c);
const char *qir_get_stage_name(struct vc5_compile *c);
struct qblock *qir_new_block(struct vc5_compile *c);
void qir_set_emit_block(struct vc5_compile *c, struct qblock *block);
void qir_link_blocks(struct qblock *predecessor, struct qblock *successor);
struct qblock *qir_entry_block(struct vc5_compile *c);
struct qblock *qir_exit_block(struct vc5_compile *c);
struct qinst *qir_inst(enum qop op, struct qreg dst,
                       struct qreg src0, struct qreg src1);
void qir_remove_instruction(struct vc5_compile *c, struct qinst *qinst);
struct qreg qir_uniform(struct vc5_compile *c,
                        enum quniform_contents contents,
                        uint32_t data);
void qir_schedule_instructions(struct vc5_compile *c);
void qir_reorder_uniforms(struct vc5_compile *c);
void qir_emit_uniform_stream_resets(struct vc5_compile *c);

struct qreg qir_emit_def(struct vc5_compile *c, struct qinst *inst);
struct qinst *qir_emit_nondef(struct vc5_compile *c, struct qinst *inst);

struct qreg qir_get_temp(struct vc5_compile *c);
void qir_calculate_live_intervals(struct vc5_compile *c);
bool qir_has_implicit_uniform(struct qinst *inst);
int qir_get_implicit_uniform_src(struct qinst *inst);
int qir_get_non_sideband_nsrc(struct qinst *inst);
int qir_get_nsrc(struct qinst *inst);
bool qir_reg_equals(struct qreg a, struct qreg b);
bool qir_has_side_effects(struct vc5_compile *c, struct qinst *inst);
bool qir_has_side_effect_reads(struct vc5_compile *c, struct qinst *inst);
bool qir_get_add_op(struct qinst *inst, enum v3d_qpu_add_op *op);
bool qir_get_mul_op(struct qinst *inst, enum v3d_qpu_mul_op *op);
bool qir_is_raw_mov(struct qinst *inst);
bool qir_is_tex(struct qinst *inst);
bool qir_is_float_input(struct qinst *inst);
bool qir_depends_on_flags(struct qinst *inst);
bool qir_writes_r3(struct qinst *inst);
bool qir_writes_r4(struct qinst *inst);
struct qreg qir_follow_movs(struct vc5_compile *c, struct qreg reg);
uint8_t qir_channels_written(struct qinst *inst);

void qir_dump(struct vc5_compile *c);
void qir_dump_inst(struct vc5_compile *c, struct qinst *inst);

void qir_validate(struct vc5_compile *c);

void qir_optimize(struct vc5_compile *c);
bool qir_opt_algebraic(struct vc5_compile *c);
bool qir_opt_constant_folding(struct vc5_compile *c);
bool qir_opt_copy_propagate(struct vc5_compile *c);
bool qir_opt_dead_code(struct vc5_compile *c);
bool qir_opt_peephole_sf(struct vc5_compile *c);
bool qir_opt_small_immediates(struct vc5_compile *c);
bool qir_opt_vpm(struct vc5_compile *c);
void vc5_nir_lower_blend(nir_shader *s, struct vc5_compile *c);
void vc5_nir_lower_io(nir_shader *s, struct vc5_compile *c);
void vc5_nir_lower_txf_ms(nir_shader *s, struct vc5_compile *c);
void qir_lower_uniforms(struct vc5_compile *c);

void vc5_qir_to_qpu(struct vc5_compile *c);
uint32_t qpu_schedule_instructions(struct vc5_compile *c);
struct qpu_reg *vc5_register_allocate(struct vc5_compile *c);

void qir_PF(struct vc5_compile *c, struct qreg src, enum v3d_qpu_pf pf);

static inline bool
quniform_contents_is_texture_p0(enum quniform_contents contents)
{
        return (contents >= QUNIFORM_TEXTURE_CONFIG_P0_0 &&
                contents < (QUNIFORM_TEXTURE_CONFIG_P0_0 +
                            VC5_MAX_TEXTURE_SAMPLERS));
}

static inline struct qreg
qir_uniform_ui(struct vc5_compile *c, uint32_t ui)
{
        return qir_uniform(c, QUNIFORM_CONSTANT, ui);
}

static inline struct qreg
qir_uniform_f(struct vc5_compile *c, float f)
{
        return qir_uniform(c, QUNIFORM_CONSTANT, fui(f));
}

#define QIR_ALU0(name)                                                   \
static inline struct qreg                                                \
qir_##name(struct vc5_compile *c)                                        \
{                                                                        \
        return qir_emit_def(c, qir_inst(QOP_##name, c->undef,            \
                                        c->undef, c->undef));            \
}                                                                        \
static inline struct qinst *                                             \
qir_##name##_dest(struct vc5_compile *c, struct qreg dest)               \
{                                                                        \
        return qir_emit_nondef(c, qir_inst(QOP_##name, dest,             \
                                           c->undef, c->undef));         \
}

#define QIR_ALU1(name)                                                   \
static inline struct qreg                                                \
qir_##name(struct vc5_compile *c, struct qreg a)                         \
{                                                                        \
        return qir_emit_def(c, qir_inst(QOP_##name, c->undef,            \
                                        a, c->undef));                   \
}                                                                        \
static inline struct qinst *                                             \
qir_##name##_dest(struct vc5_compile *c, struct qreg dest,               \
                  struct qreg a)                                         \
{                                                                        \
        return qir_emit_nondef(c, qir_inst(QOP_##name, dest, a,          \
                                           c->undef));                   \
}

#define QIR_ALU2(name)                                                   \
static inline struct qreg                                                \
qir_##name(struct vc5_compile *c, struct qreg a, struct qreg b)          \
{                                                                        \
        return qir_emit_def(c, qir_inst(QOP_##name, c->undef, a, b));    \
}                                                                        \
static inline struct qinst *                                             \
qir_##name##_dest(struct vc5_compile *c, struct qreg dest,               \
                  struct qreg a, struct qreg b)                          \
{                                                                        \
        return qir_emit_nondef(c, qir_inst(QOP_##name, dest, a, b));     \
}

#define QIR_NODST_1(name)                                               \
static inline struct qinst *                                            \
qir_##name(struct vc5_compile *c, struct qreg a)                        \
{                                                                       \
        return qir_emit_nondef(c, qir_inst(QOP_##name, c->undef,        \
                                           a, c->undef));               \
}

#define QIR_NODST_2(name)                                               \
static inline struct qinst *                                            \
qir_##name(struct vc5_compile *c, struct qreg a, struct qreg b)         \
{                                                                       \
        return qir_emit_nondef(c, qir_inst(QOP_##name, c->undef,        \
                                           a, b));                      \
}

#define QIR_PAYLOAD(name)                                                \
static inline struct qreg                                                \
qir_##name(struct vc5_compile *c)                                        \
{                                                                        \
        struct qreg *payload = &c->payload_##name;                       \
        if (payload->file != QFILE_NULL)                                 \
                return *payload;                                         \
        *payload = qir_get_temp(c);                                      \
        struct qinst *inst = qir_inst(QOP_##name, *payload,              \
                                      c->undef, c->undef);               \
        struct qblock *entry = qir_entry_block(c);                       \
        list_add(&inst->link, &entry->instructions);                     \
        c->defs[payload->index] = inst;                                  \
        return *payload;                                                 \
}

QIR_ALU2(FADD)
QIR_ALU2(VFPACK)
QIR_ALU2(FSUB)
QIR_ALU2(FMIN)
QIR_ALU2(FMAX)

QIR_ALU2(ADD)
QIR_ALU2(SUB)
QIR_ALU2(SHL)
QIR_ALU2(SHR)
QIR_ALU2(ASR)
QIR_ALU2(ROR)
QIR_ALU2(MIN)
QIR_ALU2(MAX)
QIR_ALU2(UMIN)
QIR_ALU2(UMAX)
QIR_ALU2(AND)
QIR_ALU2(OR)
QIR_ALU2(XOR)
QIR_ALU2(VADD)
QIR_ALU2(VSUB)
QIR_ALU1(NOT)
QIR_ALU1(NEG)
QIR_ALU1(FLAPUSH)
QIR_ALU1(FLBPUSH)
QIR_ALU1(FLBPOP)
QIR_ALU1(SETMSF)
QIR_ALU1(SETREVF)
QIR_ALU1(TIDX)
QIR_ALU1(EIDX)

QIR_ALU0(FXCD)
QIR_ALU0(XCD)
QIR_ALU0(FYCD)
QIR_ALU0(YCD)
QIR_ALU0(MSF)
QIR_ALU0(REVF)
QIR_ALU2(FCMP)
QIR_ALU2(VFMAX)

QIR_ALU1(FROUND)
QIR_ALU1(FTOIN)
QIR_ALU1(FTRUNC)
QIR_ALU1(FTOIZ)
QIR_ALU1(FFLOOR)
QIR_ALU1(FTOUZ)
QIR_ALU1(FCEIL)
QIR_ALU1(FTOC)

QIR_ALU1(FDX)
QIR_ALU1(FDY)

QIR_ALU1(ITOF)
QIR_ALU1(CLZ)
QIR_ALU1(UTOF)

QIR_ALU2(UMUL24)
QIR_ALU2(FMUL)
QIR_ALU2(SMUL24)
QIR_NODST_2(MULTOP)

QIR_ALU1(MOV)
QIR_ALU1(FMOV)

QIR_ALU1(RCP)
QIR_ALU1(RSQ)
QIR_ALU1(EXP2)
QIR_ALU1(LOG2)
QIR_ALU1(SIN)

QIR_NODST_1(VPMSETUP_READ)
QIR_NODST_1(VPMSETUP_WRITE)

QIR_ALU1(VARY_ADD_C)
QIR_ALU1(VARY_MOV_C)
QIR_NODST_2(TEX_DIRECT)
QIR_PAYLOAD(FRAG_Z)
QIR_PAYLOAD(FRAG_W)
QIR_PAYLOAD(FRAG_W_CENTROID)
QIR_ALU0(LDTMU)
QIR_ALU0(TLB_COLOR_READ)

static inline struct qreg
qir_SEL(struct vc5_compile *c, enum v3d_qpu_cond cond,
        struct qreg src0, struct qreg src1)
{
        struct qreg t = qir_get_temp(c);
        qir_MOV_dest(c, t, src1);
        qir_MOV_dest(c, t, src0)->cond.alu = cond;
        return t;
}

static inline struct qreg
qir_POW(struct vc5_compile *c, struct qreg x, struct qreg y)
{
        return qir_EXP2(c, qir_FMUL(c, y, qir_LOG2(c, x)));
}

static inline void
qir_VPM_WRITE(struct vc5_compile *c, struct qreg val)
{
        qir_MOV_dest(c, qir_reg(QFILE_VPM, 0), val);
}

static inline struct qreg
qir_LOAD_IMM(struct vc5_compile *c, uint32_t val)
{
        return qir_emit_def(c, qir_inst(QOP_LOAD_IMM, c->undef,
                                        qir_reg(QFILE_LOAD_IMM, val), c->undef));
}

static inline struct qreg
qir_LOAD_IMM_U2(struct vc5_compile *c, uint32_t val)
{
        return qir_emit_def(c, qir_inst(QOP_LOAD_IMM_U2, c->undef,
                                        qir_reg(QFILE_LOAD_IMM, val),
                                        c->undef));
}

static inline struct qreg
qir_LOAD_IMM_I2(struct vc5_compile *c, uint32_t val)
{
        return qir_emit_def(c, qir_inst(QOP_LOAD_IMM_I2, c->undef,
                                        qir_reg(QFILE_LOAD_IMM, val),
                                        c->undef));
}

static inline struct qinst *
qir_MOV_cond(struct vc5_compile *c, enum v3d_qpu_cond cond,
             struct qreg dest, struct qreg src)
{
        struct qinst *mov = qir_MOV_dest(c, dest, src);
        mov->cond.alu = cond;
        return mov;
}

static inline struct qinst *
qir_BRANCH(struct vc5_compile *c, enum v3d_qpu_cond cond)
{
        struct qinst *inst = qir_inst(QOP_BRANCH, c->undef, c->undef, c->undef);
        inst->cond.alu = cond;
        /* The actual uniform_data value will be set at scheduling time */
        inst->src[0] = qir_uniform_ui(c, 0);
        qir_emit_nondef(c, inst);
        return inst;
}

#define qir_for_each_block(block, c)                                    \
        list_for_each_entry(struct qblock, block, &c->blocks, link)

#define qir_for_each_block_rev(block, c)                                \
        list_for_each_entry_rev(struct qblock, block, &c->blocks, link)

/* Loop over the non-NULL members of the successors array. */
#define qir_for_each_successor(succ, block)                             \
        for (struct qblock *succ = block->successors[0];                \
             succ != NULL;                                              \
             succ = (succ == block->successors[1] ? NULL :              \
                     block->successors[1]))

#define qir_for_each_inst(inst, block)                                  \
        list_for_each_entry(struct qinst, inst, &block->instructions, link)

#define qir_for_each_inst_rev(inst, block)                                  \
        list_for_each_entry_rev(struct qinst, inst, &block->instructions, link)

#define qir_for_each_inst_safe(inst, block)                             \
        list_for_each_entry_safe(struct qinst, inst, &block->instructions, link)

#define qir_for_each_inst_inorder(inst, c)                              \
        qir_for_each_block(_block, c)                                   \
                qir_for_each_inst(inst, _block)

#endif /* VC5_COMPILER_H */
