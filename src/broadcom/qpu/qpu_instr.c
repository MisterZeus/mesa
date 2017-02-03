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

#include <stdlib.h>
#include "util/macros.h"
#include "qpu_instr.h"

const char *
v3d_qpu_add_op_name(enum v3d_qpu_add_op op)
{
        static const char *op_names[] = {
                [V3D_QPU_A_FADD] = "fadd",
                [V3D_QPU_A_FADDNF] = "faddnf",
                [V3D_QPU_A_VFPACK] = "vfpack",
                [V3D_QPU_A_ADD] = "add",
                [V3D_QPU_A_SUB] = "sub",
                [V3D_QPU_A_FSUB] = "fsub",
                [V3D_QPU_A_MIN] = "min",
                [V3D_QPU_A_MAX] = "max",
                [V3D_QPU_A_UMIN] = "umin",
                [V3D_QPU_A_UMAX] = "umax",
                [V3D_QPU_A_SHL] = "shl",
                [V3D_QPU_A_SHR] = "shr",
                [V3D_QPU_A_ASR] = "asr",
                [V3D_QPU_A_ROR] = "ror",
                [V3D_QPU_A_FMIN] = "fmin",
                [V3D_QPU_A_FMAX] = "fmax",
                [V3D_QPU_A_VFMIN] = "vfmin",
                [V3D_QPU_A_AND] = "and",
                [V3D_QPU_A_OR] = "or",
                [V3D_QPU_A_XOR] = "xor",
                [V3D_QPU_A_VADD] = "vadd",
                [V3D_QPU_A_VSUB] = "vsub",
                [V3D_QPU_A_NOT] = "not",
                [V3D_QPU_A_NEG] = "neg",
                [V3D_QPU_A_FLAPUSH] = "flapush",
                [V3D_QPU_A_FLBPUSH] = "flbpush",
                [V3D_QPU_A_FLBPOP] = "flbpop",
                [V3D_QPU_A_SETMSF] = "setmsf",
                [V3D_QPU_A_SETREVF] = "setrevf",
                [V3D_QPU_A_NOP] = "nop",
                [V3D_QPU_A_TIDX] = "tidx",
                [V3D_QPU_A_EIDX] = "eidx",
                [V3D_QPU_A_LR] = "lr",
                [V3D_QPU_A_VFLA] = "vfla",
                [V3D_QPU_A_VFLNA] = "vflna",
                [V3D_QPU_A_VFLB] = "vflb",
                [V3D_QPU_A_VFLNB] = "vflnb",
                [V3D_QPU_A_FXCD] = "fxcd",
                [V3D_QPU_A_XCD] = "xcd",
                [V3D_QPU_A_FYCD] = "fycd",
                [V3D_QPU_A_YCD] = "ycd",
                [V3D_QPU_A_MSF] = "msf",
                [V3D_QPU_A_REVF] = "revf",
                [V3D_QPU_A_VDWWT] = "vdwwt",
                [V3D_QPU_A_IID] = "iid",
                [V3D_QPU_A_SAMPID] = "sampid",
                [V3D_QPU_A_PATCHID] = "patchid",
                [V3D_QPU_A_TMUWT] = "tmuwt",
                [V3D_QPU_A_VPMSETUP] = "vpmsetup",
                [V3D_QPU_A_VPMWT] = "vpmwt",
                [V3D_QPU_A_LDVPMV] = "ldvpmv",
                [V3D_QPU_A_LDVPMD] = "ldvpmd",
                [V3D_QPU_A_LDVPMP] = "ldvpmp",
                [V3D_QPU_A_LDVPMG] = "ldvpmg",
                [V3D_QPU_A_FCMP] = "fcmp",
                [V3D_QPU_A_VFMAX] = "vfmax",
                [V3D_QPU_A_FROUND] = "fround",
                [V3D_QPU_A_FTOIN] = "ftoin",
                [V3D_QPU_A_FTRUNC] = "ftrunc",
                [V3D_QPU_A_FTOIZ] = "ftoiz",
                [V3D_QPU_A_FFLOOR] = "ffloor",
                [V3D_QPU_A_FTOUZ] = "ftouz",
                [V3D_QPU_A_FCEIL] = "fceil",
                [V3D_QPU_A_FTOC] = "ftoc",
                [V3D_QPU_A_FDX] = "fdx",
                [V3D_QPU_A_FDY] = "fdy",
                [V3D_QPU_A_STVPMV] = "stvpmv",
                [V3D_QPU_A_STVPMD] = "stvpmd",
                [V3D_QPU_A_STVPMP] = "stvpmp",
                [V3D_QPU_A_ITOF] = "itof",
                [V3D_QPU_A_CLZ] = "clz",
                [V3D_QPU_A_UTOF] = "utof",
        };

        if (op >= ARRAY_SIZE(op_names))
                return NULL;

        return op_names[op];
}

const char *
v3d_qpu_mul_op_name(enum v3d_qpu_mul_op op)
{
        static const char *op_names[] = {
                [V3D_QPU_M_ADD] = "add",
                [V3D_QPU_M_SUB] = "sub",
                [V3D_QPU_M_UMUL24] = "umul24",
                [V3D_QPU_M_VFMUL] = "vfmul",
                [V3D_QPU_M_SMUL24] = "smul24",
                [V3D_QPU_M_MULTOP] = "multop",
                [V3D_QPU_M_FMOV] = "fmov",
                [V3D_QPU_M_MOV] = "mov",
                [V3D_QPU_M_NOP] = "nop",
                [V3D_QPU_M_FMUL] = "fmul",
        };

        if (op >= ARRAY_SIZE(op_names))
                return NULL;

        return op_names[op];
}

const char *
v3d_qpu_cond_name(enum v3d_qpu_cond cond)
{
        switch (cond) {
        case V3D_QPU_COND_NONE:
                return "";
        case V3D_QPU_COND_IFA:
                return ".ifa";
        case V3D_QPU_COND_IFB:
                return ".ifb";
        case V3D_QPU_COND_IFNA:
                return ".ifna";
        case V3D_QPU_COND_IFNB:
                return ".ifnb";
        default:
                unreachable("bad cond value");
        }
}

const char *
v3d_qpu_branch_cond_name(enum v3d_qpu_branch_cond cond)
{
        switch (cond) {
        case V3D_QPU_BRANCH_COND_ALWAYS:
                return "";
        case V3D_QPU_BRANCH_COND_A0:
                return ".a0";
        case V3D_QPU_BRANCH_COND_NA0:
                return ".na0";
        case V3D_QPU_BRANCH_COND_ALLA:
                return ".alla";
        case V3D_QPU_BRANCH_COND_ANYNA:
                return ".anyna";
        case V3D_QPU_BRANCH_COND_ANYA:
                return ".anya";
        case V3D_QPU_BRANCH_COND_ALLNA:
                return ".allna";
        default:
                unreachable("bad branch cond value");
        }
}

const char *
v3d_qpu_msfign_name(enum v3d_qpu_msfign msfign)
{
        switch (msfign) {
        case V3D_QPU_MSFIGN_NONE:
                return "";
        case V3D_QPU_MSFIGN_P:
                return "p";
        case V3D_QPU_MSFIGN_Q:
                return "q";
        default:
                unreachable("bad branch cond value");
        }
}

const char *
v3d_qpu_pf_name(enum v3d_qpu_pf pf)
{
        switch (pf) {
        case V3D_QPU_PF_NONE:
                return "";
        case V3D_QPU_PF_PUSHZ:
                return ".pushz";
        case V3D_QPU_PF_PUSHN:
                return ".pushn";
        case V3D_QPU_PF_PUSHC:
                return ".pushc";
        default:
                unreachable("bad pf value");
        }
}

const char *
v3d_qpu_uf_name(enum v3d_qpu_uf uf)
{
        switch (uf) {
        case V3D_QPU_UF_NONE:
                return "";
        case V3D_QPU_UF_ANDZ:
                return ".andz";
        case V3D_QPU_UF_ANDNZ:
                return ".andnz";
        case V3D_QPU_UF_NORZ:
                return ".norz";
        case V3D_QPU_UF_NORNZ:
                return ".nornz";
        case V3D_QPU_UF_ANDN:
                return ".andn";
        case V3D_QPU_UF_ANDNN:
                return ".andnn";
        case V3D_QPU_UF_NORN:
                return ".norn";
        case V3D_QPU_UF_NORNN:
                return ".nornn";
        case V3D_QPU_UF_ANDC:
                return ".andc";
        case V3D_QPU_UF_ANDNC:
                return ".andnc";
        case V3D_QPU_UF_NORC:
                return ".norc";
        case V3D_QPU_UF_NORNC:
                return ".nornc";
        default:
                unreachable("bad pf value");
        }
}

const char *
v3d_qpu_pack_name(enum v3d_qpu_output_pack pack)
{
        switch (pack) {
        case V3D_QPU_PACK_NONE:
                return "";
        case V3D_QPU_PACK_L:
                return ".l";
        case V3D_QPU_PACK_H:
                return ".h";
        default:
                unreachable("bad pack value");
        }
}

const char *
v3d_qpu_unpack_name(enum v3d_qpu_input_unpack unpack)
{
        switch (unpack) {
        case V3D_QPU_UNPACK_NONE:
                return "";
        case V3D_QPU_UNPACK_L:
                return ".l";
        case V3D_QPU_UNPACK_H:
                return ".h";
        case V3D_QPU_UNPACK_ABS:
                return ".abs";
        default:
                unreachable("bad unpack value");
        }
}

#define D	1
#define A	2
#define B	4
static const uint8_t add_op_args[] = {
        [V3D_QPU_A_FADD] = D | A | B,
        [V3D_QPU_A_FADDNF] = D | A | B,
        [V3D_QPU_A_VFPACK] = D | A | B,
        [V3D_QPU_A_ADD] = D | A | B,
        [V3D_QPU_A_VFPACK] = D | A | B,
        [V3D_QPU_A_SUB] = D | A | B,
        [V3D_QPU_A_VFPACK] = D | A | B,
        [V3D_QPU_A_FSUB] = D | A | B,
        [V3D_QPU_A_MIN] = D | A | B,
        [V3D_QPU_A_MAX] = D | A | B,
        [V3D_QPU_A_UMIN] = D | A | B,
        [V3D_QPU_A_UMAX] = D | A | B,
        [V3D_QPU_A_SHL] = D | A | B,
        [V3D_QPU_A_SHR] = D | A | B,
        [V3D_QPU_A_ASR] = D | A | B,
        [V3D_QPU_A_ROR] = D | A | B,
        [V3D_QPU_A_FMIN] = D | A | B,
        [V3D_QPU_A_FMAX] = D | A | B,
        [V3D_QPU_A_VFMIN] = D | A | B,

        [V3D_QPU_A_AND] = D | A | B,
        [V3D_QPU_A_OR] = D | A | B,
        [V3D_QPU_A_XOR] = D | A | B,

        [V3D_QPU_A_VADD] = D | A | B,
        [V3D_QPU_A_VSUB] = D | A | B,
        [V3D_QPU_A_NOT] = D | A,
        [V3D_QPU_A_NEG] = D | A,
        [V3D_QPU_A_FLAPUSH] = D | A,
        [V3D_QPU_A_FLBPUSH] = D | A,
        [V3D_QPU_A_FLBPOP] = D | A,
        [V3D_QPU_A_SETMSF] = D | A,
        [V3D_QPU_A_SETREVF] = D | A,
        [V3D_QPU_A_NOP] = 0,
        [V3D_QPU_A_TIDX] = D,
        [V3D_QPU_A_EIDX] = D,
        [V3D_QPU_A_LR] = D,
        [V3D_QPU_A_VFLA] = D,
        [V3D_QPU_A_VFLNA] = D,
        [V3D_QPU_A_VFLB] = D,
        [V3D_QPU_A_VFLNB] = D,

        [V3D_QPU_A_FXCD] = D,
        [V3D_QPU_A_XCD] = D,
        [V3D_QPU_A_FYCD] = D,
        [V3D_QPU_A_YCD] = D,

        [V3D_QPU_A_MSF] = D,
        [V3D_QPU_A_REVF] = D,
        [V3D_QPU_A_VDWWT] = D,
        [V3D_QPU_A_IID] = D,
        [V3D_QPU_A_SAMPID] = D,
        [V3D_QPU_A_PATCHID] = D,
        [V3D_QPU_A_TMUWT] = D,
        [V3D_QPU_A_VPMWT] = D,

        [V3D_QPU_A_VPMSETUP] = D | A,

        [V3D_QPU_A_LDVPMV] = D | A,
        [V3D_QPU_A_LDVPMD] = D | A,
        [V3D_QPU_A_LDVPMP] = D | A,
        [V3D_QPU_A_LDVPMG] = D | A | B,

        /* FIXME: MOVABSNEG */

        [V3D_QPU_A_FCMP] = D | A | B,
        [V3D_QPU_A_VFMAX] = D | A | B,

        [V3D_QPU_A_FROUND] = D | A,
        [V3D_QPU_A_FTOIN] = D | A,
        [V3D_QPU_A_FTRUNC] = D | A,
        [V3D_QPU_A_FTOIZ] = D | A,
        [V3D_QPU_A_FFLOOR] = D | A,
        [V3D_QPU_A_FTOUZ] = D | A,
        [V3D_QPU_A_FCEIL] = D | A,
        [V3D_QPU_A_FTOC] = D | A,

        [V3D_QPU_A_FDX] = D | A,
        [V3D_QPU_A_FDY] = D | A,

        [V3D_QPU_A_STVPMV] = A | B,
        [V3D_QPU_A_STVPMD] = A | B,
        [V3D_QPU_A_STVPMP] = A | B,

        [V3D_QPU_A_ITOF] = D | A,
        [V3D_QPU_A_CLZ] = D | A,
        [V3D_QPU_A_UTOF] = D | A,
};

static const uint8_t mul_op_args[] = {
        [V3D_QPU_M_ADD] = D | A | B,
        [V3D_QPU_M_SUB] = D | A | B,
        [V3D_QPU_M_UMUL24] = D | A | B,
        [V3D_QPU_M_VFMUL] = D | A | B,
        [V3D_QPU_M_SMUL24] = D | A | B,
        [V3D_QPU_M_MULTOP] = D | A | B,
        [V3D_QPU_M_FMOV] = D | A,
        [V3D_QPU_M_NOP] = 0,
        [V3D_QPU_M_MOV] = D | A,
        [V3D_QPU_M_FMUL] = D | A | B,
};

bool
v3d_qpu_add_op_has_dst(enum v3d_qpu_add_op op)
{
        assert(op < ARRAY_SIZE(add_op_args));

        return add_op_args[op] & D;
}

bool
v3d_qpu_mul_op_has_dst(enum v3d_qpu_mul_op op)
{
        assert(op < ARRAY_SIZE(mul_op_args));

        return mul_op_args[op] & D;
}

int
v3d_qpu_add_op_num_src(enum v3d_qpu_add_op op)
{
        assert(op < ARRAY_SIZE(add_op_args));

        uint8_t args = add_op_args[op];
        if (args & B)
                return 2;
        else if (args & A)
                return 1;
        else
                return 0;
}

int
v3d_qpu_mul_op_num_src(enum v3d_qpu_mul_op op)
{
        assert(op < ARRAY_SIZE(mul_op_args));

        uint8_t args = mul_op_args[op];
        if (args & B)
                return 2;
        else if (args & A)
                return 1;
        else
                return 0;
}
