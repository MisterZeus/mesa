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

#ifndef VC5_QPU_DEFINES_H
#define VC5_QPU_DEFINES_H

#ifndef QPU_MASK
#define QPU_MASK(high, low) ((((uint64_t)1<<((high)-(low)+1))-1)<<(low))
/* Using the GNU statement expression extension */
#define QPU_SET_FIELD(value, field)                                       \
        ({                                                                \
                uint64_t fieldval = (uint64_t)(value) << field ## _SHIFT; \
                assert((fieldval & ~ field ## _MASK) == 0);               \
                fieldval & field ## _MASK;                                \
         })

#define QPU_GET_FIELD(word, field) ((uint32_t)(((word)  & field ## _MASK) >> field ## _SHIFT))

#define QPU_UPDATE_FIELD(inst, value, field)                              \
        (((inst) & ~(field ## _MASK)) | QPU_SET_FIELD(value, field))
#endif /* QPU_MASK */

enum vc5_mux {
        VC5_MUX_R0,
        VC5_MUX_R1,
        VC5_MUX_R2,
        VC5_MUX_R3,
        VC5_MUX_R4,
        VC5_MUX_R5,
        VC5_MUX_A,
        VC5_MUX_B,
};

enum vc5_qpu_waddr {
        VC5_QPU_WADDR_R0 = 0,
        VC5_QPU_WADDR_R1 = 1,
        VC5_QPU_WADDR_R2 = 2,
        VC5_QPU_WADDR_R3 = 3,
        VC5_QPU_WADDR_R4 = 4,
        VC5_QPU_WADDR_R5 = 5,
        /* 6 is reserved, but note 3.2.2.8: "Result Writes" */
        VC5_QPU_WADDR_NOP = 6,
        VC5_QPU_WADDR_TLB = 7,
        VC5_QPU_WADDR_TLBU = 8,
        VC5_QPU_WADDR_TMU = 9,
        VC5_QPU_WADDR_TMUL = 10,
        VC5_QPU_WADDR_TMUD = 11,
        VC5_QPU_WADDR_TMUA = 12,
        VC5_QPU_WADDR_TMUAU = 13,
        VC5_QPU_WADDR_VPM = 14,
        VC5_QPU_WADDR_VPMU = 15,
        VC5_QPU_WADDR_SYNC = 16,
        VC5_QPU_WADDR_SYNCU = 17,
        /* reserved */
        VC5_QPU_WADDR_RECIP = 19,
        VC5_QPU_WADDR_RSQRT = 20,
        VC5_QPU_WADDR_EXP = 21,
        VC5_QPU_WADDR_LOG = 22,
        VC5_QPU_WADDR_SIN = 23,
        VC5_QPU_WADDR_RSQRT2 = 24,
};

#define VC5_QPU_OP_MUL_SHIFT                58
#define VC5_QPU_OP_MUL_MASK                 QPU_MASK(63, 58)

#define VC5_QPU_SIG_SHIFT                   53
#define VC5_QPU_SIG_MASK                    QPU_MASK(57, 53)
# define VC5_QPU_SIG_THRSW_BIT              0x1
# define VC5_QPU_SIG_LDUNIF_BIT             0x2
# define VC5_QPU_SIG_LDTMU_BIT              0x4
# define VC5_QPU_SIG_LDVARY_BIT             0x8

#define VC5_QPU_COND_SHIFT                  46
#define VC5_QPU_COND_MASK                   QPU_MASK(52, 46)

#define VC5_QPU_COND_IFA                    0
#define VC5_QPU_COND_IFB                    1
#define VC5_QPU_COND_IFNA                   2
#define VC5_QPU_COND_IFNB                   3

#define VC5_QPU_MM                          QPU_MASK(45, 45)
#define VC5_QPU_MA                          QPU_MASK(44, 44)

#define VC5_QPU_WADDR_M_SHIFT               38
#define VC5_QPU_WADDR_M_MASK                QPU_MASK(43, 38)

#define VC5_QPU_BRANCH_ADDR_LOW_SHIFT       35
#define VC5_QPU_BRANCH_ADDR_LOW_MASK        QPU_MASK(55, 35)

#define VC5_QPU_WADDR_A_SHIFT               32
#define VC5_QPU_WADDR_A_MASK                QPU_MASK(37, 32)

#define VC5_QPU_BRANCH_COND_SHIFT           32
#define VC5_QPU_BRANCH_COND_MASK            QPU_MASK(34, 32)

#define VC5_QPU_BRANCH_ADDR_HIGH_SHIFT      24
#define VC5_QPU_BRANCH_ADDR_HIGH_MASK       QPU_MASK(31, 24)

#define VC5_QPU_OP_ADD_SHIFT                24
#define VC5_QPU_OP_ADD_MASK                 QPU_MASK(31, 24)

#define VC5_QPU_MUL_B_SHIFT                 21
#define VC5_QPU_MUL_B_MASK                  QPU_MASK(23, 21)

#define VC5_QPU_BRANCH_MSFIGN_SHIFT         21
#define VC5_QPU_BRANCH_MSFIGN_MASK          QPU_MASK(22, 21)

#define VC5_QPU_MUL_A_SHIFT                 18
#define VC5_QPU_MUL_A_MASK                  QPU_MASK(20, 18)

#define VC5_QPU_ADD_B_SHIFT                 15
#define VC5_QPU_ADD_B_MASK                  QPU_MASK(17, 15)

#define VC5_QPU_BRANCH_BDU_SHIFT            15
#define VC5_QPU_BRANCH_BDU_MASK             QPU_MASK(17, 15)

#define VC5_QPU_BRANCH_UB                   QPU_MASK(14, 14)

#define VC5_QPU_ADD_A_SHIFT                 12
#define VC5_QPU_ADD_A_MASK                  QPU_MASK(14, 12)

#define VC5_QPU_BRANCH_BDI_SHIFT            12
#define VC5_QPU_BRANCH_BDI_MASK             QPU_MASK(13, 12)

#define VC5_QPU_RADDR_A_SHIFT               6
#define VC5_QPU_RADDR_A_MASK                QPU_MASK(11, 6)

#define VC5_QPU_RADDR_B_SHIFT               0
#define VC5_QPU_RADDR_B_MASK                QPU_MASK(5, 0)

#endif
