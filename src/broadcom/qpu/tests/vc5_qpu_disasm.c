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

#include <stdio.h>
#include <string.h>
#include "util/macros.h"
#include "v3d_chip.h"
#include "qpu/vc5_qpu_disasm.h"
#include "qpu/qpu_instr.h"

static const struct {
        int ver;
        uint64_t inst;
        const char *expected;
} tests[] = {
        { 33, 0x3d003186bb800000ull, "nop                  ; nop               ; ldvary" },
        { 33, 0x3c20318105829000ull, "fadd  r1, r1, r5     ; nop               ; thrsw" },
        { 33, 0x3c00318735808000ull, "vfpack  tlb, r0, r1  ; nop" },
        { 33, 0x3c403186bb81d000ull, "vpmsetup  -, r5      ; nop               ; ldunif" },
        { 33, 0x3f003186bb800000ull, "nop                  ; nop               ; ldvpm" },
        { 33, 0x3c002380b6edb000ull, "or  rf0, r3, r3      ; mov  vpm, r3" },
        { 33, 0x57403006bbb80000ull, "nop                  ; fmul  r0, rf0, r5 ; ldvpm; ldunif" },
        { 33, 0x02000006002034c0ull, "b.anyap  rf19" },
        { 33, 0x02679356b4201000ull, "b.anyap  -1268280496" },
        { 33, 0x02b76a2dd0400000ull, "b.anynaq  zero_addr+0xd0b76a28" },
        { 33, 0x0200000500402000ull, "b.anynaq  lri" },
        { 33, 0x0216fe167301c8c0ull, "bu.anya  zero_addr+0x7316fe10, rf35" },
        { 33, 0x020000050040e000ull, "bu.anynaq  lri, r:unif" },
        { 33, 0x0200000300006000ull, "bu.na0  lri, a:unif" },
};

static void
swap_mux(uint8_t *a, uint8_t *b)
{
        uint8_t t = *a;
        *a = *b;
        *b = t;
}

static void
swap_pack(enum v3d_qpu_input_unpack *a, enum v3d_qpu_input_unpack *b)
{
        enum v3d_qpu_input_unpack t = *a;
        *a = *b;
        *b = t;
}

int
main(int argc, char **argv)
{
        struct v3d_chip chip = { };
        int retval = 0;

        for (int i = 0; i < ARRAY_SIZE(tests); i++) {
                chip.ver = tests[i].ver;

                printf("Testing v%d.%d 0x%016llx... ", chip.ver / 10, chip.ver % 10,
                       (long long)tests[i].inst);

                const char *disasm_output = vc5_qpu_disasm(&chip, tests[i].inst);

                if (strcmp(disasm_output, tests[i].expected) != 0) {
                        printf("FAIL\n");
                        printf("  Expected: \"%s\"\n", tests[i].expected);
                        printf("  Got:      \"%s\"\n", disasm_output);
                        retval = 1;
                        continue;
                }

                struct v3d_qpu_instr instr;
                if (!v3d_qpu_instr_unpack(&chip, tests[i].inst, &instr)) {
                        printf("FAIL (unpack) %s\n", tests[i].expected);
                        retval = 1;
                        continue;
                }

                if (instr.type == V3D_QPU_INSTR_TYPE_ALU) {
                        switch (instr.alu.add.op) {
                        case V3D_QPU_A_FADD:
                        case V3D_QPU_A_FADDNF:
                        case V3D_QPU_A_FMIN:
                        case V3D_QPU_A_FMAX:
                                /* Swap the operands to be sure that we test
                                 * how the QPUs distinguish between these ops.
                                 */
                                swap_mux(&instr.alu.add.a,
                                         &instr.alu.add.b);
                                swap_pack(&instr.alu.add.a_unpack,
                                          &instr.alu.add.b_unpack);
                        default:
                                break;
                        }
                }

                uint64_t repack;
                if (!v3d_qpu_instr_pack(&chip, &instr, &repack)) {
                        printf("FAIL (pack) %s\n", tests[i].expected);
                        retval = 1;
                        continue;
                }

                if (repack != tests[i].inst) {
                        printf("FAIL (repack) 0x%016llx\n", (long long)repack);
                        printf("  Expected: \"%s\"\n", tests[i].expected);
                        const char *redisasm = vc5_qpu_disasm(&chip, repack);
                        printf("  Got:      \"%s\"\n", redisasm);
                        retval = 1;
                }

                printf("PASS\n");
        }

        return retval;
}
