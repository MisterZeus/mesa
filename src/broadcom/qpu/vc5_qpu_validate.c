
/*
 * Copyright © 2014 Broadcom
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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "qpu_instr.h"
#include "vc5_qpu_defines.h"
#include "vc5_qpu_disasm.h"

struct vc5_qpu_validate_state {
        const struct v3d_chip *chip;
        uint64_t *instrs;
        int ip;
};

static void
fail_instr(struct vc5_qpu_validate_state *state, const char *msg)
{
        fprintf(stderr, "vc5_qpu_validate at ip %d: %s: ", state->ip, msg);
        vc5_qpu_disasm(state->chip, state->instrs[state->ip]);
        fprintf(stderr, "\n");
        abort();
}

static bool
instr_reads(struct v3d_qpu_instr *instr, uint8_t mux)
{
        return (instr->type == V3D_QPU_INSTR_TYPE_ALU &&
                (instr->alu.add.a == mux ||
                 instr->alu.add.b == mux ||
                 instr->alu.mul.a == mux ||
                 instr->alu.mul.b == mux));
}

/**
 * Checks for the instruction restrictions from page 37 ("Summary of
 * Instruction Restrictions").
 */
void
vc5_qpu_validate(const struct v3d_chip *chip, uint64_t *instrs, int num_inst)
{
        int last_sfu_write = -2;

        /* We don't want to do validation in release builds, but we want to
         * keep compiling the validation code to make sure it doesn't get
         * broken.
         */
#ifndef DEBUG
        return;
#endif

        struct vc5_qpu_validate_state state = {
                .chip = chip,
                .instrs = instrs,
        };

        struct v3d_qpu_instr prev_instr;
        for (state.ip = 0; state.ip < num_inst; state.ip++) {
                struct v3d_qpu_instr instr;
                bool ok = v3d_qpu_instr_unpack(chip, instrs[state.ip], &instr);
                assert(ok); (void)ok;

                /* LDVARY writes r5 two instructions later and LDUNIF writes
                 * r5 one instruction later, which is illegal to have
                 * together.
                 */
                if (state.ip > 0 && instr.sig.ldunif && prev_instr.sig.ldvary) {
                        fail_instr(&state, "LDUNIF after a LDVARY");
                }

                /* SFU r4 results come back two instructions later.  No doing
                 * r4 read/writes or other SFU lookups until it's done.
                 */
                if (state.ip - last_sfu_write < 2) {
                        if (instr_reads(&instr, VC5_MUX_R4)) {
                                fail_instr(&state,
                                           "R4 read too soon after SFU");
                        }
                }

                prev_instr = instr;
        }
}
