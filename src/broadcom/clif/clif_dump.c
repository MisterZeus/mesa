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
#include <stdlib.h>
#include <string.h>
#include "clif_dump.h"
#include "util/list.h"
#include "util/ralloc.h"

#define CLIF_DUMP

static uint32_t
clif_unpack_address(const uint8_t *restrict cl, uint32_t start, uint32_t end,
                   uint32_t mask_out);

#define __gen_user_data void
#define __gen_address_type uint32_t
#define __gen_address_offset(reloc) (*reloc)
#define __gen_emit_reloc(cl, reloc)
#define __gen_unpack_address clif_unpack_address

enum reloc_worklist_type {
        reloc_gl_shader_state,
};

struct reloc_worklist_entry {
        struct list_head link;

        enum reloc_worklist_type type;
        uint32_t addr;

        union {
                struct {
                        uint32_t num_attrs;
                } shader_state;
        };
};

struct clif_dump {
        const struct v3d_chip *chip;
        void (*output)(void *data, const char *output);
        bool (*lookup_vaddr)(void *data, uint32_t addr, void **vaddr);
        /* Opaque data from the caller that is passed to the callbacks. */
        void *data;

        /* List of struct reloc_worklist_entry */
        struct list_head worklist;
};

static void
out(struct clif_dump *clif, const char *fmt, ...)
{
        char *msg;
        va_list args;

        va_start(args, fmt);
        if (vasprintf(&msg, fmt, args) > 0) {
                clif->output(clif->data, msg);
                free(msg);
        }
        va_end(args);
}

#include "broadcom/cle/v3d_packet_v33_pack.h"

static uint32_t
clif_unpack_address(const uint8_t *restrict cl, uint32_t start, uint32_t end,
                   uint32_t mask_out)
{
        return __gen_unpack_uint(cl, start, end) & ~mask_out;
}

static struct reloc_worklist_entry *
clif_dump_add_address_to_worklist(struct clif_dump *clif,
                                  enum reloc_worklist_type type,
                                  uint32_t addr)
{
        struct reloc_worklist_entry *entry =
                rzalloc(clif, struct reloc_worklist_entry);
        if (!entry)
                return NULL;

        entry->type = type;
        entry->addr = addr;

        list_addtail(&entry->link, &clif->worklist);

        return entry;
}

struct clif_dump *
clif_dump_init(const struct v3d_chip *chip,
               bool (*lookup_vaddr)(void *data, uint32_t addr, void **vaddr),
               void (*output_callback)(void *data, const char *output),
               void *data)
{
        struct clif_dump *clif = rzalloc(NULL, struct clif_dump);

        clif->chip = chip;
        clif->lookup_vaddr = lookup_vaddr;
        clif->output = output_callback;
        clif->data = data;

        list_inithead(&clif->worklist);

        return clif;
}

void
clif_dump_destroy(struct clif_dump *clif)
{
        ralloc_free(clif);
}

#define out_uint(_clif, field) out(_clif, "    /* %s = */ %u\n",        \
                            #field,  values-> field);

static bool
clif_dump_packet(struct clif_dump *clif, uint32_t offset, const uint8_t *cl,
                 uint32_t *size)
{
        out(clif, "/* addr %d */\n", offset);

#define HANDLE_PACKET_INNER(name)                               \
        struct V3D33_ ## name values;                           \
        V3D33_ ## name ## _unpack(cl, &values);                 \
        V3D33_ ## name ## _clif_dump(clif, &values);            \
        *size = V3D33_ ##name ## _length;                       \

#define HANDLE_PACKET(name)                             \
        case V3D33_ ## name ## _opcode: {               \
                HANDLE_PACKET_INNER(name);              \
                return true;                            \
        }

        switch (*cl) {
                HANDLE_PACKET(NOP);
                HANDLE_PACKET(FLUSH);
                HANDLE_PACKET(FLUSH_ALL_STATE);
                HANDLE_PACKET(START_TILE_BINNING);

                HANDLE_PACKET(INCREMENT_SEMAPHORE);
                HANDLE_PACKET(WAIT_ON_SEMAPHORE);
                HANDLE_PACKET(WAIT_FOR_PREVIOUS_FRAME);

                HANDLE_PACKET(ENABLE_Z_ONLY_RENDERING);
                HANDLE_PACKET(DISABLE_Z_ONLY_RENDERING);
                HANDLE_PACKET(END_OF_Z_ONLY_RENDERING_IN_FRAME);
                HANDLE_PACKET(END_OF_RENDERING);
                HANDLE_PACKET(WAIT_FOR_TRANSFORM_FEEDBACK);

                HANDLE_PACKET(BRANCH_TO_AUTO_CHAINED_SUB_LIST);
                HANDLE_PACKET(BRANCH);
                HANDLE_PACKET(BRANCH_TO_SUB_LIST);
                HANDLE_PACKET(RETURN_FROM_SUB_LIST);

                HANDLE_PACKET(FLUSH_VCD_CACHE);

                HANDLE_PACKET(STORE_MULTI_SAMPLE_RESOLVED_TILE_COLOR_BUFFER);
                HANDLE_PACKET(STORE_MULTI_SAMPLE_RESOLVED_TILE_COLOR_BUFFER_EXTENDED);
                HANDLE_PACKET(RELOAD_TILE_COLOUR_BUFFER);
                HANDLE_PACKET(STORE_TILE_BUFFER_GENERAL);

                HANDLE_PACKET(VERTEX_ARRAY_PRIMITIVES);
                HANDLE_PACKET(VERTEX_ARRAY_INSTANCED_PRIMITIVES);
                HANDLE_PACKET(INDEXED_PRIMITIVE_LIST);
                HANDLE_PACKET(INDEXED_INSTANCED_PRIMITIVE_LIST);
                HANDLE_PACKET(PRIMITIVE_LIST_FORMAT);
                HANDLE_PACKET(STENCIL_CONFIG);
                HANDLE_PACKET(BLEND_CONFIG);
                HANDLE_PACKET(BLEND_CONSTANT_COLOUR);
                HANDLE_PACKET(COLOUR_WRITE_MASKS);

        case V3D33_GL_SHADER_STATE_opcode: {
                HANDLE_PACKET_INNER(GL_SHADER_STATE);
                struct reloc_worklist_entry *reloc =
                        clif_dump_add_address_to_worklist(clif,
                                                          reloc_gl_shader_state,
                                                          values.address);
                if (reloc) {
                        reloc->shader_state.num_attrs =
                                values.number_of_attribute_arrays;
                }
                return true;
        }

                HANDLE_PACKET(CONFIGURATION_BITS);
                HANDLE_PACKET(ZERO_ALL_FLAT_SHADE_FLAGS);
                HANDLE_PACKET(FLAT_SHADE_FLAGS);
                HANDLE_PACKET(POINT_SIZE);
                HANDLE_PACKET(LINE_WIDTH);
                HANDLE_PACKET(DEPTH_OFFSET);
                HANDLE_PACKET(CLIP_WINDOW);
                HANDLE_PACKET(VIEWPORT_OFFSET);
                HANDLE_PACKET(CLIPPER_XY_SCALING);
                HANDLE_PACKET(CLIPPER_Z_SCALE_AND_OFFSET);
                HANDLE_PACKET(CLIPPER_Z_MIN_MAX_CLIPPING_PLANES);
                HANDLE_PACKET(TILE_COORDINATES);
                HANDLE_PACKET(TILE_COORDINATES_IMPLICIT);
                HANDLE_PACKET(TILE_LIST_INITIAL_BLOCK_SIZE);

        case V3D33_TILE_BINNING_MODE_CONFIGURATION_opcode: {
                struct V3D33_TILE_BINNING_MODE_CONFIGURATION common;
                V3D33_TILE_BINNING_MODE_CONFIGURATION_unpack(cl, &common);
                if (common.sub_id == 0) {
                        struct V3D33_TILE_BINNING_MODE_CONFIGURATION_PART1 part1;
                        V3D33_TILE_BINNING_MODE_CONFIGURATION_PART1_unpack(cl, &part1);
                        V3D33_TILE_BINNING_MODE_CONFIGURATION_PART1_clif_dump(clif, &part1);
                } else {
                        struct V3D33_TILE_BINNING_MODE_CONFIGURATION_PART2 part2;
                        V3D33_TILE_BINNING_MODE_CONFIGURATION_PART2_unpack(cl, &part2);
                        V3D33_TILE_BINNING_MODE_CONFIGURATION_PART2_clif_dump(clif, &part2);
                }
                *size = V3D33_TILE_BINNING_MODE_CONFIGURATION_length;
                return true;
        }

        case V3D33_TILE_RENDERING_MODE_CONFIGURATION_opcode: {
                struct V3D33_TILE_RENDERING_MODE_CONFIGURATION common;
                V3D33_TILE_RENDERING_MODE_CONFIGURATION_unpack(cl, &common);
                if (common.sub_id == 0) {
                        HANDLE_PACKET_INNER(TILE_RENDERING_MODE_CONFIGURATION_COMMON_CONFIGURATION);
                } else if (common.sub_id == 1) {
                        HANDLE_PACKET_INNER(TILE_RENDERING_MODE_CONFIGURATION_Z_STENCIL_CONFIG);
                } else if (common.sub_id == 2) {
                        HANDLE_PACKET_INNER(TILE_RENDERING_MODE_CONFIGURATION_RENDER_TARGET_CONFIG);
                } else if (common.sub_id == 3) {
                        HANDLE_PACKET_INNER(TILE_RENDERING_MODE_CONFIGURATION_Z_STENCIL_CLEAR_VALUES);
                } else if (common.sub_id == 4) {
                        HANDLE_PACKET_INNER(TILE_RENDERING_MODE_CONFIGURATION_CLEAR_COLORS_PART1);
                } else {
                        out(clif, "Unknown render target config\n");
                }
                *size = V3D33_TILE_RENDERING_MODE_CONFIGURATION_length;
                return true;
        }

        case V3D33_HALT_opcode:
                out(clif, "HALT\n");
                *size = 1;
                return false;

        default:
                out(clif, "unknown packet %d\n", *cl);
                *size = 1;
                return false;
        }

}

static void
clif_dump_gl_shader_state_record(struct clif_dump *clif,
                                 struct reloc_worklist_entry *reloc, void *vaddr)
{
        out(clif, "GL Shader State Record at 0x%08x\n", reloc->addr);

        struct V3D33_GL_SHADER_STATE_RECORD values;
        V3D33_GL_SHADER_STATE_RECORD_unpack(vaddr, &values);
        V3D33_GL_SHADER_STATE_RECORD_clif_dump(clif, &values);
        vaddr += V3D33_GL_SHADER_STATE_RECORD_length;

        for (int i = 0; i < reloc->shader_state.num_attrs; i++) {
                struct V3D33_GL_SHADER_STATE_ATTRIBUTE_RECORD values;
                V3D33_GL_SHADER_STATE_ATTRIBUTE_RECORD_unpack(vaddr, &values);
                V3D33_GL_SHADER_STATE_ATTRIBUTE_RECORD_clif_dump(clif, &values);
                vaddr += V3D33_GL_SHADER_STATE_ATTRIBUTE_RECORD_length;
        }
}

static void
clif_process_worklist(struct clif_dump *clif)
{
        while (!list_empty(&clif->worklist)) {
                struct reloc_worklist_entry *reloc =
                        list_first_entry(&clif->worklist,
                                         struct reloc_worklist_entry, link);
                list_del(&reloc->link);

                void *vaddr;
                if (!clif->lookup_vaddr(clif->data, reloc->addr, &vaddr)) {
                        out(clif, "Failed to look up address 0x%08x\n",
                            reloc->addr);
                        continue;
                }

                switch (reloc->type) {
                case reloc_gl_shader_state:
                        clif_dump_gl_shader_state_record(clif, reloc, vaddr);
                        break;
                }
                out(clif, "\n");
        }
}

void
clif_dump_add_cl(struct clif_dump *clif, uint32_t address)
{
        uint32_t size;

        void *vaddr;
        if (!clif->lookup_vaddr(clif->data, address, &vaddr)) {
                out(clif, "Failed to look up address 0x%08x\n",
                    address);
                return;
        }

        uint8_t *cl = vaddr;
        while (clif_dump_packet(clif, address, cl, &size)) {
                cl += size;
                address += size;
        }

        out(clif, "\n");

        clif_process_worklist(clif);
}
