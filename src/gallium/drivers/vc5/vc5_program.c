/*
 * Copyright © 2014-2017 Broadcom
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
#include "nir/vc5_compiler.h"
#include "vc5_context.h"
#include "mesa/state_tracker/st_glsl_types.h"

static void *
vc5_shader_state_create(struct pipe_context *pctx,
                        const struct pipe_shader_state *cso)
{
        struct vc5_context *vc5 = vc5_context(pctx);
        struct vc5_uncompiled_shader *so = CALLOC_STRUCT(vc5_uncompiled_shader);
        if (!so)
                return NULL;

        so->program_id = vc5->next_uncompiled_program_id++;

        nir_shader *s;

        if (cso->type == PIPE_SHADER_IR_NIR) {
                /* The backend takes ownership of the NIR shader on state
                 * creation.
                 */
                s = cso->ir.nir;
        } else {
                assert(cso->type == PIPE_SHADER_IR_TGSI);

                if (vc5_debug & VC5_DEBUG_TGSI) {
                        fprintf(stderr, "prog %d TGSI:\n",
                                so->program_id);
                        tgsi_dump(cso->tokens, 0);
                        fprintf(stderr, "\n");
                }
                s = tgsi_to_nir(cso->tokens, &vc5_nir_options);
        }

        NIR_PASS_V(s, nir_opt_global_to_local);
        NIR_PASS_V(s, nir_lower_regs_to_ssa);
        NIR_PASS_V(s, nir_normalize_cubemap_coords);

        NIR_PASS_V(s, nir_lower_load_const_to_scalar);

        vc5_optimize_nir(s);

        NIR_PASS_V(s, nir_remove_dead_variables, nir_var_local);

        /* Garbage collect dead instructions */
        nir_sweep(s);

        so->base.type = PIPE_SHADER_IR_NIR;
        so->base.ir.nir = s;

        if (vc5_debug & VC5_DEBUG_NIR) {
                fprintf(stderr, "%s prog %d NIR:\n",
                        gl_shader_stage_name(s->stage),
                        so->program_id);
                nir_print_shader(s, stderr);
                fprintf(stderr, "\n");
        }

        return so;
}

static void
copy_uniform_state_to_shader(struct vc5_compiled_shader *shader,
                             struct vc5_compile *c)
{
        int count = c->num_uniforms;
        struct vc5_shader_uniform_info *uinfo = &shader->uniforms;

        uinfo->count = count;
        uinfo->data = ralloc_array(shader, uint32_t, count);
        memcpy(uinfo->data, c->uniform_data,
               count * sizeof(*uinfo->data));
        uinfo->contents = ralloc_array(shader, enum quniform_contents, count);
        memcpy(uinfo->contents, c->uniform_contents,
               count * sizeof(*uinfo->contents));
        uinfo->num_texture_samples = c->num_texture_samples;

        vc5_set_shader_uniform_dirty_flags(shader);
}

static void
vc5_setup_compiled_fs_inputs(struct vc5_context *vc5, struct vc5_compile *c,
                             struct vc5_compiled_shader *shader)
{
        struct vc5_fs_inputs inputs;

        memset(&inputs, 0, sizeof(inputs));
        inputs.input_slots = ralloc_array(shader,
                                          struct vc5_varying_slot,
                                          c->num_input_slots);

        bool input_live[c->num_input_slots];

        memset(input_live, 0, sizeof(input_live));
        qir_for_each_inst_inorder(inst, c) {
                for (int i = 0; i < qir_get_nsrc(inst); i++) {
                        if (inst->src[i].file == QFILE_VARY)
                                input_live[inst->src[i].index] = true;
                }
        }

        for (int i = 0; i < c->num_input_slots; i++) {
                struct vc5_varying_slot *slot = &c->input_slots[i];

                if (!input_live[i])
                        continue;

                /* Skip non-VS-output inputs. */
                if (slot->slot == (uint8_t)~0)
                        continue;

                if (slot->slot == VARYING_SLOT_COL0 ||
                    slot->slot == VARYING_SLOT_COL1 ||
                    slot->slot == VARYING_SLOT_BFC0 ||
                    slot->slot == VARYING_SLOT_BFC1) {
                        BITSET_SET(shader->color_inputs, inputs.num_inputs);
                }

                if (BITSET_TEST(c->flat_shade_flags, i))
                        BITSET_SET(shader->flat_shade_flags, inputs.num_inputs);

                inputs.input_slots[inputs.num_inputs] = *slot;
                inputs.num_inputs++;
        }
        shader->num_inputs = inputs.num_inputs;

        /* Add our set of inputs to the set of all inputs seen.  This way, we
         * can have a single pointer that identifies an FS inputs set,
         * allowing VS to avoid recompiling when the FS is recompiled (or a
         * new one is bound using separate shader objects) but the inputs
         * don't change.
         */
        struct set_entry *entry = _mesa_set_search(vc5->fs_inputs_set, &inputs);
        if (entry) {
                shader->fs_inputs = entry->key;
                ralloc_free(inputs.input_slots);
        } else {
                struct vc5_fs_inputs *alloc_inputs;

                alloc_inputs = rzalloc(vc5->fs_inputs_set, struct vc5_fs_inputs);
                memcpy(alloc_inputs, &inputs, sizeof(inputs));
                ralloc_steal(alloc_inputs, inputs.input_slots);
                _mesa_set_add(vc5->fs_inputs_set, alloc_inputs);

                shader->fs_inputs = alloc_inputs;
        }
}

static struct vc5_compiled_shader *
vc5_get_compiled_shader(struct vc5_context *vc5, struct vc5_key *key)
{
        nir_shader *s = key->shader_state->base.ir.nir;

        struct hash_table *ht;
        uint32_t key_size;
        if (s->stage == MESA_SHADER_FRAGMENT) {
                ht = vc5->fs_cache;
                key_size = sizeof(struct vc5_fs_key);
        } else {
                ht = vc5->vs_cache;
                key_size = sizeof(struct vc5_vs_key);
        }

        struct vc5_compiled_shader *shader;
        struct hash_entry *entry = _mesa_hash_table_search(ht, key);
        if (entry)
                return entry->data;

        struct vc5_compile *c = vc5_nir_shader_compile(&vc5->screen->chip,
                                                       key, s, vc5_debug);
        shader = rzalloc(NULL, struct vc5_compiled_shader);

        shader->program_id = vc5->next_compiled_program_id++;
        if (s->stage == MESA_SHADER_FRAGMENT) {
                vc5_setup_compiled_fs_inputs(vc5, c, shader);

                /* Note: the temporary clone in c->s has been freed. */
                nir_shader *orig_shader = key->shader_state->base.ir.nir;
                if (orig_shader->info.outputs_written & (1 << FRAG_RESULT_DEPTH))
                        shader->writes_z = true;
        } else {
                shader->num_inputs = c->num_inputs;

                /* The vertex data gets format converted by the VPM so that
                 * each attribute channel takes up a VPM column.  Precompute
                 * the sizes for the shader record.
                 */
                for (int i = 0; i < ARRAY_SIZE(shader->vattr_sizes); i++) {
                        shader->vattr_sizes[i] = c->vattr_sizes[i];
                        shader->vpm_input_size += c->vattr_sizes[i];
                }

                /* Input/output segment size are in 8x32-bit multiples. */
                shader->vpm_input_size = align(shader->vpm_input_size, 8) / 8;
                shader->vpm_output_size = align(c->num_vpm_writes, 8) / 8;

                shader->uses_vid = (s->info.system_values_read &
                                    (1ull << SYSTEM_VALUE_VERTEX_ID));
                shader->uses_iid = (s->info.system_values_read &
                                    (1ull << SYSTEM_VALUE_INSTANCE_ID));
        }

        copy_uniform_state_to_shader(shader, c);
        int shader_size = c->qpu_inst_count * sizeof(uint64_t);
        shader->bo = vc5_bo_alloc(vc5->screen, shader_size, "shader");
        vc5_bo_map(shader->bo);
        memcpy(shader->bo->map, c->qpu_insts, shader_size);

        /* Copy the compiler UBO range state to the compiled shader, dropping
         * out arrays that were never referenced by an indirect load.
         *
         * (Note that QIR dead code elimination of an array access still
         * leaves that array alive, though)
         */
        if (c->num_ubo_ranges) {
                shader->num_ubo_ranges = c->num_ubo_ranges;
                shader->ubo_ranges = ralloc_array(shader, struct vc5_ubo_range,
                                                  c->num_ubo_ranges);
                uint32_t j = 0;
                for (int i = 0; i < c->num_uniform_ranges; i++) {
                        struct vc5_compiler_ubo_range *range =
                                &c->ubo_ranges[i];
                        if (!range->used)
                                continue;

                        shader->ubo_ranges[j].dst_offset = range->dst_offset;
                        shader->ubo_ranges[j].src_offset = range->src_offset;
                        shader->ubo_ranges[j].size = range->size;
                        shader->ubo_size += c->ubo_ranges[i].size;
                        j++;
                }
        }
        if (shader->ubo_size) {
                if (vc5_debug & VC5_DEBUG_SHADERDB) {
                        fprintf(stderr, "SHADER-DB: %s prog %d/%d: %d UBO uniforms\n",
                                qir_get_stage_name(c),
                                c->program_id, c->variant_id,
                                shader->ubo_size / 4);
                }
        }

        qir_compile_destroy(c);

        struct vc5_key *dup_key;
        dup_key = ralloc_size(shader, key_size);
        memcpy(dup_key, key, key_size);
        _mesa_hash_table_insert(ht, dup_key, shader);

        return shader;
}

static void
vc5_setup_shared_key(struct vc5_context *vc5, struct vc5_key *key,
                     struct vc5_texture_stateobj *texstate)
{
        for (int i = 0; i < texstate->num_textures; i++) {
                struct pipe_sampler_view *sampler = texstate->textures[i];
                struct vc5_sampler_view *vc5_sampler = vc5_sampler_view(sampler);
                struct pipe_sampler_state *sampler_state =
                        texstate->samplers[i];

                if (!sampler)
                        continue;

                key->tex[i].return_size =
                        vc5_get_tex_return_size(sampler->format);

                /* For 16-bit, we set up the sampler to always return 2
                 * channels (meaning no recompiles for most statechanges),
                 * while for 32 we actually scale the returns with channels.
                 */
                if (key->tex[i].return_size == 16) {
                        key->tex[i].return_channels = 2;
                } else {
                        key->tex[i].return_channels =
                                vc5_get_tex_return_channels(sampler->format);
                }

                if (vc5_get_tex_return_size(sampler->format) == 32) {
                        memcpy(key->tex[i].swizzle,
                               vc5_sampler->swizzle,
                               sizeof(vc5_sampler->swizzle));
                } else {
                        /* For 16-bit returns, we let the sampler state handle
                         * the swizzle.
                         */
                        key->tex[i].swizzle[0] = PIPE_SWIZZLE_X;
                        key->tex[i].swizzle[1] = PIPE_SWIZZLE_Y;
                        key->tex[i].swizzle[2] = PIPE_SWIZZLE_Z;
                        key->tex[i].swizzle[3] = PIPE_SWIZZLE_W;
                }

                if (sampler->texture->nr_samples > 1) {
                        key->tex[i].msaa_width = sampler->texture->width0;
                        key->tex[i].msaa_height = sampler->texture->height0;
                } else if (sampler){
                        key->tex[i].compare_mode = sampler_state->compare_mode;
                        key->tex[i].compare_func = sampler_state->compare_func;
                        key->tex[i].wrap_s = sampler_state->wrap_s;
                        key->tex[i].wrap_t = sampler_state->wrap_t;
                }
        }

        key->ucp_enables = vc5->rasterizer->base.clip_plane_enable;
}

static void
vc5_update_compiled_fs(struct vc5_context *vc5, uint8_t prim_mode)
{
        struct vc5_job *job = vc5->job;
        struct vc5_fs_key local_key;
        struct vc5_fs_key *key = &local_key;

        if (!(vc5->dirty & (VC5_DIRTY_PRIM_MODE |
                            VC5_DIRTY_BLEND |
                            VC5_DIRTY_FRAMEBUFFER |
                            VC5_DIRTY_ZSA |
                            VC5_DIRTY_RASTERIZER |
                            VC5_DIRTY_SAMPLE_MASK |
                            VC5_DIRTY_FRAGTEX |
                            VC5_DIRTY_UNCOMPILED_FS))) {
                return;
        }

        memset(key, 0, sizeof(*key));
        vc5_setup_shared_key(vc5, &key->base, &vc5->fragtex);
        key->base.shader_state = vc5->prog.bind_fs;
        key->is_points = (prim_mode == PIPE_PRIM_POINTS);
        key->is_lines = (prim_mode >= PIPE_PRIM_LINES &&
                         prim_mode <= PIPE_PRIM_LINE_STRIP);
        key->blend = vc5->blend->rt[0];
        key->clamp_color = vc5->rasterizer->base.clamp_fragment_color;
        if (vc5->blend->logicop_enable) {
                key->logicop_func = vc5->blend->logicop_func;
        } else {
                key->logicop_func = PIPE_LOGICOP_COPY;
        }
        if (job->msaa) {
                key->msaa = vc5->rasterizer->base.multisample;
                key->sample_coverage = (vc5->rasterizer->base.multisample &&
                                        vc5->sample_mask != (1 << VC5_MAX_SAMPLES) - 1);
                key->sample_alpha_to_coverage = vc5->blend->alpha_to_coverage;
                key->sample_alpha_to_one = vc5->blend->alpha_to_one;
        }

        key->depth_enabled = (vc5->zsa->base.depth.enabled ||
                              vc5->zsa->base.stencil[0].enabled);
        if (vc5->zsa->base.alpha.enabled) {
                key->alpha_test = true;
                key->alpha_test_func = vc5->zsa->base.alpha.func;
        }

        if (vc5->framebuffer.cbufs[0]) {
                struct pipe_surface *cbuf = vc5->framebuffer.cbufs[0];
                const struct util_format_description *desc =
                        util_format_description(cbuf->format);

                key->swap_color_rb = desc->swizzle[0] == PIPE_SWIZZLE_Z;
        }

        if (key->is_points) {
                key->point_sprite_mask =
                        vc5->rasterizer->base.sprite_coord_enable;
                key->point_coord_upper_left =
                        (vc5->rasterizer->base.sprite_coord_mode ==
                         PIPE_SPRITE_COORD_UPPER_LEFT);
        }

        key->light_twoside = vc5->rasterizer->base.light_twoside;

        struct vc5_compiled_shader *old_fs = vc5->prog.fs;
        vc5->prog.fs = vc5_get_compiled_shader(vc5, &key->base);
        if (vc5->prog.fs == old_fs)
                return;

        vc5->dirty |= VC5_DIRTY_COMPILED_FS;

        if (old_fs &&
            (vc5->prog.fs->flat_shade_flags != old_fs->flat_shade_flags ||
             (vc5->rasterizer->base.flatshade &&
              vc5->prog.fs->color_inputs != old_fs->color_inputs))) {
                vc5->dirty |= VC5_DIRTY_FLAT_SHADE_FLAGS;
        }

        if (old_fs && vc5->prog.fs->fs_inputs != old_fs->fs_inputs)
                vc5->dirty |= VC5_DIRTY_FS_INPUTS;
}

static void
vc5_update_compiled_vs(struct vc5_context *vc5, uint8_t prim_mode)
{
        struct vc5_vs_key local_key;
        struct vc5_vs_key *key = &local_key;

        if (!(vc5->dirty & (VC5_DIRTY_PRIM_MODE |
                            VC5_DIRTY_RASTERIZER |
                            VC5_DIRTY_VERTTEX |
                            VC5_DIRTY_VTXSTATE |
                            VC5_DIRTY_UNCOMPILED_VS |
                            VC5_DIRTY_FS_INPUTS))) {
                return;
        }

        memset(key, 0, sizeof(*key));
        vc5_setup_shared_key(vc5, &key->base, &vc5->verttex);
        key->base.shader_state = vc5->prog.bind_vs;
        key->fs_inputs = vc5->prog.fs->fs_inputs;
        key->clamp_color = vc5->rasterizer->base.clamp_vertex_color;

        key->per_vertex_point_size =
                (prim_mode == PIPE_PRIM_POINTS &&
                 vc5->rasterizer->base.point_size_per_vertex);

        struct vc5_compiled_shader *vs =
                vc5_get_compiled_shader(vc5, &key->base);
        if (vs != vc5->prog.vs) {
                vc5->prog.vs = vs;
                vc5->dirty |= VC5_DIRTY_COMPILED_VS;
        }

        key->is_coord = true;
        /* Coord shaders don't care what the FS inputs are. */
        key->fs_inputs = NULL;
        struct vc5_compiled_shader *cs =
                vc5_get_compiled_shader(vc5, &key->base);
        if (cs != vc5->prog.cs) {
                vc5->prog.cs = cs;
                vc5->dirty |= VC5_DIRTY_COMPILED_CS;
        }
}

void
vc5_update_compiled_shaders(struct vc5_context *vc5, uint8_t prim_mode)
{
        vc5_update_compiled_fs(vc5, prim_mode);
        vc5_update_compiled_vs(vc5, prim_mode);
}

static uint32_t
fs_cache_hash(const void *key)
{
        return _mesa_hash_data(key, sizeof(struct vc5_fs_key));
}

static uint32_t
vs_cache_hash(const void *key)
{
        return _mesa_hash_data(key, sizeof(struct vc5_vs_key));
}

static bool
fs_cache_compare(const void *key1, const void *key2)
{
        return memcmp(key1, key2, sizeof(struct vc5_fs_key)) == 0;
}

static bool
vs_cache_compare(const void *key1, const void *key2)
{
        return memcmp(key1, key2, sizeof(struct vc5_vs_key)) == 0;
}

static uint32_t
fs_inputs_hash(const void *key)
{
        const struct vc5_fs_inputs *inputs = key;

        return _mesa_hash_data(inputs->input_slots,
                               sizeof(*inputs->input_slots) *
                               inputs->num_inputs);
}

static bool
fs_inputs_compare(const void *key1, const void *key2)
{
        const struct vc5_fs_inputs *inputs1 = key1;
        const struct vc5_fs_inputs *inputs2 = key2;

        return (inputs1->num_inputs == inputs2->num_inputs &&
                memcmp(inputs1->input_slots,
                       inputs2->input_slots,
                       sizeof(*inputs1->input_slots) *
                       inputs1->num_inputs) == 0);
}

static void
delete_from_cache_if_matches(struct hash_table *ht,
                             struct hash_entry *entry,
                             struct vc5_uncompiled_shader *so)
{
        const struct vc5_key *key = entry->key;

        if (key->shader_state == so) {
                struct vc5_compiled_shader *shader = entry->data;
                _mesa_hash_table_remove(ht, entry);
                vc5_bo_unreference(&shader->bo);
                ralloc_free(shader);
        }
}

static void
vc5_shader_state_delete(struct pipe_context *pctx, void *hwcso)
{
        struct vc5_context *vc5 = vc5_context(pctx);
        struct vc5_uncompiled_shader *so = hwcso;

        struct hash_entry *entry;
        hash_table_foreach(vc5->fs_cache, entry)
                delete_from_cache_if_matches(vc5->fs_cache, entry, so);
        hash_table_foreach(vc5->vs_cache, entry)
                delete_from_cache_if_matches(vc5->vs_cache, entry, so);

        ralloc_free(so->base.ir.nir);
        free(so);
}

static void
vc5_fp_state_bind(struct pipe_context *pctx, void *hwcso)
{
        struct vc5_context *vc5 = vc5_context(pctx);
        vc5->prog.bind_fs = hwcso;
        vc5->dirty |= VC5_DIRTY_UNCOMPILED_FS;
}

static void
vc5_vp_state_bind(struct pipe_context *pctx, void *hwcso)
{
        struct vc5_context *vc5 = vc5_context(pctx);
        vc5->prog.bind_vs = hwcso;
        vc5->dirty |= VC5_DIRTY_UNCOMPILED_VS;
}

void
vc5_program_init(struct pipe_context *pctx)
{
        struct vc5_context *vc5 = vc5_context(pctx);

        pctx->create_vs_state = vc5_shader_state_create;
        pctx->delete_vs_state = vc5_shader_state_delete;

        pctx->create_fs_state = vc5_shader_state_create;
        pctx->delete_fs_state = vc5_shader_state_delete;

        pctx->bind_fs_state = vc5_fp_state_bind;
        pctx->bind_vs_state = vc5_vp_state_bind;

        vc5->fs_cache = _mesa_hash_table_create(pctx, fs_cache_hash,
                                                fs_cache_compare);
        vc5->vs_cache = _mesa_hash_table_create(pctx, vs_cache_hash,
                                                vs_cache_compare);
        vc5->fs_inputs_set = _mesa_set_create(pctx, fs_inputs_hash,
                                              fs_inputs_compare);
}

void
vc5_program_fini(struct pipe_context *pctx)
{
        struct vc5_context *vc5 = vc5_context(pctx);

        struct hash_entry *entry;
        hash_table_foreach(vc5->fs_cache, entry) {
                struct vc5_compiled_shader *shader = entry->data;
                vc5_bo_unreference(&shader->bo);
                ralloc_free(shader);
                _mesa_hash_table_remove(vc5->fs_cache, entry);
        }

        hash_table_foreach(vc5->vs_cache, entry) {
                struct vc5_compiled_shader *shader = entry->data;
                vc5_bo_unreference(&shader->bo);
                ralloc_free(shader);
                _mesa_hash_table_remove(vc5->vs_cache, entry);
        }
}
