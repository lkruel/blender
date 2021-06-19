/*
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * The Original Code is Copyright (C) 2021 Blender Foundation.
 * All rights reserved.
 */

/** \file
 * \ingroup edtransform
 */

#include "DNA_windowmanager_types.h"

#include "BLI_blenlib.h"
#include "BLI_math.h"

#include "BKE_context.h"
#include "BKE_scene.h"

#include "SEQ_iterator.h"
#include "SEQ_sequencer.h"
#include "SEQ_time.h"

#include "WM_types.h"

#include "UI_resources.h"
#include "UI_view2d.h"

#include "MEM_guardedalloc.h"

#include "transform.h"
#include "transform_convert.h"
#include "transform_snap.h"

typedef struct TransSeqSnapData {
  int *source_snap_points;
  int source_snap_point_count;
  int source_point_min;
  int source_point_max;

  int *target_snap_points;
  int target_snap_point_count;

  eSeqTransformFlag snap_side;
} TransSeqSnapData;

/* -------------------------------------------------------------------- */
/** \name Snap sources
 * \{ */

static eSeqTransformFlag seq_snap_source_side_get(TransInfo *t, SeqCollection *snap_sources)
{
  SequencerToolSettings *tool_settings = SEQ_tool_settings_ensure(t->scene);

  if ((tool_settings->snap_side & SEQ_SNAP_SOURCE_MOUSE) == 0) {
    return tool_settings->snap_side;
  }

  int boundbox_min = MAXFRAME, boundbox_max = MINFRAME;
  Sequence *seq;
  SEQ_ITERATOR_FOREACH (seq, snap_sources) {
    if (seq->startdisp < boundbox_min) {
      boundbox_min = seq->startdisp;
    }
    if (seq->enddisp > boundbox_max) {
      boundbox_max = seq->enddisp;
    }
  }

  /* Set the snap mode based on how close the mouse is at the end/start source_snap_points. */
  int xmouse = (int)UI_view2d_region_to_view_x((View2D *)t->view, t->mouse.imval[0]);
  if (abs(xmouse - boundbox_max) > abs(xmouse - boundbox_min)) {
    return SEQ_SNAP_SOURCE_LEFT;
  }
  return SEQ_SNAP_SOURCE_RIGHT;
}

static int seq_get_snap_source_points_count(TransInfo *t,
                                            TransSeqSnapData *snap_data,
                                            SeqCollection *snap_sources)
{
  SequencerToolSettings *tool_settings = SEQ_tool_settings_ensure(t->scene);

  int count = 1; /* in case of SEQ_SNAP_SELECTION */
  if (tool_settings->snap_source == SEQ_SNAP_EACH_STRIP) {
    count = SEQ_collection_count(snap_sources);
  }

  if (snap_data->snap_side == SEQ_SNAP_SOURCE_BOTH) {
    count *= 2;
  }
  return count;
}

static void seq_snap_source_points_alloc(TransInfo *t,
                                         TransSeqSnapData *snap_data,
                                         SeqCollection *snap_sources)
{
  size_t point_count = seq_get_snap_source_points_count(t, snap_data, snap_sources);
  snap_data->source_snap_points = MEM_callocN(sizeof(int) * point_count, __func__);
  memset(snap_data->source_snap_points, 0, sizeof(int));
  snap_data->source_snap_point_count = point_count;
}

static void seq_snap_source_points_build_set_min_max(
    TransInfo *t, TransSeqSnapData *snap_data, int left, int right, int *r_i)
{
  SequencerToolSettings *tool_settings = SEQ_tool_settings_ensure(t->scene);

  snap_data->source_point_min = min_ii(left, snap_data->source_point_min);
  snap_data->source_point_max = max_ii(right, snap_data->source_point_max);

  int min = left, max = right;
  if (tool_settings->snap_source == SEQ_SNAP_SELECTION) {
    min = snap_data->source_point_min;
    max = snap_data->source_point_max;
  }

  if (snap_data->snap_side & (SEQ_SNAP_SOURCE_LEFT | SEQ_SNAP_SOURCE_BOTH)) {
    snap_data->source_snap_points[*r_i] = left;
    *r_i += 1;
  }
  if (snap_data->snap_side & (SEQ_SNAP_SOURCE_RIGHT | SEQ_SNAP_SOURCE_BOTH)) {
    snap_data->source_snap_points[*r_i] = right;
    *r_i += 1;
  }
  BLI_assert(*r_i <= snap_data->source_snap_point_count);

  if (tool_settings->snap_source == SEQ_SNAP_SELECTION) {
    *r_i = 0;
  }
}

static int cmp_fn(const void *a, const void *b)
{
  return (*(int *)a - *(int *)b);
}

static void seq_snap_source_points_build(TransInfo *t,
                                         TransSeqSnapData *snap_data,
                                         SeqCollection *snap_sources)
{
  snap_data->source_point_max = INT32_MIN;
  snap_data->source_point_min = INT32_MAX;

  int i = 0;
  Sequence *seq;
  SEQ_ITERATOR_FOREACH (seq, snap_sources) {
    int left = 0, right = 0;
    if (seq->flag & SEQ_LEFTSEL) {
      left = right = seq->startdisp;
    }
    else if (seq->flag & SEQ_RIGHTSEL) {
      left = right = seq->enddisp;
    }
    else {
      left = seq->startdisp;
      right = seq->enddisp;
    }

    seq_snap_source_points_build_set_min_max(t, snap_data, left, right, &i);
  }
  qsort(snap_data->source_snap_points, snap_data->source_snap_point_count, sizeof(int), cmp_fn);
}

/** \} */

/* -------------------------------------------------------------------- */
/** \name Snap targets
 * \{ */

static SeqCollection *query_snap_targets(const TransInfo *t)
{
  ListBase *seqbase = SEQ_active_seqbase_get(SEQ_editing_get(t->scene, false));
  SequencerToolSettings *tool_settings = SEQ_tool_settings_ensure(t->scene);

  SeqCollection *collection = SEQ_collection_create();
  LISTBASE_FOREACH (Sequence *, seq, seqbase) {
    if ((seq->flag & SELECT)) {
      continue; /* Selected are being transformed. */
    }
    if ((seq->flag & SEQ_MUTE) && (tool_settings->transform_flag & SEQ_SNAP_IGNORE_MUTED)) {
      continue;
    }
    if (seq->type == SEQ_TYPE_SOUND_RAM &&
        (tool_settings->transform_flag & SEQ_SNAP_IGNORE_SOUND)) {
      continue;
    }
    SEQ_collection_append_strip(seq, collection);
  }
  return collection;
}

static int seq_get_snap_target_points_count(TransInfo *t,
                                            TransSeqSnapData *snap_data,
                                            SeqCollection *snap_targets)
{
  SequencerToolSettings *tool_settings = SEQ_tool_settings_ensure(t->scene);

  int count = 0; /* in case of SEQ_SNAP_SELECTION */
  if (tool_settings->transform_flag & SEQ_SNAP_TO_STRIP_START) {
    count++;
  }
  if (tool_settings->transform_flag & SEQ_SNAP_TO_STRIP_END) {
    count++;
  }
  if (tool_settings->transform_flag & SEQ_SNAP_TO_STRIP_HOLD) {
    count += 2;
  }

  count *= SEQ_collection_count(snap_targets);

  if (tool_settings->transform_flag & SEQ_SNAP_TO_PLAYHEAD) {
    count++;
  }

  return count;
}

static void seq_snap_target_points_alloc(TransInfo *t,
                                         TransSeqSnapData *snap_data,
                                         SeqCollection *snap_targets)
{
  size_t point_count = seq_get_snap_target_points_count(t, snap_data, snap_targets);
  snap_data->target_snap_points = MEM_callocN(sizeof(int) * point_count, __func__);
  memset(snap_data->target_snap_points, 0, sizeof(int));
  snap_data->target_snap_point_count = point_count;
}

static void seq_snap_target_points_build(TransInfo *t,
                                         TransSeqSnapData *snap_data,
                                         SeqCollection *snap_targets)
{
  Scene *scene = t->scene;
  SequencerToolSettings *tool_settings = SEQ_tool_settings_ensure(t->scene);

  int i = 0;

  if (tool_settings->transform_flag & SEQ_SNAP_TO_PLAYHEAD) {
    snap_data->target_snap_points[i] = CFRA;
    i++;
  }

  Sequence *seq;
  SEQ_ITERATOR_FOREACH (seq, snap_targets) {
    if (tool_settings->transform_flag & SEQ_SNAP_TO_STRIP_START) {
      snap_data->target_snap_points[i] = seq->startdisp;
      i++;
    }
    if (tool_settings->transform_flag & SEQ_SNAP_TO_STRIP_END) {
      snap_data->target_snap_points[i] = seq->enddisp;
      i++;
    }
    if (tool_settings->transform_flag & SEQ_SNAP_TO_STRIP_HOLD) {
      const int content_start = min_ii(seq->enddisp, seq->start);
      const int content_end = max_ii(seq->startdisp, seq->start + seq->len);
      snap_data->target_snap_points[i] = content_start;
      snap_data->target_snap_points[i + 1] = content_end;
      i += 2;
    }
  }
  BLI_assert(i <= snap_data->target_snap_point_count);
  qsort(snap_data->target_snap_points, snap_data->target_snap_point_count, sizeof(int), cmp_fn);
}

/** \} */

/* -------------------------------------------------------------------- */
/** \name Snap utilities
 * \{ */

static bool seq_snap_use_snapping(const TransInfo *t)
{
  if ((t->flag & T_MODAL) == 0) {
    return false;
  }

  SequencerToolSettings *tool_settings = SEQ_tool_settings_ensure(t->scene);
  return (bool)(tool_settings->transform_flag & SEQ_USE_SNAPPING) ^
         (bool)(t->modifiers & MOD_SNAP_INVERT);
}

static int seq_snap_threshold_get_frame_distance(TransInfo *t, const int mval[2])
{
  SequencerToolSettings *tool_settings = SEQ_tool_settings_ensure(t->scene);
  float xmouse = UI_view2d_region_to_view_x(&t->region->v2d, mval[0]);
  float threshold_dist = UI_view2d_region_to_view_x(&t->region->v2d,
                                                    mval[0] + tool_settings->snap_threshold);
  threshold_dist -= xmouse;
  return round_fl_to_int(threshold_dist);
}

void seq_snap_offset_apply(TransInfo *t, const int mval[2])
{
  if (!seq_snap_use_snapping(t)) {
    return;
  }

  TransSeqSnapData *snap_data = transform_convert_sequencer_get_snap_data(t);
  int best_dist = MAXFRAME, best_target_frame = 0, best_source_frame = 0;

  for (int i = 0; i < snap_data->source_snap_point_count; i++) {
    int snap_source_frame = snap_data->source_snap_points[i] + round_fl_to_int(t->values[0]);
    for (int j = 0; j < snap_data->target_snap_point_count; j++) {
      int snap_target_frame = snap_data->target_snap_points[j];

      int dist = abs(snap_target_frame - snap_source_frame);
      if (dist > best_dist) {
        continue;
      }

      best_dist = dist;
      best_target_frame = snap_target_frame;
      best_source_frame = snap_source_frame;
    }
  }

  if (best_dist > seq_snap_threshold_get_frame_distance(t, mval)) {
    return;
  }

  t->values[0] += best_target_frame - best_source_frame;
}

TransSeqSnapData *seq_snap_data_alloc(TransInfo *t, ListBase *seqbase)
{
  if (!seq_snap_use_snapping(t)) {
    return NULL;
  }

  TransSeqSnapData *snap_data = MEM_callocN(sizeof(TransSeqSnapData), __func__);

  /* Build array of snap points for transformed strips. */
  SeqCollection *snap_sources = SEQ_query_selected_strips(seqbase);
  snap_data->snap_side = seq_snap_source_side_get(t, snap_sources);
  seq_snap_source_points_alloc(t, snap_data, snap_sources);
  seq_snap_source_points_build(t, snap_data, snap_sources);
  SEQ_collection_free(snap_sources);

  SeqCollection *snap_targets = query_snap_targets(t);
  seq_snap_target_points_alloc(t, snap_data, snap_targets);
  seq_snap_target_points_build(t, snap_data, snap_targets);
  SEQ_collection_free(snap_targets);
  return snap_data;
}

void seq_snap_data_free(TransSeqSnapData *data)
{
  if (data == NULL) {
    return;
  }

  MEM_freeN(data->source_snap_points);
  MEM_freeN(data->target_snap_points);
  MEM_freeN(data);
}

/** \} */
