# Design: Assessment Weights & Risk Projection

## Problem

During a semester, unit coordinators need to track whether students are on track
to pass. Raw scores on individual assessments don't give a clear picture —
weights vary, and early in the semester most assessments are incomplete. We need
a way to assign weights to assessments, calculate a projected final grade, and
flag students at risk of failing.

## Data Model

Unit-level weight configuration stored in `.nightmare/weights.json`:

```json
{
  "version": 1,
  "saved_at": "2026-02-08T10:00:00",
  "weights": {
    "Assignment 1": 10,
    "Midterm Quiz": 15,
    "Final Exam": 40
  }
}
```

- Keys match assessment name strings exactly (Canvas assignments and uploaded
  exam assessments use the same namespace).
- Values are percentage weights.
- Assessments not in the map are excluded from the grade calculation.
- No enforcement that weights sum to 100 — the UI shows the running total so
  the user can see and correct it.

New file: `R/utils/weights_data.R` (load/save/calculate). New reactiveVal:
`weightsData` in `server.R`.

## UI: Grade Projection Strip

A compact bar between the Assessments section header and the table. Visible
only when weights are configured and at least one weighted assessment is
completed. Hidden otherwise.

```
┌──────────────────────────────────────────────────┐
│  PROJECTED   62.3%         * MODERATE RISK  (i)  │
│  ██████████████████████░░░░░░░░░░░░░░░░░░░░░░░░  │
│  65% of grade assessed                           │
└──────────────────────────────────────────────────┘
```

- **Projected grade**: weighted average of completed assessments (sum of score%
  x weight / sum of completed weights).
- **Progress bar**: fills proportionally to completed weight / total weight.
- **Risk label**: derived from the projected performance model (see below).
- **Info icon (i)**: click to show a popover explaining the risk model and
  thresholds.

## UI: Weight Edit Mode

Triggered by an "Edit Weights" link in the Assessments section header.

**Entering edit mode:**
1. Section header changes to "Assessments — Editing Weights" with a "Done"
   button on the right.
2. A "Weight" column appears in the table with small numeric inputs (placeholder
   "—").
3. A footer row shows `Total: 75 / 100%`, updating live. Red text if > 100.

**Exiting edit mode (clicking "Done"):**
1. Weights save to `.nightmare/weights.json`.
2. Weight column disappears.
3. Projection strip updates (or appears for the first time).
4. Section header reverts to "Assessments".

Weights are unit-level — editing on any student's page applies to all students.

## Risk Model

Based on projected performance analysis, a well-established approach in learning
analytics early warning systems (Arnold & Pistilli, 2012; Dawson et al., 2017).

### Calculation

Given:
- **completed_points** = sum of (score% x weight) for completed assessments
- **completed_weight** = sum of weights for completed assessments
- **remaining_weight** = total configured weight - completed_weight
- **current_average** = completed_points / completed_weight (if > 0)
- **max_possible** = completed_points + remaining_weight (100% on everything
  left)
- **required_average** = (50 * total_weight - completed_points) /
  remaining_weight (what's needed on remaining to reach 50%)

### Thresholds

| Level    | Condition                              | Style              |
|----------|----------------------------------------|--------------------|
| Critical | max_possible < 50% — cannot pass       | Black bg, white text |
| High     | required_average > 80%                 | Dark text, #EEE bg |
| Moderate | required_average > 60%                 | Dark text, #F5F5F5 bg |
| Low      | On track, required_average <= 60%      | No special styling |
| Hidden   | No weights or no completed assessments | Strip not shown    |

### References

- Arnold, K. E., & Pistilli, M. D. (2012). Course Signals at Purdue: Using
  learning analytics to increase student success. *Proceedings of the 2nd
  International Conference on Learning Analytics and Knowledge*, 267-270.
- Dawson, S., Jovanovic, J., Gasevic, D., & Pardo, A. (2017). From prediction
  to impact: Evaluation of a learning analytics retention program. *Proceedings
  of the 7th International Conference on Learning Analytics and Knowledge*,
  474-478.
- Gasevic, D., Dawson, S., Rogers, T., & Gasevic, D. (2016). Learning analytics
  should not promote one size fits all: The effects of instructional conditions
  in predicting academic success. *The Internet and Higher Education*, 28, 68-84.

## Files to Create/Modify

- **Create** `R/utils/weights_data.R` — load, save, calculate projected grade
  and risk level
- **Modify** `R/server.R` — source new file, add `weightsData` reactiveVal,
  load/save observers
- **Modify** `R/utils/ui_helpers.R` — projection strip, edit mode toggle,
  weight column rendering
- **Modify** `www/custom.css` — projection strip styles, edit mode styles, risk
  badges, info popover

## Verification

1. `Rscript -e "source('app.R')"` — app loads without errors
2. Projection strip hidden when no weights configured
3. Edit mode: weight column appears, total updates live, saves on Done
4. Risk labels update correctly as scores change
5. Weights persist across page reloads (stored in JSON)
6. Info icon shows methodology popover
