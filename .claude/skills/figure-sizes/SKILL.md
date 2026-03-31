---
name: format-figures
description: Sets reasonable, uniform sizes for embedded figures. Used when discussing figure sizes or when the user asks to adjust, update or fix the figure sizes.
---

This skill only pertains to embedded figures that are generated via code. It does not apply to images that are inserted directly.

## Standard header for full-width figures

Use the following header for full-width embedded code blocks:

#| out-height: 2in
#| fig-width: 5
#| fig-height: 3
#| fig-align: center

Only set `out-height`, not `out-width`; this preserves the aspect ratio set by `fig-width`/`fig-height`. If a chunk already has `out-width`, remove it.

Keep `fig-height: 3` constant across all plots to ensure consistent font sizes. Adjust `fig-width` if a different aspect ratio is needed (e.g. wider plots), but do not change `fig-height`.

## Column layouts

Leave figures inside column layouts (`:::: {.columns}` / `::: {.column}`) unchanged. These have intentional custom sizing to fit their column width. Flag them in your comments so the user can review, but do not modify them.

## General guidance

Comment liberally on whether you think there are any issues, so that the user can have a look. In particular, flag:
- Figures in column layouts (left unchanged)
- Figures with unusual custom sizing (e.g. biplots, small square plots, matrix plots) that may be intentional