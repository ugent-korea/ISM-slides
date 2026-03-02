# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Quarto-based Beamer slide decks for the "Introduction to Statistical Modeling" course. QMD files render to PDF via LuaTeX.

## Build Commands

```bash
# Render all slides
quarto render

# Render a single slide deck
quarto render 01a-regression-intro.qmd

# Preview with live reload
quarto preview 01a-regression-intro.qmd
```

Output goes to `pdfs/` directory. Quarto uses `freeze: auto` so cached results in `_freeze/` are only regenerated when source changes.

## Slide Naming Convention

Files follow `[chapter][letter]-[topic].qmd`:
- **01a-g**: Linear regression (intro, simple, multiple, predictivity, outliers, categorical, multicollinearity)
- **02a-d**: PCA and logistic regression
- **03a-f**: Nonlinear regression, model selection, sensitivity analysis, Monte Carlo

## Slide Format

All QMD files use identical Beamer YAML frontmatter:
```yaml
format:
  beamer:
    theme: Pittsburgh
    colortheme: default
    fonttheme: default
    include-in-header:
      - file: header.tex
    pdf-engine: lualatex
```

`header.tex` adds left-aligned frame titles, slide numbers, emoji support, and Korean language support via `luatexko`. The `lualatex` engine is required.

## Key Directories

- `datasets/` — CSV, RDA, XLSX data files used in examples
- `scripts/` — R helper functions (regression, parameter estimation, sensitivity analysis)
- `images/` — Static images organized by topic
- `_freeze/` — Quarto render cache (auto-managed)
- `case-studies/` — Supporting R materials

## R Environment

R dependencies are managed with **renv**. Key packages: tidyverse, ggplot2, car, MASS, caret, glmnet, pROC, plot3D.

```bash
# Restore R environment
Rscript -e "renv::restore()"
```
