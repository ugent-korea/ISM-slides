# bdiag — logistic regression vs. XGBoost

Three classifiers for the Wisconsin breast cancer dataset (`bdiag.csv`),
predicting `diagnosis` (B/M) from the cell-nucleus measurements.

## Models

1. Logistic regression on `radius_mean` + `texture_mean`
2. Logistic regression on all features
3. XGBoost on all features

All models are trained on a 75/25 train/test split and evaluated by accuracy and AUC on the held-out test set.

## Running

R dependencies are pinned via `renv`:

```bash
Rscript -e 'renv::restore()'
Rscript classifiers.R
```

## Results (test set, n = 143)

| Model                                | Accuracy |    AUC |
| ------------------------------------ | -------: | -----: |
| Logistic (radius_mean + texture_mean)|   0.8392 | 0.9327 |
| Logistic (all features)              |   0.9301 | 0.9514 |
| XGBoost (all features)               |   0.9790 | 0.9964 |
