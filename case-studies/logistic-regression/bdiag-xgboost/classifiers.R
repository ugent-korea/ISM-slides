library(xgboost)
library(pROC)

set.seed(49773858)

bdiag <- read.csv("bdiag.csv", check.names = FALSE)
bdiag <- bdiag[, nzchar(names(bdiag))]
bdiag$id <- NULL
names(bdiag) <- make.names(names(bdiag))
bdiag$diagnosis <- factor(bdiag$diagnosis, levels = c("B", "M"))

n <- nrow(bdiag)
train_idx <- sample.int(n, size = floor(0.75 * n))
train <- bdiag[train_idx, ]
test  <- bdiag[-train_idx, ]

evaluate <- function(label, truth, prob) {
  pred <- factor(ifelse(prob >= 0.5, "M", "B"), levels = c("B", "M"))
  cm <- table(truth = truth, pred = pred)
  acc <- mean(pred == truth)
  auc <- as.numeric(pROC::auc(pROC::roc(truth, prob, quiet = TRUE,
                                        levels = c("B", "M"), direction = "<")))
  cat("\n==", label, "==\n")
  print(cm)
  cat(sprintf("Accuracy: %.4f\nAUC:      %.4f\n", acc, auc))
}

# 1. Logistic regression: radius_mean + texture_mean
fit1 <- glm(diagnosis ~ radius_mean + texture_mean,
            data = train, family = binomial)
print(summary(fit1))
prob1 <- predict(fit1, newdata = test, type = "response")
evaluate("Logistic (radius_mean + texture_mean)", test$diagnosis, prob1)

# 2. Logistic regression: all features
fit2 <- suppressWarnings(
  glm(diagnosis ~ ., data = train, family = binomial)
)
prob2 <- suppressWarnings(predict(fit2, newdata = test, type = "response"))
evaluate("Logistic (all features)", test$diagnosis, prob2)

# 3. XGBoost on all features
feature_cols <- setdiff(names(bdiag), "diagnosis")
x_train <- as.matrix(train[, feature_cols])
x_test  <- as.matrix(test[, feature_cols])
y_train <- as.integer(train$diagnosis) - 1L
y_test  <- as.integer(test$diagnosis)  - 1L

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest  <- xgb.DMatrix(data = x_test,  label = y_test)

params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss",
  eta = 0.1,
  max_depth = 4
)
fit3 <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 20,
  verbose = 0
)
prob3 <- predict(fit3, dtest)
evaluate("XGBoost (all features)", test$diagnosis, prob3)
