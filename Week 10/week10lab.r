library(dplyr)
library(class)
library(e1071)
library(survival)

# EXAM STEP 1: Load dataset from package
data(rotterdam, package = "survival")

# EXAM STEP 2: Explore and prepare data
str(rotterdam)
summary(rotterdam)

# EXAM STEP 3: Keep only alive patients (death = 0)
rot_alive <- rotterdam %>%
  filter(death == 0)

# EXAM STEP 4: Convert outcome to factor
rot_alive$recur <- factor(rot_alive$recur)

# EXAM STEP 5: Select relevant predictors + outcome
rot_df <- rot_alive %>%
  select(recur, age, size, grade, nodes, pgr, er)

# EXAM STEP 6: Split data into training (80%) / test (20%)
set.seed(100)
train_idx <- sample(1:nrow(rot_df), size = 0.8 * nrow(rot_df))
train.data <- rot_df[train_idx, ]
test.data <- rot_df[-train_idx, ]

# ---------------------------
# EXAM STEP 7: Logistic Regression (LR)
# - Use summary() to find significant predictors (p < 0.05)
# - Positive coefficient => higher relapse risk
# ---------------------------
lr_model <- glm(recur ~ ., data = train.data, family = "binomial")
summary(lr_model)

# Predicted probabilities and classes (threshold = 0.5)
lr_prob <- predict(lr_model, newdata = test.data, type = "response")
lr_pred <- ifelse(lr_prob > 0.5, 1, 0)
lr_pred <- factor(lr_pred, levels = c(0, 1))

lr_cm <- table(Predicted = lr_pred, Actual = test.data$recur)
lr_acc <- mean(lr_pred == test.data$recur)

lr_cm
lr_acc

# ---------------------------
# EXAM STEP 8: kNN (requires normalization)
# ---------------------------
nor <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

rot_knn <- rot_df
rot_knn[, c("age", "grade", "nodes", "pgr", "er")] <-
  lapply(rot_knn[, c("age", "grade", "nodes", "pgr", "er")], nor)

# size is a factor (<=20, 20-50, >50). Convert to numeric rank for kNN.
rot_knn$size <- as.numeric(factor(rot_knn$size, levels = c("<=20", "20-50", ">50")))

set.seed(100)
train_knn <- rot_knn[train_idx, ]
test_knn <- rot_knn[-train_idx, ]

# EXAM STEP 9: Choose best k (loop over k and pick max accuracy)
acc_k <- numeric(30)
for (k in 1:30) {
  set.seed(101)
  knn_pred <- knn(
    train = train_knn[, -1],
    test = test_knn[, -1],
    cl = train_knn$recur,
    k = k
  )
  acc_k[k] <- mean(knn_pred == test_knn$recur)
}

plot(acc_k, type = "b", xlab = "k", ylab = "Accuracy")

best_k <- which.max(acc_k)
best_k

set.seed(101)
knn_best <- knn(
  train = train_knn[, -1],
  test = test_knn[, -1],
  cl = train_knn$recur,
  k = best_k
)

knn_cm <- table(Predicted = knn_best, Actual = test_knn$recur)
knn_acc <- mean(knn_best == test_knn$recur)

knn_cm
knn_acc

# ---------------------------
# EXAM STEP 10: SVM (Linear + Tuned Radial)
# - Linear kernel first
# - Then tune radial kernel using cost + gamma
# ---------------------------

# Linear SVM
svm_linear <- svm(recur ~ ., data = train_knn, kernel = "linear")
svm_pred_lin <- predict(svm_linear, newdata = test_knn)

svm_lin_cm <- table(Predicted = svm_pred_lin, Actual = test_knn$recur)
svm_lin_acc <- mean(svm_pred_lin == test_knn$recur)

svm_lin_cm
svm_lin_acc

# Tuned radial SVM (nonlinear kernel)
set.seed(123)
svm_tune <- tune.svm(
  recur ~ ., data = train_knn, kernel = "radial",
  cost = 10^(-1:2), gamma = c(0.1, 0.5, 1, 2)
)

best_svm <- svm_tune$best.model
svm_pred_rad <- predict(best_svm, newdata = test_knn)

svm_rad_cm <- table(Predicted = svm_pred_rad, Actual = test_knn$recur)
svm_rad_acc <- mean(svm_pred_rad == test_knn$recur)

svm_rad_cm
svm_rad_acc

# ---------------------------
# EXAM STEP 11: Performance comparison
# - Accuracy, FPR, FNR
# ---------------------------
calc_metrics <- function(cm) {
  tp <- cm["1", "1"]
  tn <- cm["0", "0"]
  fp <- cm["1", "0"]
  fn <- cm["0", "1"]
  acc <- (tp + tn) / sum(cm)
  fpr <- fp / (fp + tn)
  fnr <- fn / (fn + tp)
  data.frame(Accuracy = acc, FPR = fpr, FNR = fnr)
}

results <- rbind(
  LR = calc_metrics(lr_cm),
  kNN = calc_metrics(knn_cm),
  SVM_Linear = calc_metrics(svm_lin_cm),
  SVM_Radial = calc_metrics(svm_rad_cm)
)

results
