# ==============================================================================
# Practice Question: Classification with LR, kNN, and SVM (R)
# Dataset: PimaIndiansDiabetes (mlbench) — binary classification
# ==============================================================================

# ------------------------------------------------------------------------------
# Part A: Setup & Data Preparation (implementation only)
# ------------------------------------------------------------------------------
# 1) Install and load the mlbench package, then load the PimaIndiansDiabetes
#    dataset into a data frame called df.
library(mlbench)
library(dplyr)
data(PimaIndiansDiabetes)
df <- PimaIndiansDiabetes

# 2) Inspect the structure and summary of df.
str(df)
summary(df)

# 3) Split df into 80% training and 20% testing (set.seed(123)).
set.seed(123)
training.idx <- sample(1:nrow(df), size = nrow(df) * 0.8)
train.data <- df[training.idx, ]
test.data <- df[-training.idx, ]

# 4) Normalize/scale numeric predictors for kNN and SVM.
nor <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
df1 <- df %>% mutate(across(where(is.numeric), nor))

# ------------------------------------------------------------------------------
# Part B: Logistic Regression (binary) - implementation only
# ------------------------------------------------------------------------------
# 5) Fit a logistic regression model to predict the diabetes outcome.
mlogit <- glm(diabetes ~ ., data = train.data, family = "binomial")
# Display model summary.
summary(mlogit)

# 6) Predict probabilities on the test set and convert to class labels.
# Predicted probability P(Y=1) for the test data.

Pred.p <- predict(mlogit, newdata = test.data, type = "response")
# Convert probabilities to predicted class labels (0/1) using 0.5 threshold.
y_pred_num <- ifelse(Pred.p > 0.5, 1, 0)
# Convert numeric predictions to factor with explicit levels.
test.data$diabetes <- factor(test.data$diabetes, levels = c(0, 1))
y_pred <- factor(y_pred_num, levels = c(0, 1))   # no labels


# 7) Compute accuracy and a confusion matrix.
mean(y_pred == test.data$diabetes)
conf_mat <- table(predicted = y_pred, actual = test.data$diabetes)
conf_mat

# 8) Compute false positive rate and recall (TPR) from the confusion matrix.
true_negative <- conf_mat["0", "0"]
false_positive <- conf_mat["1", "0"]
false_negative <- conf_mat["0", "1"]
true_positive <- conf_mat["1", "1"]
false_positive_rate <- false_positive / (false_positive + true_negative)
recall <- true_positive / (true_positive + false_negative)
cat("False positive rate:", false_positive_rate, "\n")
cat("Recall (TPR):", recall, "\n")
# ------------------------------------------------------------------------------
# Part C: k-Nearest Neighbors (kNN) - implementation only
# ------------------------------------------------------------------------------
# 9) Train kNN models for k = 1 to 30 and store the accuracy for each k.
# 10) Plot accuracy vs k (line plot) and identify the best k from the graph.
# 11) Refit kNN using the best k and compute accuracy, confusion matrix,
#     false positive rate, and recall.
#
# ------------------------------------------------------------------------------
# Part D: Support Vector Machine (SVM) - implementation only
# ------------------------------------------------------------------------------
# 12) Train an SVM classifier with a linear kernel.
# 13) Predict class labels on the test set.
# 14) Compute accuracy, confusion matrix, false positive rate, and recall.
# 15) (Optional) Train an SVM with a radial kernel and compare accuracy.
#
# ------------------------------------------------------------------------------
# Part E: Final Comparison (implementation summary)
# ------------------------------------------------------------------------------
# 16) Create a short results table comparing LR, kNN (best k), and SVM accuracy.
# 17) Identify which model has the highest recall and explain why.