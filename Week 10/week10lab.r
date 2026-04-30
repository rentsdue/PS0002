library(dplyr) # Load dplyr for data manipulation and piping.
library(class) # Load class package for kNN classification.
library(e1071) # Load e1071 package for SVM and tuning helpers.
library(survival) # Load survival package that contains the rotterdam dataset.

# EXAM STEP 1: Load dataset from package
data(rotterdam, package = "survival") # Load rotterdam data from the survival package.

# EXAM STEP 2: Explore and prepare data
str(rotterdam) # Inspect variable types and overall structure.
summary(rotterdam) # Inspect summary statistics and value distributions.

# EXAM STEP 3: Keep only alive patients (death = 0)
rot_alive <- rotterdam %>%
  filter(death == 0) # Keep only observations where patients are alive.

# EXAM STEP 4: Convert stuff
rot_alive$recur <- factor(rot_alive$recur) # Convert recurrence outcome to factor for classification.
rot_alive$size <- as.numeric(rot_alive$size) # Convert size feature to numeric values.

# EXAM STEP 5: Select relevant predictors + outcome

rot_df <- rot_alive %>%
  select(recur, age, size, grade, nodes, pgr, er) # Keep selected predictors plus outcome.

# EXAM STEP 6: Split data into training (80%) / test (20%)
set.seed(100) # Set seed for reproducible random split.
train_idx <- sample(1:nrow(rot_df), size = 0.8 * nrow(rot_df)) # Sample indices for training rows.
train.data <- rot_df[train_idx, ] # Create training set from sampled rows.
test.data <- rot_df[-train_idx, ] # Create test set from remaining rows.

# ---------------------------
# EXAM STEP 7: Logistic Regression (LR)
# - Use summary() to find significant predictors (p < 0.05)
# - Positive coefficient => higher relapse risk
# ---------------------------
lr_model <- glm(recur ~ ., data = train.data, family = "binomial") # Fit logistic regression on training data.
summary(lr_model) # Print logistic regression summary and p-values.

# Predicted probabilities and classes (threshold = 0.5)
lr_prob <- predict(lr_model, newdata = test.data, type = "response") # Predict recurrence probabilities on test set.
lr_pred <- ifelse(lr_prob > 0.5, 1, 0) # Convert probabilities to class labels using threshold 0.5.
lr_pred <- factor(lr_pred, levels = c(0, 1)) # Convert predicted labels to factor with fixed levels.

lr_cm <- table(Predicted = lr_pred, Actual = test.data$recur) # Build confusion matrix for logistic predictions.
lr_acc <- mean(lr_pred == test.data$recur) # Compute logistic test accuracy.

lr_cm # Display logistic confusion matrix.

coef(lr_model) # Display logistic regression coefficients.

# ---------------------------
# EXAM STEP 8: kNN (requires normalization)
# ---------------------------
nor <- function(x) {
  x <- as.numeric(x) # Ensure input is numeric before scaling.
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE) # Compute range of the vector.
  if (!is.finite(rng) || rng == 0) return(rep(0, length(x))) # Handle invalid/constant vectors safely.
  (x - min(x, na.rm = TRUE)) / rng # Apply min-max normalization to [0,1].
} # End normalization function.

rot_knn <- rot_df # Copy selected dataset for kNN preprocessing.

# # size is an ordered category in this dataset. Convert it to numeric rank for kNN.
# rot_knn$size <- as.numeric(factor(rot_knn$size, levels = c("<=20", "20-50", ">50")))

rot_knn[, c("age", "size", "grade", "nodes", "pgr", "er")] <-
  lapply(rot_knn[, c("age", "size", "grade", "nodes", "pgr", "er")], nor) # Normalize numeric predictor columns.

# kNN cannot handle missing values.
rot_knn <- rot_knn[complete.cases(rot_knn), ] # Remove rows containing missing values.

set.seed(100) # Set seed for reproducible kNN train/test split.
train_idx_knn <- sample(1:nrow(rot_knn), size = 0.8 * nrow(rot_knn)) # Sample 80% indices for kNN training.
train_knn <- rot_knn[train_idx_knn, ] # Create kNN training set.
test_knn <- rot_knn[-train_idx_knn, ] # Create kNN test set.

sum(is.na(train_knn)) # Count missing values in kNN training data.
sum(is.na(test_knn)) # Count missing values in kNN test data.

# EXAM STEP 9: Choose best k (loop over k and pick max accuracy)
acc_k <- numeric(30) # Initialize vector to store kNN accuracy for k=1..30.
for (k in 1:30) {
  set.seed(101) # Reset seed for consistent tie-breaking behavior.
  knn_pred <- knn(
    train = train_knn[, -1], # Use all predictor columns (exclude outcome) for training.
    test = test_knn[, -1], # Use all predictor columns (exclude outcome) for testing.
    cl = train_knn$recur, # Supply training class labels.
    k = k # Use current neighbor count from loop.
  )
  acc_k[k] <- mean(knn_pred == test_knn$recur) # Save accuracy for this k.
}

plot(acc_k, type = "b", xlab = "k", ylab = "Accuracy") # Plot accuracy trend across k values.

best_k <- which.max(acc_k) # Find k index that gives highest accuracy.
best_k # Print best-performing k value.

set.seed(100) # Set seed before final kNN model run.
knn_best <- knn(
  train = train_knn[, -1], # Use training predictors for final kNN model.
  test = test_knn[, -1], # Use test predictors for final evaluation.
  cl = train_knn$recur, # Use training labels for final model.
  k = best_k # Use selected best k value.
)

knn_cm <- table(Predicted = knn_best, Actual = test_knn$recur) # Build confusion matrix for final kNN predictions.
knn_acc <- mean(knn_best == test_knn$recur) # Compute final kNN accuracy.

knn_cm # Display final kNN confusion matrix.
knn_acc # Display final kNN accuracy.

# ---------------------------
# EXAM STEP 10: SVM (Linear + Tuned Radial)
# - Linear kernel first
# - Then tune radial kernel using cost + gamma
# ---------------------------

# Linear SVM
svm_linear <- svm(recur ~ ., data = train_knn, kernel = "linear") # Train linear-kernel SVM classifier.
svm_pred_lin <- predict(svm_linear, newdata = test_knn) # Predict test labels using linear SVM.

svm_lin_cm <- table(Predicted = svm_pred_lin, Actual = test_knn$recur) # Build confusion matrix for linear SVM.
svm_lin_acc <- mean(svm_pred_lin == test_knn$recur) # Compute linear SVM accuracy.

svm_lin_cm # Display linear SVM confusion matrix.
svm_lin_acc # Display linear SVM accuracy.

# Tuned radial SVM (nonlinear kernel)
set.seed(123) # Set seed for reproducible parameter tuning.
svm_tune <- tune.svm(
  recur ~ ., data = train_knn, kernel = "radial", # Tune radial-kernel SVM on training data.
  cost = 10^(-1:2), gamma = c(0.1, 0.5, 1, 2) # Search over candidate cost and gamma values.
)

best_svm <- svm_tune$best.model # Extract best tuned SVM model.
svm_pred_rad <- predict(best_svm, newdata = test_knn) # Predict test labels using tuned radial SVM.

svm_rad_cm <- table(Predicted = svm_pred_rad, Actual = test_knn$recur) # Build confusion matrix for radial SVM.
svm_rad_acc <- mean(svm_pred_rad == test_knn$recur) # Compute radial SVM accuracy.

svm_rad_cm # Display radial SVM confusion matrix.
svm_rad_acc # Display radial SVM accuracy.

# ---------------------------
# EXAM STEP 11: Performance comparison
# - Accuracy, FPR, FNR
# ---------------------------
calc_metrics <- function(cm) {
  tp <- cm["1", "1"] # True positives: predicted 1 and actual 1.
  tn <- cm["0", "0"] # True negatives: predicted 0 and actual 0.
  fp <- cm["1", "0"] # False positives: predicted 1 but actual 0.
  fn <- cm["0", "1"] # False negatives: predicted 0 but actual 1.
  acc <- (tp + tn) / sum(cm) # Compute accuracy from confusion matrix counts.
  fpr <- fp / (fp + tn) # Compute false positive rate.
  fnr <- fn / (fn + tp) # Compute false negative rate.
  data.frame(Accuracy = acc, FPR = fpr, FNR = fnr) # Return metrics as one-row data frame.
} # End helper function definition.

results <- rbind(
  LR = calc_metrics(lr_cm), # Add logistic regression metrics.
  kNN = calc_metrics(knn_cm), # Add kNN metrics.
  SVM_Linear = calc_metrics(svm_lin_cm), # Add linear SVM metrics.
  SVM_Radial = calc_metrics(svm_rad_cm) # Add radial SVM metrics.
)

results # Display comparison table of raw metrics.

# Explicit false positive / false negative rates (in %)
rate_table <- results %>%
  mutate(
    Accuracy = round(Accuracy * 100, 2), # Convert accuracy to percentage with 2 decimals.
    FPR = round(FPR * 100, 2), # Convert false positive rate to percentage.
    FNR = round(FNR * 100, 2) # Convert false negative rate to percentage.
  )

rate_table # Display percentage-formatted performance table.




