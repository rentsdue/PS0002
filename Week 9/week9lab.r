library(dplyr) # Load dplyr for data manipulation helpers.
library(class) # Load class package to use the knn() classifier.

# EXAM STEP 1: Load dataset # Label this section as step 1 of the workflow.
grad <- read.csv("Week 8/gradadmit.csv") # Read the graduate admission CSV into a data frame named grad.

# EXAM STEP 2: Inspect structure + summary # Label this section as data inspection.
str(grad) # Show the structure (column types and preview) of grad.
summary(grad) # Show summary statistics/frequencies for each column.

# EXAM STEP 3: Convert categorical variables to factor # Label this section as type conversion.
grad$rank <- factor(grad$rank) # Convert rank from numeric/integer to categorical factor.

# EXAM STEP 4: Ensure outcome is factor for classification # Label this section as target conversion.
grad$admit <- factor(grad$admit) # Convert admit to factor so models treat it as class labels.

# EXAM STEP 5: Normalize numeric predictors for kNN (distance-based) # Label this section as normalization.
nor <- function(x) { # Define a min-max normalization function that scales values to [0, 1].
  (x - min(x)) / (max(x) - min(x)) # Compute normalized value for each element of x.
} # End normalization function definition.

grad_norm <- grad # Create a copy of grad to store normalized predictor values.
grad_norm[, c("gre", "gpa")] <- lapply(grad_norm[, c("gre", "gpa")], nor) # Apply normalization to gre and gpa columns.

# EXAM STEP 6: Split into training (80%) and test (20%) # Label this section as train/test split.
set.seed(100) # Fix random seed so sampling is reproducible.

train_idx <- sample(1:nrow(grad_norm), size = 0.8 * nrow(grad_norm)) # Randomly sample row indices for 80% training data.

train.data <- grad_norm[train_idx, ] # Build training set from sampled indices.
test.data  <- grad_norm[-train_idx, ] # Build test set from remaining indices.

# --------------------------- # Visual separator line for readability.
# EXAM STEP 7: Logistic regression (baseline comparison) # Label this section as logistic baseline.
# --------------------------- # Visual separator line for readability.

model <- glm(admit ~ gre + gpa + rank, # Fit logistic regression predicting admit from gre, gpa, and rank.
             data = train.data, # Use training data for model fitting.
             family = "binomial") # Specify binomial family for logistic regression.

summary(model) # Display fitted logistic regression coefficients and diagnostics.

# Predicted probabilities # Label next line as probability prediction.
prob <- predict(model, newdata = test.data, type = "response") # Predict admit probabilities on test data.

# Convert to class (threshold = 0.5) # Label next line as threshold-based classification.
pred <- ifelse(prob > 0.5, 1, 0) # Convert probabilities to class 1 if > 0.5, else class 0.

# Convert to factor to match actual values # Label next line as factor alignment.
pred <- factor(pred, levels = c(0,1)) # Convert predicted classes to factor with levels 0 and 1.

# Confusion matrix + accuracy # Label next outputs as performance metrics.
table(Predicted = pred, Actual = test.data$admit) # Create confusion matrix for logistic predictions vs actual labels.

mean(pred == test.data$admit) # Compute logistic model accuracy on test set.

# --------------------------- # Visual separator line for readability.
# EXAM STEP 8: kNN classification # Label this section as first kNN run.
# --------------------------- # Visual separator line for readability.

# Start with a chosen k (simple version) # Label next block as initial kNN with fixed k.
set.seed(101) # Set random seed for reproducible tie-breaking behavior.
knn_pred <- knn( # Run k-nearest neighbors classification.
  train = train.data[, c("gre", "gpa", "rank")], # Provide predictor columns from training data.
  test  = test.data[, c("gre", "gpa", "rank")], # Provide predictor columns from test data.
  cl    = train.data$admit, # Provide training class labels.
  k     = 5 # Set number of neighbors to 5.
) # End kNN function call.

table(Predicted = knn_pred, Actual = test.data$admit) # Create confusion matrix for kNN predictions.
mean(knn_pred == test.data$admit) # Compute initial kNN accuracy.

# EXAM STEP 9: Try different k values to find best accuracy # Label this section as k tuning.
acc <- numeric(30) # Create numeric vector to store accuracy for k = 1 to 30.
for (i in 1:30) { # Loop through candidate k values from 1 to 30.
  set.seed(101) # Reset seed each iteration for consistent random behavior.
  knn_i <- knn( # Run kNN using current k value i.
    train = train.data[, c("gre", "gpa", "rank")], # Use same training predictors.
    test  = test.data[, c("gre", "gpa", "rank")], # Use same test predictors.
    cl    = train.data$admit, # Use same training labels.
    k     = i # Set k to current loop value.
  ) # End kNN call for this iteration.
  acc[i] <- mean(knn_i == test.data$admit) # Store test accuracy at position i.
  cat("k =", i, " accuracy =", acc[i], "\n") # Print k and corresponding accuracy.
} # End loop over candidate k values.

# Plot accuracy vs k (choose the best k) # Label next line as model selection visualization.
plot(acc, type = "b", xlab = "K", ylab = "Accuracy") # Plot accuracy across k values using points and lines.

# Best k value # Label next section as final chosen-k evaluation.
# Start with a chosen k (simple version) # Label next block as final kNN run.
set.seed(109) # Set seed before final model run for reproducibility.
knn_pred <- knn( # Run kNN again with selected k.
  train = train.data[, c("gre", "gpa", "rank")], # Use training predictors for final model.
  test  = test.data[, c("gre", "gpa", "rank")], # Use test predictors for final evaluation.
  cl    = train.data$admit, # Use training labels for final model.
  k     = 4 # Set selected best k value to 4.
) # End final kNN call.

table(Predicted = knn_pred, Actual = test.data$admit) # Show confusion matrix for final kNN predictions.
mean(knn_pred == test.data$admit) # Compute final kNN test accuracy.


