# ==========================================
# STEP 1: SETUP & DATA
# ==========================================
library(MASS)   # Load MASS package, which provides the Boston dataset.
library(caret)  # Load caret package for model training and evaluation utilities.

data("Boston") # Load the Boston housing dataset into the current R session.

# ==========================================
# STEP 2: SPLIT THE DATA (80/20)
# ==========================================
set.seed(100) # Set random seed so the data split is reproducible.
train_idx <- sample(1:nrow(Boston), 0.8 * nrow(Boston)) # Randomly choose 80% of row indices for training.

train_data <- Boston[train_idx, ] # Create training dataset using sampled indices.
test_data  <- Boston[-train_idx, ] # Create test dataset using the remaining indices.

# ==========================================
# STEP 3: TRAIN THE kNN MODEL
# ==========================================
# We use crim ~ . to predict crime using all other variables
# preProcess is MANDATORY for kNN distance calculations
set.seed(101) # Set seed for reproducible cross-validation and tuning results.
knn_model <- train( # Train a kNN regression model using caret.
  crim ~ ., # Model formula: predict crim using all other variables.
  data = train_data, # Use the training dataset for fitting.
  method = "knn", # Specify k-nearest neighbors algorithm.
  trControl = trainControl(method = "cv", number = 10), # Use 10-fold cross-validation during training.
  preProcess = c("center", "scale"), # Standardize predictors to zero mean and unit variance.
  tuneLength = 10 # Try 10 different k values and pick the best one.
) # End model training call.

# ==========================================
# STEP 4: VISUALIZE & FIND BEST K
# ==========================================
# This plot shows you which 'k' had the lowest error (RMSE)
plot(knn_model) # Plot model performance across candidate k values.

# This prints the specific 'k' value R chose as the winner
knn_model$bestTune # Display the best-tuned k value selected by caret.

# ==========================================
# STEP 5: TEST THE MODEL
# ==========================================
# Predict crime rates for the 20% of data the model hasn't seen
predictions <- predict(knn_model, test_data) # Generate crime-rate predictions on the test set.

# Calculate the final RMSE (The "Average Mistake" in crime rate units)
final_rmse <- RMSE(predictions, test_data$crim) # Compute RMSE between predicted and actual crim values.
print(paste("The Average Prediction Error (RMSE) is:", round(final_rmse, 2))) # Print rounded RMSE as final error metric.

# ==========================================
# STEP 6: PERFORMANCE PLOT
# ==========================================
# Plot Actual vs. Predicted values. 
# A perfect model would have all points on the red line.
plot(test_data$crim, predictions, # Create scatter plot of actual vs predicted crime rates.
     main = "kNN Performance: Predicted vs Actual Crime", # Set plot title.
     xlab = "Actual Crime Rate", # Label x-axis as actual values.
     ylab = "Predicted Crime Rate", # Label y-axis as predicted values.
     pch = 19, col = "darkblue") # Use solid dark-blue points for visibility.
abline(0, 1, col = "red", lwd = 2) # Draw y=x reference line for perfect predictions.


