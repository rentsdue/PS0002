# ==========================================
# STEP 1: SETUP & DATA
# ==========================================
library(MASS)   # Contains the Boston dataset
library(caret)  # The ML toolkit

data("Boston")

# ==========================================
# STEP 2: SPLIT THE DATA (80/20)
# ==========================================
set.seed(100) # Ensures you get the same "random" split every time
train_idx <- sample(1:nrow(Boston), 0.8 * nrow(Boston))

train_data <- Boston[train_idx, ]
test_data  <- Boston[-train_idx, ]

# ==========================================
# STEP 3: TRAIN THE kNN MODEL
# ==========================================
# We use crim ~ . to predict crime using all other variables
# preProcess is MANDATORY for kNN distance calculations
set.seed(101)
knn_model <- train(
  crim ~ ., 
  data = train_data, 
  method = "knn",
  trControl = trainControl(method = "cv", number = 10), # 10-fold Cross Validation
  preProcess = c("center", "scale"),                   # Normalization
  tuneLength = 10                                      # Try 10 different values of k
)

# ==========================================
# STEP 4: VISUALIZE & FIND BEST K
# ==========================================
# This plot shows you which 'k' had the lowest error (RMSE)
plot(knn_model)

# This prints the specific 'k' value R chose as the winner
knn_model$bestTune

# ==========================================
# STEP 5: TEST THE MODEL
# ==========================================
# Predict crime rates for the 20% of data the model hasn't seen
predictions <- predict(knn_model, test_data)

# Calculate the final RMSE (The "Average Mistake" in crime rate units)
final_rmse <- RMSE(predictions, test_data$crim)
print(paste("The Average Prediction Error (RMSE) is:", round(final_rmse, 2)))

# ==========================================
# STEP 6: PERFORMANCE PLOT
# ==========================================
# Plot Actual vs. Predicted values. 
# A perfect model would have all points on the red line.
plot(test_data$crim, predictions, 
     main = "kNN Performance: Predicted vs Actual Crime",
     xlab = "Actual Crime Rate", 
     ylab = "Predicted Crime Rate",
     pch = 19, col = "darkblue")
abline(0, 1, col = "red", lwd = 2)

