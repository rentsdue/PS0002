library(dplyr) # Load dplyr for data manipulation functions.

# EXAM STEP 1: Load dataset
# (Use this if asked to work with gradadmit.csv)
grad <- read.csv("Week 8/gradadmit.csv") # Read the gradadmit CSV file into grad.

# EXAM STEP 2: Inspect structure + summary
str(grad) # Display data structure, including variable types.
summary(grad) # Display summary statistics/frequencies for each column.

# EXAM STEP 3: Convert categorical variables to factor
grad$rank <- factor(grad$rank) # Convert rank into factor for categorical modeling.

# EXAM STEP 4: Ensure outcome is factor for classification
grad$admit <- factor(grad$admit) # Convert admit into factor for binary classification.

str(grad) # Re-check structure to confirm conversions.

# EXAM STEP 5: Split into training (80%) and test (20%)
set.seed(100) # Set seed to make the random split reproducible.

train_idx <- sample(1:nrow(grad), size = 0.8 * nrow(grad)) # Sample row indices for 80% training data.

train.data <- grad[train_idx, ] # Create training subset from sampled rows.
test.data  <- grad[-train_idx, ] # Create test subset from remaining rows.

# EXAM STEP 6: Logistic regression model
# (Use glm() with family="binomial" for binary classification)

model <- glm(admit ~ gre + gpa + rank, # Fit logistic model predicting admit from gre, gpa, and rank.
             data = train.data, # Use training dataset for model fitting.
             family = "binomial") # Specify binomial family for logistic regression.

summary(model) # Show model coefficients and statistical significance.

# EXAM STEP 7: Predicted probabilities on test set
prob <- predict(model, newdata = test.data, type = "response") # Predict admission probabilities on test data.

# EXAM STEP 8: Convert probabilities to class labels
pred <- ifelse(prob > 0.5, 1, 0) # Convert probabilities to 0/1 classes using threshold 0.5.

# EXAM STEP 9: Convert predictions to factor for confusion matrix
pred <- factor(pred, levels = c(0,1)) # Convert predictions to factor with consistent class levels.

# EXAM STEP 10: Confusion matrix + accuracy
table(Predicted = pred, Actual = test.data$admit) # Build confusion matrix comparing predictions with actual labels.

mean(pred == test.data$admit) # Calculate test accuracy as proportion of correct predictions.
