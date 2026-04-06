library(dplyr)
library(class)

# EXAM STEP 1: Load dataset
grad <- read.csv("Week 8/gradadmit.csv")

# EXAM STEP 2: Inspect structure + summary
str(grad)
summary(grad)

# EXAM STEP 3: Convert categorical variables to factor
grad$rank <- factor(grad$rank)

# EXAM STEP 4: Ensure outcome is factor for classification
grad$admit <- factor(grad$admit)

# EXAM STEP 5: Normalize numeric predictors for kNN (distance-based)
nor <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

grad_norm <- grad
grad_norm[, c("gre", "gpa")] <- lapply(grad_norm[, c("gre", "gpa")], nor)

# EXAM STEP 6: Split into training (80%) and test (20%)
set.seed(100)

train_idx <- sample(1:nrow(grad_norm), size = 0.8 * nrow(grad_norm))

train.data <- grad_norm[train_idx, ]
test.data  <- grad_norm[-train_idx, ]

# ---------------------------
# EXAM STEP 7: Logistic regression (baseline comparison)
# ---------------------------

model <- glm(admit ~ gre + gpa + rank,
             data = train.data,
             family = "binomial")

summary(model)

# Predicted probabilities
prob <- predict(model, newdata = test.data, type = "response")

# Convert to class (threshold = 0.5)
pred <- ifelse(prob > 0.5, 1, 0)

# Convert to factor to match actual values
pred <- factor(pred, levels = c(0,1))

# Confusion matrix + accuracy
table(Predicted = pred, Actual = test.data$admit)

mean(pred == test.data$admit)

# ---------------------------
# EXAM STEP 8: kNN classification
# ---------------------------

# Start with a chosen k (simple version)
set.seed(101)
knn_pred <- knn(
  train = train.data[, c("gre", "gpa", "rank")],
  test  = test.data[, c("gre", "gpa", "rank")],
  cl    = train.data$admit,
  k     = 5
)

table(Predicted = knn_pred, Actual = test.data$admit)
mean(knn_pred == test.data$admit)

# EXAM STEP 9: Try different k values to find best accuracy
acc <- numeric(30)
for (i in 1:30) {
  set.seed(101)
  knn_i <- knn(
    train = train.data[, c("gre", "gpa", "rank")],
    test  = test.data[, c("gre", "gpa", "rank")],
    cl    = train.data$admit,
    k     = i
  )
  acc[i] <- mean(knn_i == test.data$admit)
  cat("k =", i, " accuracy =", acc[i], "\n")
}

# Plot accuracy vs k (choose the best k)
plot(acc, type = "b", xlab = "K", ylab = "Accuracy")

# Best k value
# Start with a chosen k (simple version)
set.seed(109)
knn_pred <- knn(
  train = train.data[, c("gre", "gpa", "rank")],
  test  = test.data[, c("gre", "gpa", "rank")],
  cl    = train.data$admit,
  k     = 4
)

table(Predicted = knn_pred, Actual = test.data$admit)
mean(knn_pred == test.data$admit)

