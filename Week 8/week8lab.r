library(dplyr)

# EXAM STEP 1: Load dataset
# (Use this if asked to work with gradadmit.csv)
grad <- read.csv("Week 8/gradadmit.csv")

# EXAM STEP 2: Inspect structure + summary
str(grad)
summary(grad)

# EXAM STEP 3: Convert categorical variables to factor
grad$rank <- factor(grad$rank)

# EXAM STEP 4: Ensure outcome is factor for classification
grad$admit <- factor(grad$admit)

str(grad)

# EXAM STEP 5: Split into training (80%) and test (20%)
set.seed(100)

train_idx <- sample(1:nrow(grad), size = 0.8 * nrow(grad))

train.data <- grad[train_idx, ]
test.data  <- grad[-train_idx, ]

# EXAM STEP 6: Logistic regression model
# (Use glm() with family="binomial" for binary classification)

model <- glm(admit ~ gre + gpa + rank,
             data = train.data,
             family = "binomial")

summary(model)

# EXAM STEP 7: Predicted probabilities on test set
prob <- predict(model, newdata = test.data, type = "response")

# EXAM STEP 8: Convert probabilities to class labels
pred <- ifelse(prob > 0.5, 1, 0)

# EXAM STEP 9: Convert predictions to factor for confusion matrix
pred <- factor(pred, levels = c(0,1))

# EXAM STEP 10: Confusion matrix + accuracy
table(Predicted = pred, Actual = test.data$admit)

mean(pred == test.data$admit)
