## Lab 7 task
# Set working directory so the CSV path resolves correctly.

# Read the graduate admissions dataset into a data frame.
grad <- read.csv("Week 8/gradadmit.csv", header = TRUE, sep = ",")

# Inspect dataset dimensions.
dim(grad)
# Summary statistics for each variable.
summary(grad)
# Structure (types and preview) of the dataset.
str(grad)

# Convert admit and rank into categorical variables (factors).
# factor(rank) allows comparisons between any two rank levels.
grad$admit <- factor(grad$admit)
grad$rank <- factor(grad$rank)
str(grad)

# Split data into training and testing sets.
set.seed(100)
training.idx <- sample(1:nrow(grad), size = nrow(grad) * 0.8)
train.data <- grad[training.idx, ]
test.data <- grad[-training.idx, ]

# Fit a logistic regression model on the training data.
mlogit <- glm(admit ~ ., data = train.data, family = "binomial")
# Display model summary.
summary(mlogit)

# Based on the training data, the fitted logistic regression shows GPA and rank
# are significant variables, while GRE is not significant.

# Odds ratios for a 1-unit change in each variable.
exp(coef(mlogit))
# Interpretation: 1-unit changes in non-significant variable GRE makes almost
# no changes in the P(Y=1). 1-unit increase in GPA doubles the odds of being
# admitted. Odds of being admitted for applicants from rank 2 (rank 3, rank 4)
# institutions is 41% (23%, 17%) of that for applicants from the highest
# prestige institution, respectively.

####################
# Note: because factor(rank) is used and rank has 4 levels, R treats such a
# categorical variable by generating 3 dummy (binary) variables: rank2, rank3,
# rank4, where level 1 is a baseline level. For i = 2,3,4, ranki = 1 (if rank=i)
# and = 0 (otherwise). R uses these 3 dummy variables to replace the original
# variable rank in the model. The estimated coefficient -0.89 of rank2 represents
# how ln(odds) changes when rank goes from the baseline level (level 1) to level 2.
# exp(-0.89) = 41% indicates that odds of level 2 is 41% of that of level 1,
# i.e., odds decreases by 59% from level 1 to level 2. Similar for rank3 and rank4.
# In this way, the comparison between any two rank levels can be done. If one
# directly uses variable rank instead of factor(rank) in the model, such
# comparisons cannot be done from the output.

# Predicted probability P(Y=1) for the test data.
Pred.p <- predict(mlogit, newdata = test.data, type = "response")
# Convert probabilities to predicted class labels (0/1) using 0.5 threshold.
y_pred_num <- ifelse(Pred.p > 0.5, 1, 0)
# Convert numeric predictions to factor with explicit levels.
y_pred <- factor(y_pred_num, levels = c(0, 1))
# Compute overall accuracy.
mean(y_pred == test.data$admit)
# Confusion matrix of predictions vs actual labels.
conf_mat <- table(predicted = y_pred, actual = test.data$admit)
conf_mat

# Calculate false positive rate and recall (true positive rate).
true_negative <- conf_mat["0", "0"]
false_positive <- conf_mat["1", "0"]
false_negative <- conf_mat["0", "1"]
true_positive <- conf_mat["1", "1"]
false_positive_rate <- false_positive / (false_positive + true_negative)
recall <- true_positive / (true_positive + false_negative)
cat("False positive rate:", false_positive_rate, "\n")
cat("Recall (TPR):", recall, "\n")

# Based on this logistic regression model, 67.5% of applicants are correctly
# classified.