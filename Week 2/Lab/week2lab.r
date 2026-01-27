# Task 1: Data Frame Manipulation in R

# Part 1: Create an R data frame “lab1” by importing the “lab1fixed.txt” file into the R

# Various columns
varnames <- c("id", "gender", "height", "weight", "siblings")
widthList <- c(3, 1, 3, 2, 1)
filePath <- "Week 2/Lab/lab1fixed.txt"

# Create/Print Table
lab1 <- read.fwf(file = filePath, header = F, width = widthList, col.names = varnames)
attach(lab1)
print(lab1)
print("Succesfully printed")

# Create image
png("Week 2/Lab/my_plot.png")
print("Succesfully Created")

# Create plot
plot(x = height, y = weight, xlab = "Height", ylab = "Weight")

# Close the device to save the file
dev.off()

# Part 2: Based on “lab1”, create an R data frame “lab1m” which contains the data for all the
# male subjects. How many males are there in the data frame “lab1”?
lab1m <- lab1[gender == "M",]

print(lab1m)
print(nrow(lab1m)) # Counts number of rows

# Part 3: Import “lab1test” into R. Merge the two datasets “lab1” and “lab1test”. Let us call
# this new R data frame “lab1merge”. Identify individuals whose heights are greater
# than 182 cm. What are the test scores of subjects whose heights are greater than
# 182 cm?

# Create Table
lab1test <- read.table("Week 2/Lab/lab1test.txt", header = T, sep=" ")
lab1merge <- merge(lab1, lab1test)
tallpeople <- lab1merge[height>182,]
attach(lab1merge)

# Part 4: Suppose that there was an error in the weight of the Subject 211 in the text file.
# Obtain a new R data frame “lab1remo” by removing the record related to the
# Subject 211 from the data

lab1remo <- lab1[id != 211, ]

# Part 5: After checking with the Subject 211, we found out that his actual weight should be
# 80kg instead of 60kg. Modify the R data frame “lab1” by rectifying the mistake

lab1[id == 211, "weight"] <- 80
print(lab1)

# Part 6: Who is the second tallest female in this group and what are her height, weight, and
# test score?
# 1. Create a data frame of only females from the MERGED data (so we have test scores)
lab1f <- lab1merge[lab1merge$gender == "F", ]

# 2. Sort the females by height in descending order
lab1f_sorted <- lab1f[order(lab1f$height, decreasing = TRUE), ]

# 3. Select the 2nd row (second tallest) and the specific columns needed
second_tallest_female <- lab1f_sorted[2, c("id", "height", "weight", "test")]

print(second_tallest_female)

# Task 2: Matrices
column1 <- c(1, 1, 1, 1)
column2 <- c(1, 3, 5, 7)
X <- cbind(column1, column2)
Y <- c(4, 6, 13, 20)

ans <- solve(t(X) %*% X) %*% t(X) %*% Y
print(ans)

# Task 3: Function to compute first two moments

# Import height data
height <- lab1["height"]
