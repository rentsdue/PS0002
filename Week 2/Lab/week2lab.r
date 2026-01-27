# Task 1: Create an R data frame “lab1” by importing the “lab1fixed.txt” file into the R

# Various columns
varnames <- c("id", "gender", "height", "weight", "siblings")
widthList <- c(3, 1, 3, 2, 1)
filePath <- "Week 2/Lab/lab1fixed.txt"

# Create/Print Table
lab1 <- read.fwf(file = filePath, header = F, width = widthList, col.names = varnames)
print(lab1)
print("Succesfully printed")

# Create image
png("Week 2/Lab/my_plot.png")
print("Succesfully Created")

# Create plot
plot(x = lab1$height, y = lab1$weight, xlab = "Height", ylab = "Weight")

# Close the device to save the file
dev.off()

# Task 2: Based on “lab1”, create an R data frame “lab1m” which contains the data for all the
# male subjects. How many males are there in the data frame “lab1”?
lab1m <- lab1[lab1$gender == "M",]

print(lab1m)
print(nrow(lab1m)) # Counts number of rows

# Import “lab1test” into R. Merge the two datasets “lab1” and “lab1test”. Let us call
# this new R data frame “lab1merge”. Identify individuals whose heights are greater
# than 182 cm. What are the test scores of subjects whose heights are greater than
# 182 cm?

# Create Table
lab1test <- read.table("Week 2/Lab/lab1test.txt", header = T, sep=" ")
lab1merge <- merge(lab1, lab1test)
print(lab1merge)
