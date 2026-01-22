#Creating a numeric vector with c (combine) function

value.num <- c(10, 20, 30, 40, 50)
print(value.num)

# Creating a character vector
value.char <- c("nigger", "banana", "spick", "faggot", "chink")
print(value.char)

# Creating a logical vector 
value.log <- c(T, F, T, T, F)
print(value.log)

# Creating a vector with a vector
value.num2 <- c(value.num,9,10)
print(value.num2)

# Numeric Vector
numeric_vector <- numeric(5)
print(numeric_vector)

# Repeating Values
value.rep <- rep(2,5)
print(value.rep)

# Combining Vectors (more)
value.combined <- c(rep(6, 7), 6, 7, 9, 11)
print(value.combined)

# Creating sequences
seq_vector <- seq(from<-6, to<-60, by<-3)
print(seq_vector)

seq_vector_2 <- seq(from<-7, to<-70, length<-2)
print(seq_vector_2)

# Intro to Matrices
matrix_test_vector <- c(1:6)*2
dim(matrix_test_vector) <- c(2,3)
print(matrix_test_vector)
dim(matrix_test_vector) <- NULL
print(matrix_test_vector)

# Matrices filled by turns/columns

# Turn wise (Column wise)
v <- c(1:8)
M <- matrix(v,2,4) # Vector, no of rows, no columns
print(M)

# Row wise
v <- c(1:8)*2
M <- matrix(v,2,4, byrow <- T) # Remember to include byrow <- T
print(M)

#Rbind/Cbind

rowA <- c(1, 2, 3)
rowB <- c(4, 5, 6)
rowC <- c(7, 8, 9)
M1 <- rbind(rowA, rowB, rowC)
M2 <- cbind(rowA, rowB, rowC)
print(M1)
print(M2)

# Transpose and inverse

row1 <- c(1, 6, 7)
row2 <- c(3, 5, 5)
row3 <- c(1, 2, 2)
M3 <- rbind(row1, row2, row3)

print(t(M3))  # Transpose
print(solve(M3)) # Solve: Inverse

# data.frame” function
# The function “data.frame” converts a matrix or a collection of
# vectors into a dataframe.

v <- c(1:8) * 2
m <- matrix(v,2,4)
df1 <- data.frame(m)
names(df1) <- c("Column 1", "Column 2", "Column 3", "Column 4") # Changes names of the column of data frame
row.names(df1) <- c("Row1","Row2") # Change row names
print(df1)

# Read functions
ex1.3 <- read.table("Week 2/ex1.3.txt", header = F)
print(ex1.3)

ex1.3.1 <- read.csv("Week 2/ex1.3comma.txt", header = F) # csv: when there are commas instead of spaces to demarcate different entries
print(ex1.3.1)

# Yields same results as above - uses comma separator instead of CSV
ex1.3c <- read.table("Week 2/ex1.3comma.txt", header = F, sep=",")
print(ex1.3c)

