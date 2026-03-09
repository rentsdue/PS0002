
# Setup
library(dplyr)

# Download and read data
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
download.file(url, destfile = "msleep_ggplot2.csv", mode = "wb")
msleep <- read.csv("msleep_ggplot2.csv")

# # Number of rows and columns
# print(dim(msleep))

# # Summary of all variables
# print(summary(msleep))

# # Show the first 10 rows using function “head()”
# print(head(msleep, 10))

# # Select a set of columns: the “name” and the “sleep_total” columns
# print(select(msleep, name, sleep_total))

# # Select all the columns except a specific column, use the “-“ (subtraction) operator
# print(select(msleep, -name))

# # Select a range of columns by name, use the “:” (colon) operator
# print(select(msleep, (name:order)))

# # Select all columns that start with the character string “sl”, use the function starts_with()
# print(select(msleep, starts_with("sl")))

# # Filter the rows for mammals that sleep a total of more than 16 hours
# print(filter(msleep, sleep_total > 16))

# # Filter the rows for mammals that sleep a total of more than 16 hours and have a body weight of greater than 1 kilogram
# print(filter(msleep, sleep_total > 16 & bodywt > 1))

# # Filter the rows for mammals with order as “Perissodactyla” and “Primates”
# print(filter(msleep, order == "Perissodactyla" | order == "Primates"))

# # Using pipe operator, Select two columns (name and sleep_total) and then show the first 6 rows of the selected data frame.
# print(msleep %>% select(name, sleep_total) %>% head(6))

# Select three columns from sleepdata, arrange the rows by the order and then arrange the rows by sleep_total. Finally filter the rows for mammals that sleep for 16 or more hours
print(msleep %>% select(name, order, sleep_total) %>% arrange(order, sleep_total) %>% filter(sleep_total > 16))

# Change the order of sleep_total column to a descending order using the function desc() in the step above
print(msleep %>% select(name, order, sleep_total) %>% arrange(order, desc(sleep_total)) %>% filter(sleep_total > 16))