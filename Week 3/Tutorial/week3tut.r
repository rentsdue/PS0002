# Use library() instead of import 
library(dplyr)
library(nycflights13)

# The following functions are from your tutorial notes [cite: 44, 45, 46, 47]
print(select(flights, year, month, day))
select(flights, year:day) 
select(flights, -(year:day))
select(flights, ends_with("time"))

print(filter(flights, month==1, day>=16))