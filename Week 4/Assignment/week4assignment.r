# %%

library(ggplot2)

library(dplyr)

<<<<<<< HEAD
# View a descriptive summary for this dataset using function summary().
summary(msleep)

# Question 1: Create a new dataframe named as “df2”, which includes all variables in the msleep dataset excluding “name” and “genus”, for mammals with vore not being “NA”.

df2 <- msleep %>% filter(!is.na(vore)) %>% select(-name, -genus)

# Question 2: Add the following two new variables to the dataframe “df2”: “non_rem”, which is obtained by subtracting rem sleep time from total sleep time, “sleep_awake_ratio”, which is the ratio of sleep_total to awake.

df2 <- mutate(df2, non_rem = sleep_total - sleep_rem, sleep_awake_ratio = sleep_total / awake)

# Print the data to the console
df2

# Question 3: Find summary statistics, including the number of mammals, average of non_rem and average of sleep_awake_ratio over different vore groups. Based on the results you obtained, add a brief comment in the code using "#" on the comparison between vore groups
summary_data <- group_by(df2, vore) %>% summarise(n_mammals = n(), avg_non_rem = mean(non_rem, na.rm = TRUE), avg_sleep_awake_ratio = mean(sleep_awake_ratio, na.rm = TRUE))

# Print the data
summary_data

# Comment: The most common vore in the data set is herbivore (32), followed by omnivores (20), carnivores (19), and finally insectivores (5). 
# Omnivores and insectivores generally show higher average non_rem sleep (9.09 hours for omnivores and 13 hours for insectivores) than the other vores (7.37 hours for carnivores, 7.78 hours for herbivores). 
# For avg_sleep_awake_ratio, insectivores have the highest ratio (2.72), followed by carnivores (1.05). The two lowest avg_sleep_awake_ratio values come from herbivores (0.861) and omnivores (0.980).

# Question 4: Use an appropriate plot to compare the distributions of sleep_awake_ratio for different vore groups of mammals. Add a brief comment in the code using "#" to describe how the distribution of sleep_awake_ratio varies across different vore groups

ggplot(df2, aes(x = vore, y = sleep_awake_ratio)) + geom_boxplot() + labs(x = "Vore group", y = "Sleep / awake ratio")

# Comment: Insectivores show the greatest variability in sleep_awake_ratio amongst the vore groups, followed by herbivores (2nd most variability), and carnivores come in 3rd place for most variability.
# The vore group with least variability in sleep_awake_ratio is the omnivore group.

# Question 5: Use an appropriate plot to visualize the relationship between sleep_awake_ratio and non_rem. Include a comment in the code using "#" to describe the relationship observed in the plot.

ggplot(df2, aes(x = non_rem, y = sleep_awake_ratio)) + geom_point() + geom_smooth() + labs(x = "Non-REM sleep (hours)", y = "Sleep / awake ratio")

# Comment: There is a clear positive relationship. Mammals with higher non_rem sleep tend to have a higher sleep_awake_ratio.
=======


# View a descriptive summary for this dataset using function summary().

summary(msleep)



# Question 1: Create a new dataframe named as “df2”, which includes all variables in the msleep dataset excluding “name” and “genus”, for mammals with vore not being “NA”.



df2 <- msleep %>% filter(!is.na(vore)) %>% select(-name, -genus)



# Question 2: Add the following two new variables to the dataframe “df2”: “non_rem”, which is obtained by subtracting rem sleep time from total sleep time, “sleep_awake_ratio”, which is the ratio of sleep_total to awake.



df2 <- mutate(df2, non_rem = sleep_total - sleep_rem, sleep_awake_ratio = sleep_total / awake)



# Print the data to the console

df2



# Question 3: Find summary statistics, including the number of mammals, average of non_rem and average of sleep_awake_ratio over different vore groups. Based on the results you obtained, add a brief comment in the code using "#" on the comparison between vore groups

summary_data <- group_by(df2, vore) %>% summarise(n_mammals = n(), avg_non_rem = mean(non_rem, na.rm = TRUE), avg_sleep_awake_ratio = mean(sleep_awake_ratio, na.rm = TRUE))



# Print the data

summary_data



# Comment: The most common vore in the data set is herbivore (32), followed by omnivores (20), carnivores (19), and finally insectivores (5). 

# Omnivores and insectivores generally show higher average non_rem sleep (9.09 hours for omnivores and 13 hours for insectivores) than the other vores (7.37 hours for carnivores, 7.78 hours for herbivores). 

# For avg_sleep_awake_ratio, insectivores have the highest ratio (2.72), followed by carnivores (1.05). The two lowest avg_sleep_awake_ratio values come from herbivores (0.861) and omnivores (0.980).



# Question 4: Use an appropriate plot to compare the distributions of sleep_awake_ratio for different vore groups of mammals. Add a brief comment in the code using "#" to describe how the distribution of sleep_awake_ratio varies across different vore groups



ggplot(df2, aes(x = vore, y = sleep_awake_ratio)) + geom_boxplot() + labs(x = "Vore group", y = "Sleep / awake ratio")



# Comment: Insectivores show the greatest variability in sleep_awake_ratio amongst the vore groups, followed by herbivores (2nd most variability), and carnivores come in 3rd place for most variability.

# The vore group with least variability in sleep_awake_ratio is the omnivore group.



# Question 5: Use an appropriate plot to visualize the relationship between sleep_awake_ratio and non_rem. Include a comment in the code using "#" to describe the relationship observed in the plot.



ggplot(df2, aes(x = non_rem, y = sleep_awake_ratio)) + geom_point() + geom_smooth() + labs(x = "Non-REM sleep (hours)", y = "Sleep / awake ratio")



# Comment: There is a clear positive relationship. Mammals with higher non_rem sleep tend to have a higher sleep_awake_ratio.
>>>>>>> 53d8de23a6e082d6369939500f9742d15bb09f4e
