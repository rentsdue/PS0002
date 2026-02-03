library(ggplot2)
library(dplyr)

summary(msleep)

# 

df2 <- msleep %>% filter(!is.na(vore)) %>% select(-name, -genus)

df2 <- df2 %>% mutate(non_rem = sleep_total - sleep_rem, sleep_awake_ratio = sleep_total / awake)

summary_vore <- df2 %>% group_by(vore) %>% summarise(n_mammals = n(), mean_non_rem = mean(non_rem, na.rm = TRUE), mean_sleep_awake_ratio = mean(sleep_awake_ratio, na.rm = TRUE))

summary_vore

# Comment:
# Carnivores and insectivores generally show higher average non_rem sleep and higher
# sleep_awake_ratio than herbivores, while omnivores tend to lie between these groups.

ggplot(df2, aes(x = vore, y = sleep_awake_ratio)) + geom_boxplot() + labs(x = "Vore group", y = "Sleep / awake ratio")

# Comment:
# Carnivores and insectivores show larger variability and generally higher values of
# sleep_awake_ratio, while herbivores and omnivores exhibit smaller variability and
# lower typical values.

ggplot(df2, aes(x = non_rem, y = sleep_awake_ratio)) + geom_point() + geom_smooth() + labs(x = "Non-REM sleep (hours)", y = "Sleep / awake ratio")

# Comment:
# There is a clear positive relationship: mammals with higher non_rem sleep tend to have
# higher sleep_awake_ratio.
