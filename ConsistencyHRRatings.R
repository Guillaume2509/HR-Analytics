# Consistency of HR Ratings
# Loading the packages
library(dplyr)
library(readr)

# Importing the data
hr_data <- read_csv("C:\\Users\\Guillaume\\Downloads\\hr_data.csv")
performance_data <- read_csv("C:\\Users\\Guillaume\\Downloads\\performance_data.csv")

# Examining the datasets
summary(hr_data)
summary(performance_data)

# Joining the two tables
joined_data <- left_join(hr_data, performance_data, key = employee_id)

# Examining the result
summary(joined_data)

# Checking whether the average performance rating differs by gender 
joined_data %>% 
  group_by(gender) %>%
  summarise(avg_rating = mean(rating))

# Adding the high_performer column
performance <- joined_data %>%  
  mutate(high_performer = ifelse(rating >= 4, 1, 0))

# Testing whether one gender is more likely to be a high performer
chisq.test(performance$gender, performance$high_performer)   

# Doing the same test, and tidy the output
library(broom)
chisq.test(performance$gender, performance$high_performer) %>% 
  tidy()

# Is the test result significant?
significant <- TRUE

# Visualizing the distribution of high_performer by gender
performance %>%
  ggplot(aes(gender, fill = factor(high_performer))) +
  geom_bar(position = "fill")

# Visualizing the distribution of all ratings by gender
performance %>%
  ggplot(aes(gender, fill = factor(rating))) +
  geom_bar(position = "fill")

# Visualizing the distribution of job_level by gender
performance %>%
  ggplot(aes(x = gender, fill = job_level)) +
  geom_bar(position = "fill")

# Testing whether men and women have different job level distributions
chisq.test(performance$gender, performance$job_level) 

# Visualizing the distribution of high_performer by gender, faceted by job level
performance %>%
  ggplot(aes(x = gender, fill = factor(high_performer))) +
  geom_bar(position = "fill") +
  facet_wrap(~ job_level)

# Running a simple logistic regression
logistic_simple <- glm(high_performer ~ gender, family = "binomial", data = performance) 

# Viewing the result with summary()
logistic_simple %>%
  summary()

# Viewing a tidy version of the result
logistic_simple %>%
  tidy()

# Is the result significant?
significant <- TRUE

# Running a multiple logistic regression, accounting for both the gender AND the job_level
logistic_multiple <- glm(high_performer ~ gender + job_level, family = "binomial", data = performance)

# Viewing the result with summary() or tidy()
logistic_multiple %>%
  tidy()

# Is the result significant?  
significant <- TRUE