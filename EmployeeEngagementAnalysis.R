# Loading the packages
library(readr)
library(dplyr)

# Importing the data
survey <- read_csv("C:\\Users\\Guillaume\\Downloads\\survey_data.csv")

# Getting an overview of the data
summary(survey)

# Examining the counts of the department variable
survey %>%
  count(department)

# Outputing the average engagement score for each department, sorted
survey %>%
  group_by(department) %>%
  summarize(avg_engagement = mean(engagement)) %>%
  arrange(avg_engagement)

# Creating the disengaged variable and assign the result to survey
survey_disengaged <- survey %>% 
  mutate(disengaged = ifelse(engagement <= 2, 1, 0)) 

# Summarizing the three variables by department
survey_summary <- survey_disengaged %>%
  group_by(department) %>%
  summarize(pct_disengaged = mean(disengaged),
            avg_salary = mean(salary),
            avg_vacation_days = mean(vacation_days_taken))

# Loading packages
library(ggplot2)
library(tidyr)

# Gathering data for plotting
survey_gathered <- survey_summary %>% 
  gather(key = "measure", value = "value",
         pct_disengaged, avg_salary, avg_vacation_days)

# Creating three bar charts
ggplot(survey_gathered, aes(x = measure, y = value, fill = department)) +
  geom_col(position = "dodge") +
  facet_wrap(~ measure, scales = "free")

# Adding the in_sales variable
survey_sales <- survey %>%
  mutate(in_sales = ifelse(department == "Sales", "Sales", "Other"))

# Testing the hypothesis using survey_sales
chisq.test(survey_sales$in_sales, survey_sales$disengaged)

# Is the result significant?
significant <- TRUE

# Testing the hypothesis using the survey_sales data
t.test(vacation_days_taken ~ in_sales, data = survey_sales)

# Is the result significant?
significant <- TRUE

## Conclusion: Based on the dataset, employees in the sales department
## report lower levels of engagements and lower average vacation days.