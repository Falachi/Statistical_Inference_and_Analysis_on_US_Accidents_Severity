# Reducing the data
library(tidyverse)
big_data <- read_csv('US_Accidents_March23.csv')

#examine the data properties
big_data[1,]
colnames(big_data)
str(big_data)
colSums(is.na(big_data))

#examining how many objects are for each year
library(lubridate)
library(dplyr)
accidents_count <- big_data %>%
  group_by(year = year(Start_Time)) %>%
  summarize(count = n())
print(accidents_count)

# Extract the year from the Start_Time column
big_data <- big_data %>%
  mutate(year = year(Start_Time))

# Calculate the proportion of each year in the original data
year_proportions <- big_data %>%
  group_by(year) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

# Define the target sample size (4000 objects)
target_sample_size <- 4000

# Join the year proportions with the original data
reduced_data <- big_data %>%
  left_join(year_proportions, by = "year") %>%
  group_by(year) %>%
  sample_n(size = round(target_sample_size * proportion[1]), replace = TRUE) %>%
  ungroup() %>%
  filter(row_number() <= target_sample_size)

#comparing the proportions from reduced and original data
library(knitr)

# Calculate the year proportions for the original data
original_year_proportions <- big_data %>%
  group_by(year) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

# Calculate the year proportions for the reduced data
reduced_year_proportions <- reduced_data %>%
  group_by(year) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

# Create a data frame for the table
year_comparison <- data.frame(
  "Year" = original_year_proportions$year,
  "Year Proportions for Original Data" = original_year_proportions$proportion,
  "Year Proportions for Reduced Data" = reduced_year_proportions$proportion
)

# Display the table nicely
kable(year_comparison, align = "c", caption = "Year Proportions Comparison")

#examine properties of the sampled data
colnames(reduced_data)
reduced_data <- reduced_data %>% select(-year, -count,-proportion) #removing extra uneeded columns
colSums(is.na(reduced_data))

#save the reduced data as a csv file
write.csv(reduced_data, file = 'reduced_data.csv', row.names=FALSE)