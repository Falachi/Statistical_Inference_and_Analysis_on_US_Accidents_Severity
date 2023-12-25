library(tidyverse)
df <- read_csv('US_Accidents_March23.csv')

table(df['Severity'])

set.seed(42)

library(caret)

# Assuming your data frame is named 'df'
# Assuming 'Severity' is your target variable
# Assuming you want each class to have 1000 samples

# Find the smallest class count
min_class_count <- 1000

library(ROSE)

# Assuming your data frame is named 'df'
# Assuming 'Severity' is your target variable
# Assuming you want each class to have 1000 samples

# Find the smallest class count
min_class_count <- 1000
library(ROSE)
install.packages('ROSE')
# Use ovun.sample to balance the classes

# Load the dplyr package
library(dplyr)


# Function to sample each Severity level
equalize_severity <- function(df) {
  n_samples <- min(1000, n())
  return(df %>% slice_sample(n = n_samples))
}

# Apply the function to each Severity level
df_balanced <- df2 %>%
  group_by(Severity) %>%
  group_modify(~ equalize_severity(.)) %>%
  ungroup()

# Check the new distribution
table(df_balanced$Severity)

df2 <- df %>% drop_na()
table(df2$Severity)

str(df2)
df2$Severity <- as.character(df2$Severity)

# Find the unique levels of Severity
levels <- unique(df2$Severity)

# Initialize an empty data frame for the results
df_balanced <- data.frame()

# Loop over each level
for (level in levels) {
  # Subset the data for this level
  df_level <- df2[df2$Severity == level, ]
  
  # Randomly sample 1000 rows (or fewer if there are not enough)
  df_sample <- df_level[sample(nrow(df_level), size = min(1000, nrow(df_level))), ]
  
  # Add the sampled rows to the results
  df_balanced <- rbind(df_balanced, df_sample)
}

# Print the balanced data frame
print(df_balanced)

table(df_balanced$Severity)
colSums(is.na(df_balanced))
write.csv(df_balanced, "reduced_data2.csv", row.names = FALSE)
