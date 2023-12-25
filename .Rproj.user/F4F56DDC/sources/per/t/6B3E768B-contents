library(tidyverse)
df <- read.csv("preprocessed_data.csv")
library(ggplot2)
library(corrplot)

str(df)
df$Severity <- as.factor(df$Severity)
df$Month <- as.factor(df$Month)

# Install and load necessary packages
if (!requireNamespace("GGally", quietly = TRUE)) {
  install.packages("GGally")
}
library(GGally)

# Subset data with numerical variables
numerical_data <- df[, c("Temperature.F.", "Wind_Chill.F.", "Humidity...", "Pressure.in.", "Wind_Speed.mph.", "Precipitation.in.")]

# Create pair plot with hue based on Severity
ggpairs(df, columns = c("Temperature.F.", "Wind_Chill.F.", "Humidity...", "Pressure.in.", "Wind_Speed.mph.", "Precipitation.in."),
        mapping = aes(color = Severity))

ggpairs(numerical_data)

# Subset data with numerical variables
numerical_data <- df[, c("Temperature.F.", "Wind_Chill.F.", "Humidity...", "Pressure.in.", "Visibility.mi.", "Wind_Speed.mph.", "Precipitation.in.")]

# Calculate the correlation matrix
cor_matrix <- cor(numerical_data)

# Create a heatmap of the correlation matrix
corrplot(cor_matrix, method = "color")

# Subset data with numerical variables
numerical_data <- df[, c("Temperature.F.", "Wind_Chill.F.", "Humidity...", "Pressure.in.", "Visibility.mi.", "Wind_Speed.mph.", "Precipitation.in.")]

# Calculate the correlation matrix
cor_matrix <- cor(numerical_data)

# Create a heatmap of the correlation matrix with numeric values
corrplot(cor_matrix, method = "color", tl.col = "black", tl.srt = 45, addCoef.col = "black")

# Set the theme for better aesthetics
theme_set(theme_minimal())

# Create a bar plot
ggplot(df, aes(x = Time_Of_Day, fill = Severity)) +
  geom_bar(position = "dodge", color = "white") +
  
  # Set labels and title
  labs(x = "Time Category", y = "Count", title = "Severity Count by Time Category") +
  
  # Customize colors (you can choose your own palette)
  scale_fill_manual(values = c("1" = "#440154FF", "2" = "#21908CFF", "3" = "#FDE725FF", "4" = "#FFFFFF")) +
  
  # Show legend
  guides(fill = guide_legend(title = "Severity")) +
  
  # Adjust the plot size
  theme(legend.position="top", legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Display the plot
  ggsave("severity_count_by_time_category.png", width = 10, height = 6, units = "in")

# Create a bar plot
ggplot(df, aes(x = factor(Month), fill = Severity)) +
  geom_bar(position = "dodge", color = "white") +
  
  # Set labels and title
  labs(x = "Month", y = "Count", title = "Severity Count by Month") +
  
  # Customize colors (you can choose your own palette)
  # scale_fill_manual(values = c("1" = "#440154FF", "2" = "#21908CFF", "3" = "#FDE725FF", "4" = "#FFFFFF")) +
  
  # Show legend
  guides(fill = guide_legend(title = "Severity")) +
  
  # Adjust the plot size
  theme(legend.position="top", legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # Modify x-axis labels
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Create a bar plot with normalized counts
ggplot(df, aes(x = factor(Month), fill = Severity)) +
  geom_bar(position = "fill", color = "white") +
  labs(x = "Month", y = "Proportion", title = "Severity Proportion by Month") +
  guides(fill = guide_legend(title = "Severity")) +
  theme(legend.position = "top", legend.box.background = element_rect(color = "black"),
        plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))




