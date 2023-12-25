library(tidyverse)
df <- read.csv("preprocessed_data.csv")

library(nnet)
library(caTools)
library(VGAM)
library(MASS)
library(caret)
#encoding
df$Severity <- as.numeric(factor(df$Severity))
df$Month <- as.numeric(factor(df$Month))
df$Wind_Direction <- as.numeric(factor(df$Wind_Direction))
df$Weather_Condition <- as.numeric(factor(df$Weather_Condition))
df$Time_Of_Day <- as.numeric(factor(df$Time_Of_Day))

df <- df %>%
  mutate_all(~ as.numeric(.))

str(df)
table(df$Severity)

head(df, 5)
write_csv(df, 'encoded_data.csv')