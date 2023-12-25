set.seed(4)

# Feature Selections (Selecting the top 5 variables from the dataset)

df <- read.csv("encoded_data.csv")
df$Severity <- as.factor(df$Severity)

model <- clm(Severity ~ ., data = df)
summary(model)

# Extract coefficients
coefficients <- coef(model)
print(coefficients)

# Calculate variable importance and sort from highest to lowest
var_importance <- abs(coefficients)
var_importance <- sort(var_importance, decreasing = TRUE)  
print(var_importance)

# Modelling

#Spliting the dataset
split <- sample.split(data, SplitRatio = 0.8)
train <- subset(data, split == TRUE)  
test <- subset(data, split == FALSE)  

#Implement into the model
model <- multinom(Severity ~ Bump + Stop + Precipitation.in. + Traffic_Calming + Crossing, data = train)
predictions <- predict(model, newdata = test)
test$Severity <- as.factor(test$Severity)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, test$Severity)
print(conf_matrix)