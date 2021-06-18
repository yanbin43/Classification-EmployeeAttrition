##=================================================================================================##
##                                HR Attrition Dataset (From DS360)                                ##
##=================================================================================================##

hr <- read.csv("HR_dataset.csv", stringsAsFactors = T) %>% 
  mutate(status = as.factor(ifelse(left == 1, "Left", "Stayed")))


##-------------------------------------------------------------------------------------------------##
##                                    Exploratory Data Analysis                                    ##
##-------------------------------------------------------------------------------------------------##

dim(hr)
head(hr)
str(hr)
summary(hr)
table(is.na(hr))
which(is.na(hr))

data.frame(Numeric = sapply(hr, is.numeric))
data.frame(Role = unique(hr$role))
data.frame(Salary = unique(hr$salary))

library(reshape2)
library(ggplot2)
library(dplyr)


##-------------------------------------------------------------------------------------------------##
## Correlation Heatmap for Numeric Columns

numhr <- hr[, sapply(hr, is.numeric)]
corr <- cor(numhr)
corr[upper.tri(corr)] <- NA

meltcorr <- melt(corr) %>% 
  mutate(Var1 = factor(Var1, levels = rev(levels(.$Var1))), Var2 = factor(Var2))

ggplot(meltcorr, aes(Var1, Var2, fill = value, label = round(value,2))) +
  ggtitle("Correlation Heatmap") + 
  geom_tile() +
  geom_text(size = 3) +
  coord_equal() +
  theme(plot.title = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(angle = -90, hjust = 0)) +
  scale_x_discrete(name = "") +
  scale_y_discrete(name = "") +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "navy",
                       midpoint = 0, na.value = "white", 
                       name = "Correlation", limits = c(-1, 1))


##-------------------------------------------------------------------------------------------------##
## Freq Plot (all discrete features)

# aes = as.factor: number_project, exp_in_company, work_accident, promotion_last_5years
ggplot(hr, aes(as.factor(number_project))) + geom_bar()

# fill = status, salary, role
ggplot(hr, aes(as.factor(number_project), fill = status)) + 
  geom_bar(position = "stack") +
  scale_fill_hue()


##-------------------------------------------------------------------------------------------------##
## Freq Plot (all categorical features)

# role, salary, status
ggplot(hr, aes(role)) + geom_bar()

# fill = status
ggplot(hr, aes(role, fill = status)) + 
  geom_bar(position = "stack") +
  scale_fill_hue()


##-------------------------------------------------------------------------------------------------##
## Distribution Plot (continuous features)

# satisfaction_level, last_evaluation, average_monthly_hours
ggplot(hr, aes(satisfaction_level)) + geom_bar()

# # fill = status, salary
ggplot(hr, aes(last_evaluation, fill = salary)) + 
  geom_bar(position = "stack") +
  scale_fill_hue()


##-------------------------------------------------------------------------------------------------##
## Boxplot & Violin Plot

# x / fill = role, salary, number_project, exp_in_company, work_accident, promotion_last_5years
# y = satisfaction_level, last_evaluation, average_monthly_hours

ggplot(hr, aes(x = salary, y = satisfaction_level, colour = status)) + 
  geom_boxplot() + scale_fill_hue()

ggplot(hr, aes(x = salary, y = satisfaction_level, fill = status)) + 
  geom_violin() + scale_fill_hue()

ggplot(hr, aes(x = salary, y = satisfaction_level, fill = status)) + 
  geom_violin(position = position_dodge(width = 0.4)) + 
  geom_boxplot(position = position_dodge(width = 0.4), width = 0.1) +
  scale_fill_hue()


##-------------------------------------------------------------------------------------------------##
##                                       Predictive Analysis                                       ##
##-------------------------------------------------------------------------------------------------##

library(caret)

set.seed(111)
split_numdata <- createDataPartition(numdata$left, times = 1, p = 0.75, list = FALSE)


##-------------------------------------------------------------------------------------------------##
## Logistic Regression

train <- numhr[ split_numdata,]
test <- numhr[-split_numdata,]

model <- glm (left ~ ., data = train, family = binomial)
summary(model)

predict <- predict(model, newdata = test, type = 'response')
predict[predict >= 0.5] = 1
predict[predict < 0.5] = 0

test$left <- as.factor(test$left)
predict <- as.factor(predict)
confusionMatrix(test$left, predict)





