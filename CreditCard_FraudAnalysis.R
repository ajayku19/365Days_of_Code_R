library(tidyverse)
library(tidyquant)
library(pROC)
# Modeling
library(parsnip)

# Preprocessing & Sampling
library(recipes)
library(rsample)

# Modeling Error Metrics
library(yardstick)

# Plotting Decision Trees
#library(rpart.plot)

creditcard_data <- read_csv("C:/Users/ZBFZHR/Desktop/Tidytuesday/datasets/creditcard.csv")
glimpse(creditcard_data)

table(creditcard_data$Class)
summary(creditcard_data)

set.seed(seed = 1113)
split_obj <- rsample::initial_split(creditcard_data, prop = 0.80)
split_obj %>% training()
split_obj %>% testing()

train_data <- training(split_obj)
test_data  <- testing(split_obj)

Logistic_Model <- glm(Class~.,test_data, family = binomial())
summary(Logistic_Model)
plot(Logistic_Model)

Logistic_Model <- glm(Class~.,train_data, family = binomial())
summary(Logistic_Model)
plot(Logistic_Model)

library(pROC)
lr.predict <- predict(Logistic_Model,train_data, probability = TRUE)
auc.gbm = roc(train_data$Class, lr.predict, plot = TRUE, col = "blue")







