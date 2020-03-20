#classify the risk level (scale 1-8) of a given applicant for life insurance
#using decision tree model, random forest model

#load libraries
library(tidyverse)
library(caret)
library(readr)
library(rpart)
library(Metrics)
library(randomForest)

#import data
train <- read_csv("C:/Users/John/Documents/Data Analysis Work/R/projects/Prudential-Risk-Classification/data/0-raw/train.csv/train.csv")
                  
#######eda
view(train)
head(train)
tail(train)
ncol(train)
nrow(train)
str(train)


#deal with NAs
any(is.na(train))
num_NA <- colSums(is.na(train)) #num of missing values per col
#remove any col which has more than 1000 missing values
remove <- ifelse(num_NA > 1000, 1, 0)
train <- train[,-which(remove == 1)]
train <- na.omit(train)
nrow(train)
train <- train[-1] #remove Id col

#possible collinearity between features not an issue for decision trees, no impact on 
#interpretability
#do not need to normalize all cols
#do not need to recode categorical variables as dummy vars for tree-based algos

####decision tree model
# model_rpart <- train(Response ~ ., data = train, 
#                      method = "rpart", 
#                      tuneGrid = data.frame(cp = seq(0, .05, leng = 25)))
# ggplot(model_rpart)

##make predictions on test set
test <- read_csv("C:/Users/John/Documents/Data Analysis Work/R/projects/Prudential-Risk-Classification/data/0-raw/test.csv/test.csv")
#test <- test[-which(remove == 1)] #not needed, does not fix issue with est_risk 
#having fewer values
#test <- na.omit(test)
#est_risk <- predict(model_rpart, newdata = test)
#submission <- data.frame(tibble(Id = test$Id, Response = est_risk))
#can't calculate rmse because I wasn't given the real test data
#each person is put in a category with a certain risk level, and all people in that
#category have the same estimated risk level


####random forest model
set.seed(1)
# categories <- unique(train$Product_Info_2) #distinct() returns a dataframe
# name <- rep(NA, 19)
#doesn't work because it won't transfer over name into mutate, the col is called "name"
# for (i in 1:19) {
#   name <- paste("col", categories[i], sep = "_")
#   train <- train %>%
#     mutate(name = ifelse(train$Product_Info_2 == categories[i], 1, 0))
# }
# x <- train[, -115]
# x2 <- x[, -2]
# y <- train$Response
# model_rf_initial <- Rborist(x2, y)
# plot(model_rf_initial)
#Rborist function can't accept characters, but Rborist method in train function can
# control <- trainControl(method = "cv", number = 5, p = 0.8)
# grid <- expand.grid(minNode = c(1, 5), predFixed = seq(0, 100, 25))
# model_rf <- train(Response ~ ., data = train,
#                   method = "Rborist", 
#                   ntree = 100,
#                   tuneGrid = grid,
#                   nsamp = 5)
# plot(model_rf)
# varImp(model_rf)
#Rborist method did not work well for me
control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1, 5), predFixed = c(10, 15, 25, 35, 50))
model_rf <- train(x = , y = y,
                  method = "rf",
                  ntree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nsamp = 5)

#na.action = na.pass gives error, =na.exclude gives error, na.omit gives error on
#unchanged test set
est_risk_rf <- predict(model_rf, newdata = test3)
est_risk_rf <- ifelse(est_risk_rf > 8, 8,
                      ifelse(est_risk_rf < 1, 1, est_risk_rf))
avg_est <- mean(est_risk_rf)
est_risk_rf <- append(est_risk_rf, avg_est, after = na_index[3])
est_risk_rf <- append(est_risk_rf, avg_est, after = na_index[2])
est_risk_rf <- append(est_risk_rf, avg_est, after = na_index[1])
est_risk_rf <- round(est_risk_rf, 0)
submission <- tibble(Id = test$Id, Response = est_risk_rf)
write.table(submission, "submission.csv", row.names = F, sep = ",")
