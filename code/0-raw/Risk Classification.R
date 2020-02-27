#classify the risk level (scale 1-8) of a given applicant for life insurance
#using decision tree model, random forest model

#load libraries
library(tidyverse)
library(caret)
library(readr)
library(rpart)
library(Metrics)
library(Rborist)

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



#export file as .csv
#sep specifies the type of file you are writing to, this saves it to the 
#current working directory
#this will overwrite the previously saved file of the same name
write.table(submission, file = "submission.csv", row.names = F, sep = ",", col.names = T)

#to save somewhere other than working directory
#don't need the C: in the path name!
write.table(est_risk, file = "/Users/John/Documents/estimated_risk.csv", 
            row.names = F, sep = ',')

#write .csv command
write.csv(est_risk, file = "/Users/John/Documents/estimated_risk.csv", row.names = F)

#write as .txt. file
write.table(est_risk, file = "estimated_risk.txt", row.names = F, sep = "\t")

#save as space delimited file - will show as just type File 
write.table(est_risk, file = "estimated_risk", row.names = F, sep = " ")
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
control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1, 5), predFixed = seq(0, 100, 25))
model_rf <- train(Response ~ ., data = train,
                  method = "Rborist", 
                  ntree = 100,
                  tuneGrid = grid,
                  nsamp = 5)
plot(model_rf)
varImp(model_rf)

