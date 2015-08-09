
setwd("~/Desktop/data/dd_blood/")

train <- read.csv("train.csv")
test <- read.csv("test.csv")

names(train) <- c("v1","since_last","donations","volume","since_first","target")
names(test) <- c("v1","since_last","donations","volume","since_first")

test$target <- 9999
full <- rbind(train, test)
full$tenure <- full$since_first - full$since_last
full$avg <- full$tenure/full$donations

full$since_last <- scale(full$since_last)
full$since_first <- scale(full$since_first)
full$donations <- scale(full$donations)
full$volume <- scale(full$volume)
full$tenure <- scale(full$tenure)
full$avg <- scale(full$avg)

train <- full[full$target != 9999, ]
test <- full[full$target == 9999, ]

train_old <- train

droprows <- c(398,264,387,389,9,1)
train <- train[-droprows,]

library(Metrics)

### RANDOM FOREST
library(randomForest)
m_rf <- randomForest(as.factor(target) ~
                      tenure + avg + since_last + donations +since_first,
                      data = train, importance = TRUE, sampsize=30)
p_rf <- predict(m_rf, data = train, type = "prob")
rmsle(as.numeric(train$target), p_rf[,2]) #0.2761856

### LOGISTIC REGRESSION
m_glm <- glm(target ~ tenure + avg + since_last + donations + since_first,
              data = train, family = binomial)
p_glm <- predict(m_glm, data = train, type = "response")
rmsle(as.numeric(train$target), p_glm) #0.2763152

### SVM
library(e1071)
m_svm <- svm(target ~ tenure+avg+since_last + donations + volume + since_first,
              data = train, probability = T)
p_svm <- predict(m_svm, data = train, type = "response")
rmsle(as.numeric(train$target), as.numeric(p_svm)) #0.2736668

### CART
library(rpart); library(rpart.plot)
m_cart <- rpart(target ~ tenure+avg + since_last + donations + volume + since_first,
                 data = train, method = "class")
p_cart <- predict(m_cart, data = train, type = "prob")
rmsle(as.numeric(train$target), p_cart[,2]) #0.2653818

### C5.0
library(C50)
m_c50 <- C5.0(as.factor(target) ~ tenure+avg+donations+since_last+
                since_first, data = train)
p_c50 <- predict(m_c50, train)
rmsle(as.numeric(train$target), as.numeric(p_c50)) #0.6560474

### NAIVE BAYES
m_nb <- naiveBayes(as.factor(target) ~ tenure+avg+since_last + donations + volume +
                     since_first, data = train)
p_nb <- predict(m_nb, train$target)
rmsle(as.numeric(train$target), as.numeric(p_nb)) #0.6044373

# ### XGBOOST
# library(xgboost)
# library(dplyr)
# d_xgb1 <- select(train, -target, -v1, -volume)
# d_xgb2 <- select(train, target) %>% as.matrix()
# m_xgb <- xgboost(data = as.matrix(d_xgb1),
#                  label = as.numeric(d_xgb2),
#                  objective="binary:logistic", verbose=2, nrounds=10)
# p_xgb <- predict(m_xgb, as.matrix(d_xgb1), outputmargin=T)
# rmsle(as.numeric(train$target), p_xgb) # not computed because of neg probs

# ### ADABOOST
# library(adabag)
# m_ada <- boosting(target ~ since_last + donations + volume +
#                   since_first, data = train)
# p_ada <- predict(m_ada, train)
# rmsle(as.numeric(train$target), p_ada$prob[,2]) #0.47678

### SUBMISSIONS

# cart
submission <- read.csv("sample submission.csv", check.names = F)
submission[,1] <- test$v1
submission[,2] <- predict(m_cart, newdata = test, type = "prob")[,2]
write.csv(submission, "submission_rf_size30.csv", row.names = F)

# adaboost
test2 <- test
test2$target = factor(c("0"), levels=c("0","1"))
predictions <- predict(m_ada, newdata=test2)
submission <- data.frame(test$v1, predictions$prob[,2])
write.csv(submission, "submission5.csv", row.names = F)

# naivebayes
predictions <- predict(m_nb, newdata = test, type="raw")
submission <- data.frame(test$v1, predictions[,2])
write.csv(submission, "submission_nb.csv", row.names = F)

# svm
predictions <- predict(m_svm, newdata = test, type="response")
submission <- data.frame(test$v1, predictions)
write.csv(submission, "submission_svm.csv", row.names = F)

# randomforest

submission <- read.csv("sample submission.csv", check.names = F)
submission[,1] <- test$v1
submission[,2] <- predict(m_rf, newdata = test, type = "prob")[,2]
write.csv(submission, "submission_rf_size30.csv", row.names = F)


