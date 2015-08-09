
setwd("~/Desktop/data/avAuxesia/")

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F )
submit <- read.csv("sample_submission.csv", stringsAsFactors = F)

test$shares <- -1
full <- rbind(train, test)
write.csv(full, "full.csv", row.names = F)

library(dplyr)
temp <- select(full, -id, -shares, -Day_of_publishing, -Category_article)
temp <- scale(temp) %>% as.data.frame()
temp <- cbind(id = full$id,
              temp,
              day = full$Day_of_publishing,
              category = full$Category_article,
              shares = full$shares)

train_scaled <- temp[which(temp$shares > 0), ]
test_scaled <- temp[which(temp$shares < 0), ]

### RANDOM FOREST
library(randomForest)
m_rf <- randomForest(log(train_scaled$shares + 1) ~ .,
                     data = select(train_scaled, -id))
p_rf <- exp(predict(m_rf, train_scaled))-1
rmse(train_scaled$shares, p_rf) #9830.767

### MAKING A LEANER DF
keeps = c("n_tokens_content", "num_hrefs", "num_imgs", "num_videos",
         "average_token_length", "num_keywords", "category",
         "self_reference_avg_shares", "day", "global_sentiment_polarity",
         "global_rate_positive_words", "global_rate_negative_words",
         "rate_positive_words", "rate_negative_words", "avg_positive_polarity",
         "min_positive_polarity", "max_positive_polarity",
         "avg_negative_polarity", "min_negative_polarity",
         "max_negative_polarity", "title_subjectivity",
         "title_sentiment_polarity", "abs_title_subjectivity",
         "abs_title_sentiment_polarity", "shares")
drops <- c("abs_title_sentiment_polarity", "abs_title_subjectivity",
           "title_sentiment_polarity", "title_subjectivity",
           "max_negative_polarity", "min_positive_polarity",
           "global_rate_negative_words", "LDA_04", "LDA_03",
           "LDA_02", "LDA_01", "LDA_00", "self_reference_avg_shares",
           "self_reference_max_shares", "self_reference_min_shares",
           "kw_max_avg", "kw_max_max", "kw_min_max", "kw_avg_min",
           "kw_max_min", "kw_min_min", "num_videos", "num_self_hrefs",
           "n_non_stop_unique_tokens", "n_non_stop_words",
           "n_unique_tokens")
lean <- train_scaled[, (names(train_scaled) %in% keeps)]
lean2 <- train_scaled[, !(names(train_scaled) %in% drops)]

### LEAN RF 1
m_rf2 <- randomForest(log(lean$shares + 1) ~ .,
                      data = lean, importance=T)
p_rf2 <- exp(predict(m_rf2, lean))-1
rmse(lean$shares, p_rf2) #10088.42

### LEAN RF 2
m_rf3 <- randomForest(log(lean2$shares + 1) ~ .,
                      data = lean2, importance=T)
p_rf3 <- exp(predict(m_rf3, lean2))-1
rmse(lean2$shares, p_rf3) #9931.384

### XGBOOST
library(xgboost)
d_xgb1 <- select(train_scaled, -id, -shares, -day, -category)
d_xgb2 <- select(train_scaled, shares)
m_xgb <- xgboost(data=as.matrix(d_xgb1),
                 label=as.matrix(train_scaled[, "shares"]),
                 objective="reg:linear", verbose=2, nrounds=5)
p_xgb <- predict(m_xgb, as.matrix(d_xgb1), outputmargin=T)
rmse(train_scaled$shares, p_xgb) #8791.116

############

submit$predictions <- exp(predict(m_rf, newdata=test_scaled))-1
write.csv(submit, "submit_1_rf.csv", row.names=F)

submit$predictions <- exp(predict(m_rf2, newdata=test_scaled))-1
write.csv(submit, "submit_2_rflean.csv", row.names=F)

submit$predictions <- predict(m_xgb,
                              as.matrix(select(test_scaled,
                                                -id, -shares, -day, -category)),
                              outputmargin=T)
write.csv(submit, "submit_3_xgb.csv", row.names=F)

submit$predictions <- pRF*0.6 + pSVM*0.2 + pLM*0.2
write.csv(submit, "submit_6_ens", row.names=F)

submit$predictions <- exp(predict(m_rf3, newdata=test_scaled))-1
write.csv(submit, "submit_7_rf.csv", row.names=F)

############






### PCA
pca1 <- prcomp(select(train_scaled, -id, -day, -category, -shares))
pca2 <- princomp(select(train_scaled, -id, -day, -category, -shares))
# plot(cumsum(pca1$sdev^2/sum(pca1$sdev^2)))
# sweep(abs(pca1$rotation), 2, colSums(abs(pca1$rotation)), "/")
# nComp <- 1
# dfComponents <- predict(princ, newdata=pmatrix)[,1:nComp]
# dfEvaluate <- cbind(as.data.frame(dfComponents), cluster=g_labels$V1)

### ENSEMBLE ON TRAIN
ens <- p_rf*0.6 + p_svm*0.2 + p_lm*0.2
rmse(train_scaled$shares, ens)

### PREDS TO ENSEMBLE
pRF <- exp(predict(m_rf, newdata=test_scaled))-1
pLM <- exp(predict(m_lm, test_scaled, type="response"))-1
pSVM <- exp(predict(m_svm, test_scaled))-1

### SVM
library(e1071)
m_svm <- svm(log(train_scaled$shares + 1) ~ .,
             data = select(train_scaled, -id))
p_svm <- exp(predict(m_svm, train_scaled))-1
rmse(train_scaled$shares, p_svm) #11448.1

### LINEAR REGRESSION
m_lm <- lm(log(train_scaled$shares + 1) ~ .,
           data = select(train_scaled, -id, -day, -category))
p_lm <- exp(predict(m_lm, train_scaled, type="response"))-1
library(Metrics)
rmse(train_scaled$shares, p_lm) #11659.28

### CART
library(rpart)
m_cart <- rpart(log(train_scaled$shares + 1) ~ .,
                data = select(train_scaled, -id))
p_cart <- exp(predict(m_cart, train_scaled))-1
rmse(train_scaled$shares, p_cart) #11507.06

### LEANER CART
m_cart2 <- rpart(log(lean$shares + 1) ~ ., data = lean)
p_cart2 <- exp(predict(m_cart2, lean))-1
rmse(lean$shares, p_cart2) #11537.65


