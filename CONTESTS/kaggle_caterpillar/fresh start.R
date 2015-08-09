
library(Metrics)
library(dplyr)
library(gbm)
library(caret)
library(randomForest)
library(foreach); library(doMC)

setwd("~/Desktop/data/contests/caterpillar/")

options(scipen = 10)

test = read.csv("test_set.csv", stringsAsFactors = F)
train = read.csv("train_set.csv", stringsAsFactors = F)

train$id = -(1:nrow(train))
test$cost = 0

full = rbind(train, test)

bom <- read.csv("bill_of_materials.csv")
tubes <- read.csv("tube.csv")
specs <- read.csv("specs.csv")

full <- merge(full, bom, by = "tube_assembly_id", all.x = T)
full <- merge(full, tubes, by = "tube_assembly_id", all.x = T)
full <- merge(full, specs, by = "tube_assembly_id", all.x = T)

full$quote_date  <- strptime(full$quote_date,format = "%Y-%m-%d", tz="GMT")
full$year <- year(as.IDate(full$quote_date))
full$month <- month(as.IDate(full$quote_date))
full$week <- week(as.IDate(full$quote_date))
full$quote_date  <- NULL
full$tube_assembly_id  <- NULL

### Clean NA values
for(i in 1:ncol(full)){
  if(is.numeric(full[,i])){
    full[is.na(full[,i]),i] = 0
  }else{
    full[,i] = as.character(full[,i])
    full[is.na(full[,i]),i] = " "
    full[,i] = as.factor(full[,i])
  }
}
rm(i)

# ### Clean variables with too many categories
# for(i in 1:ncol(full)){
#   if(!is.numeric(full[,i])){
#     freq = data.frame(table(full[,i]))
#     freq = freq[order(freq$Freq, decreasing = TRUE),]
#     full[,i] = as.character(match(full[,i], freq$Var1[1:30]))
#     full[is.na(full[,i]),i] = "rareValue"
#     full[,i] = as.factor(full[,i])
#   }
# }

### UNTUNED GBM

test = full[which(full$id > 0),]
train = full[which(full$id < 0),]

m_gbm1 = gbm(log(train$cost + 1)~.,
            data = train,
            n.cores = 2,
            n.trees = 2000)
p_gbm1 = exp(predict(m_gbm, train, n.trees = 2000)) - 1
rmsle(train$cost, p_gbm1) # 0.5862168

### TUNED GBM

tr_control <- trainControl(method = "cv", number = 5)
tr_grid <- expand.grid(n.trees = seq(1:10)*50,
                       interaction.depth = c(5,15,30),
                       shrinkage = 0.1,
                       n.minobsinnode = 10)
m_gbm = train(log(train$cost + 1)~.,
              data = train,
              method="gbm",
              tuneGrid = tr_grid,
              trControl = tr_control, n.cores = 2)
# Stopped with errors
p_gbm = exp(predict(m_gbm, train, n.trees = 2000)) - 1
rmsle(train$cost, p_gbm) # 0.5862168

### RANDOM FOREST
registerDoMC()
m_rf = foreach(ntree = rep(20, 4), .combine = combine,
               multicombine = T, .packages = 'randomForest') %dopar%
               randomForest(log(train2$cost + 1) ~.,
                            train, ntree = ntree)



### SUBMISSIONS

submit = data.frame(id = test$id, cost = p_rf)
submit = aggregate(data.frame(cost = submit$cost), by = list(id = submit$id), mean)
write.csv(submit, "submit.csv", row.names = FALSE, quote = FALSE)
