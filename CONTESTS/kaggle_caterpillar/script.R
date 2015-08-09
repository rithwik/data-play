
library(rpart)
library(Metrics)
library(randomForest)

setwd("~/Desktop/data/kg_caterpilar/")

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

### Clean NA values
for(i in 1:ncol(full)){
  if(is.numeric(full[,i])){
    full[is.na(full[,i]),i] = -1
  }else{
    full[,i] = as.character(full[,i])
    full[is.na(full[,i]),i] = "NAvalue"
    full[,i] = as.factor(full[,i])
  }
}

### Clean variables with too many categories
for(i in 1:ncol(full)){
  if(!is.numeric(full[,i])){
    freq = data.frame(table(full[,i]))
    freq = freq[order(freq$Freq, decreasing = TRUE),]
    full[,i] = as.character(match(full[,i], freq$Var1[1:30]))
    full[is.na(full[,i]),i] = "rareValue"
    full[,i] = as.factor(full[,i])
  }
}

test = full[which(full$id > 0),]
train = full[which(full$id < 0),]

### CART
m_cart = rpart(log(train$cost + 1)~.,
                   data=train[,-match(c("id", "cost", "tube_assembly_id"), names(train))])
p_cart = exp(predict(m_cart, train)) - 1
rmsle(train$cost, p_cart) # 0.4551563

### RF
m_rf = randomForest(log(train$cost + 1)~.,
                    data=train[, -match(c("id", "cost", "tube_assembly_id"), names(train))],
                    ntree = 20)
p_rf = exp(predict(m_rf, train)) - 1
rmsle(train$cost, p_rf) # 0.1099532
# 0.264869 on kaggle

submit = data.frame(id = test$id, cost = p_rf)
submit = aggregate(data.frame(cost = submit$cost), by = list(id = submit$id), mean)

write.csv(submit, "submit.csv", row.names = FALSE, quote = FALSE)



# ### Merge datasets if only 1 variable in common
# continueLoop = TRUE
# while(continueLoop){
#   continueLoop = FALSE
#   for(file in dir()){
#     df = read.csv(file, stringsAsFactors = F)
#     commonVariables = intersect(names(train), names(df))
#     if(length(commonVariables) == 1){
#       train = merge(train_all, df, by = commonVariables, all.x = TRUE)
#       continueLoop = TRUE
#       print(dim(train))
#     }
#   }
# }
