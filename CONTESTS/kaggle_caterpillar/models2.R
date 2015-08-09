
drops <- c("annual_usage", "min_order_quantity", "quantity", "id",
           "quantity_5", "quantity_6", "quantity_7", "quantity_8", "bend_radius",
           "num_boss", "num_bracket", "other")

train2 <- train[, !(names(train) %in% drops)]

### CART
m_cart2 = rpart(log(train2$cost + 1)~.,
               data=train2[,-match(c("cost", "tube_assembly_id"), names(train2))])
p_cart2 = exp(predict(m_cart2, train2)) - 1
rmsle(train2$cost, p_cart2) # 0.708128

### RF
library(doMC); library(foreach); library(randomForest)
registerDoMC()
m_rf2 = foreach(ntree = rep(20, 4), .combine = combine,
                multicombine = TRUE, .packages = 'randomForest') %dopar%
  randomForest(log(train2$cost + 1) ~.,
               train2[,-match(c("id", "cost", "tube_assembly_id"), names(train))],
               ntree = ntree)

# m_rf2 = randomForest(log(train2$cost + 1)~.,
#                     data=train2[, -match(c("cost", "tube_assembly_id"), names(train2))],
#                     ntree = 20)

p_rf2 = exp(predict(m_rf2, train2)) - 1
Metrics::rmsle(train2$cost, p_rf2) # 0.5537819

### GBM
library(gbm)
m_gbm = gbm(log(train2$cost + 1)~.,
            data=train2[,-match(c("cost", "tube_assembly_id"), names(train2))],
            n.cores = 2,
            n.trees = 500)
p_gbm = exp(predict(m_gbm, train2, n.trees = 500)) - 1
Metrics::rmsle(train2$cost, p_gbm) # 0.7807313
