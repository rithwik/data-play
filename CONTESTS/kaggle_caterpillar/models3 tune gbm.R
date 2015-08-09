
library(caret)
library(gbm)

tr_control <- trainControl(method = "cv", number = 5)
tr_grid <- expand.grid(n.trees = seq(1:20)*50,
                       interaction.depth = c(5,15,30),
                       shrinkage = 0.1,
                       n.minobsinnode = 10)

m_gbm <- train(log(train2$cost + 1)~.,
          data=train2[, -match(c("id", "cost", "tube_assembly_id"), names(train))],
          method="gbm",
          tuneGrid = tr_grid,
          trControl = tr_control)
