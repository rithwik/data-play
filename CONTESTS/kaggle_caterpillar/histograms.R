
for (i in 1:ncol(train)) {
  if (is.numeric(train[,i]) | is.integer(train[,i]))
    hist(train[,i], xlab = names(train)[i])
}

drops <- c("annual_usage", "min_order_quantity", "quantity", "cost", "id",
           "quantity_5", "quantity_6", "quantity_7", "quantity_8", "bend_radius",
           "num_boss", "num_bracket", "other")

lean2 <- train_scaled[, !(names(train_scaled) %in% drops)]

#lean <- train_scaled[, (names(train_scaled) %in% keeps)]

