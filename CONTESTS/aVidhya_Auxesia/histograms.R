
for (i in 1:ncol(train)) {
  if (is.numeric(train[,i]) | is.integer(train[,i]))
    hist(train[,i], xlab = names(train)[i])
}
