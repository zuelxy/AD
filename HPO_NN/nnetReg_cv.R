nnetReg_cv <- function(xx, train, nfold){
  
  size <- xx[1]
  maxit <- xx[2]
  decay <- 10^(xx[3])
  
  err <- numeric()
  k <- kfold(train, nfold)
  for (i in 1:nfold) {
    idx <- which(k==i)
    subtrain <- train[-idx,]
    subtest <- train[idx, ]
    model <- nnet(value ~ ., data = subtrain, maxit = maxit,
                  size = size, decay = decay, trace = FALSE, 
                  linout = T)
    pred_subtest <- predict(model, subtest, "raw")
    err[i] <- sqrt(mean((pred_subtest-subtest$value)^2)/var(mydata$value))
  }
  merr <- mean(err)
  return(merr)
}
