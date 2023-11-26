nnet_cv <- function(xx, train, nfold){
  
  size <- xx[1]
  maxit <- xx[2]
  decay <- 10^(xx[3])
  
  err <- numeric()
  k <- kfold(train, nfold)
  for (i in 1:nfold) {
    idx <- which(k==i)
    subtrain <- train[-idx,]
    subtest <- train[idx, ]
    model <- nnet(class ~ ., data = subtrain, maxit = maxit,
                  size = size, decay = decay, trace = FALSE)
    err[i] <- sum(predict(model, subtest, 
                          type = 'class') != subtest$class)/nrow(subtest)
  }
  merr <- mean(err)
  return(merr)
}
