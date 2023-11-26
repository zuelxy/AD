AD <- function(d0){
  
  n <- nrow(d0)   ## number of runs
  d <- ncol(d0)
  q <- apply(as.matrix(d0), 2, function(x) length(unique(x))) 
  
  term1 <- 1/(q^2)+2
  const <- prod(term1)/(3^d)
  
  Im <- 0 
  for (i in 1:n) {
    term2 <- (3*q^2+1)/(4*q^2)-(d0[i,]-0.5)^2
    Im <- Im+prod(term2)
  }
  
  Q <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      term <- prod((1-abs(d0[i,]-d0[j,])))
      Q <- Q+term
      }
  }
  
  y <- const-2*Im/n+Q/(n^2)

  return(y)
}


LD <- function(d0){
  n <- nrow(d0)   ## number of runs
  d <- ncol(d0)
  q <- apply(as.matrix(d0), 2, function(x) length(unique(x))) 
  
  
  Q <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      term <- 1
      for (k in 1:d) {
        dist <- abs(d0[i,k]-d0[j,k])
        alpha <- min(dist, 1-dist)
        term <- term * (1-alpha)
      }
      Q <- Q+term
    }
  }
  
  id_even <- which(q%%2==0)
  if(length(id_even)==d)
    const <- (3/4)^(length(id_even))
  if(length(id_even)==0)
    const <- prod(3/4+1/(4*q^2))
  if(0<length(id_even)&length(id_even)<d)
    const <- (3/4)^(length(id_even)) * prod(3/4+1/(4*(q[-id_even])^2))
  
  return(-const+Q/(n^2))
}


DD <- function(d0, a=3/2, b=5/4){
  n <- nrow(d0)   ## number of runs
  d <- ncol(d0)
  q <- apply(as.matrix(d0), 2, function(x) length(unique(x))) 
  
  const <- prod((a+(q-1)*b)/q)
  
  Q <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      delta <- length(which(d0[i,]==d0[j,]))
      term <- a^delta*b^(d-delta)
      Q <- Q+term
    }
  }
  
  return(-const+Q/(n^2))
}

