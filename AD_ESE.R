####Function to extract elements from lists composed with lists#####
extract_list <- function(l){return(l[[1]])}

#####discrepancyC2_EP_ESE#####
#---------------------------------------------------------------------------|
#args :  m     : the design before the EP                                   |
#        k     : the column of lhs elementary permutation                   |
#        p     : the square of the C2_star_discrepancy of m                 |
#out     l     : a list with new design and its C2 discrepancy value        |
#depends       : alphaDD, betaDD, gammaDD, gDD, hDD, ccDD                   |
#---------------------------------------------------------------------------|
discrepancyAD_EP_ESE <- function(m, k)
{
  n <- nrow(m)
  G <- m
  i <- trunc(runif(2,0,n))+1   # random two elements in k-th column
  i1 = i[1]
  i2 = i[2] 
  x <- G[i1,k]
  G[i1,k] <- G[i2,k]
  G[i2,k] <- x             # element exchange
  
  dC2 <- AD(G)
  l = list(dC2,G)
  return(l)  
}

#####discrepESE_LHS#####
#####L2 DISCREPANCY LHS VIA ESE OPTIMIZATION#####

#---------------------------------------------------------------------------|
#args :  design     : the design                                            |
#        T0    : the initial temperature                                    |
#        inner_it  : number of iterations for inner loop                    |
#        J     : number of new proposed LHS in inner loop                   |
#        it    : number of iterations for outer loop                        |
#        criterion: "C2", "W2" or "L2star"                               |
#output        : a list containing all the input arguments plus:            |
#       low L2_discrepancy design                                           |
#       vector of criterion values along the iterations                     |
#       vector of temperature values along the iterations                   |
#       vector of acceptation probability values along the iterations       |
#depends :  discrepancyC2_EP_ESE,                     |
#---------------------------------------------------------------------------|


UD_AD_ESE <- function(design,inner_it=100,it=2,criterion="AD")
{
  m <- design
  T0 <- 0.005*AD(m)
  J <- min(50, 0.1*nrow(m)*(nrow(m)-1))
  crit <- NULL
  temp <- NULL
  proba <- NULL
  
  d <- ncol(m)
  Temperature <- T0
  Best <- m
  dC2 <- AD(m)                   
  best <- dC2
  crit <- dC2
  
  for (q in 1:it)      # outer loop
  {
    BOLD <- Best       # BOLD = new LHS built at each iteration q
    bold <- best       # Best = new LHS built at every step         
    
    ni <- 0
    count <- 0
    na <- 0
    while(count<=inner_it)  # inner loop
    {
      count<-count+1
      
      modulo <- count%%d   # selecteed column $count mod d$
      l <- list(m)
      l <- rep(l,J)        # prepare J element exchange design
      
      g <- lapply(l, discrepancyAD_EP_ESE, k = modulo+1)
      values <- lapply(g, extract_list)   # the discrepancy of J designs
      k <- which.min(values)
      a <- values[[k]]    # the minimum discrapancy 
      
      Delta <- a-dC2
      
      if((Delta)<=(Temperature*runif(1))){  
        # higher is the temperature, higher is the probability of accepting a bad design.
        # if Delta is low, the probability is high of accepting a bad design.   
        # if Delta>Temperature, m is never accept.
        m <- g[[k]][[2]]
        dC2 <- a
        na <- na+1
        if(a<=best){
          Best <- m
          best <- a
          ni <- ni+1   #if optimization is ok, ni=ni+1
        }                       
      }
      crit <- c(crit,best)
    }
    
    v1 <- na/inner_it    # v1 <- acceptance ratio
    v2 <- ni/inner_it    # v2 <- optimization ratio
    
    temp <- c(temp,rep(Temperature,inner_it)) 
    proba <- c(proba,rep(v1,inner_it))   
    
    if (best-bold<0){
      f <- 1
      if(v1>=0.1 & v2<=v1){
        Temperature<-0.8*Temperature
      }
      else {
        if (v1>=0.1 & v2==v1){} 
        else {Temperature<-Temperature/0.8}
      }  
    }
    # if the criteria is optimized after the inner loop, then f equals 1
    else {
      f <- 0
      if (v1<=0.1){Temperature <- Temperature/0.7}
      else {Temperature <- Temperature*0.9}
    }
    # else, it is the exploratory step
  }
  List.res <- list(design,T0,inner_it,J,it,criterion,Best,crit,temp,proba)
  names(List.res) <- c("InitialDesign","TO","inner_it","J","it","criterion","design","critValues","tempValues","probaValues") 
  return(List.res)
}
