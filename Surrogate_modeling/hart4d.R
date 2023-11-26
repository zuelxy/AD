library(DiceKriging)

hart4 <- function(xx)
{
  alpha <- c(1.0, 1.2, 3.0, 3.2)
  A <- c(10, 3, 17, 3.5, 1.7, 8,
         0.05, 10, 17, 0.1, 8, 14,
         3, 3.5, 1.7, 10, 17, 8,
         17, 8, 0.05, 10, 0.1, 14)
  A <- matrix(A, 4, 6, byrow=TRUE)
  P <- 10^(-4) * c(1312, 1696, 5569, 124, 8283, 5886,
                   2329, 4135, 8307, 3736, 1004, 9991,
                   2348, 1451, 3522, 2883, 3047, 6650,
                   4047, 8828, 8732, 5743, 1091, 381)
  P <- matrix(P, 4, 6, byrow=TRUE)
  
  xxmat <- matrix(rep(xx,times=4), 4, 4, byrow=TRUE)
  inner <- rowSums(A[,1:4]*(xxmat-P[,1:4])^2)
  outer <- sum(alpha * exp(-inner))
  
  y <- (1.1 - outer) / 0.839
  return(y)
}

testpoint <- as.matrix(read.table("~/Desktop/R_script/AD/design/tp_4.txt",
                                  header = T))
testy <- apply(testpoint, 1, hart4)
colnames(testpoint) <- c("V1", "V2", "V3", "V4")

########## LHD 
library(LHD)
set.seed(71)
lhd_result <- numeric()
for (l in 1:10) {
  lhd_2d <- as.data.frame((rLHD(24,4)-0.5)/24)
  y_lhd <- apply(lhd_2d, 1, hart4)
  km_lhd <-  km(design = lhd_2d, response = y_lhd)
  yhat_lhd <- predict(km_lhd, testpoint, "SK")
  lhd_result[l] <- sqrt(mean((testy-yhat_lhd$mean)^2)/mean((testy-mean(y_lhd))^2))
}
mean(lhd_result)
# [1] 0.5790433

library(SLHD)
# maxilhd <- as.data.frame(maximinSLHD(t=1, m=24, k=4)$Design)
# maxilhd <- (maxilhd-0.5)/24
maxilhd <- read.table("~/Desktop/R_script/AD/design/maximinLHD24_4.txt", header = T)
y_maxi <- apply(maxilhd, 1, hart4)
km_maxi <- km(design = maxilhd, response = y_maxi)
yhat_maxi <- predict(km_maxi, testpoint, "SK")

mean((testy-yhat_maxi$mean)^2)
# [1] 0.2553758
sqrt(0.2553758/mean((testy-mean(y_maxi))^2))
# [1] 0.5013848

########## 12 levels
UD_AD <- read.table("~/Desktop/R_script/AD/design/AD24_4(12).txt", header = T)
y_AD <- apply(UD_AD, 1, hart4)

UD_DD <- read.table("~/Desktop/R_script/AD/design/DD24_4(12).txt", header = T)
y_DD <- apply(UD_DD, 1, hart4)

UD_LD <- read.table("~/Desktop/R_script/AD/design/LD24_4(12).txt", header = T)
y_LD <- apply(UD_LD, 1, hart4)

km_ad <-  km(design =  UD_AD, response = y_AD)
yhat_ad <- predict(km_ad, testpoint, "SK")

km_dd <- km(design =  UD_DD, response = y_DD)
yhat_dd <- predict(km_dd, testpoint, "SK")

km_ld <- km(design =  UD_LD, response = y_LD)
yhat_ld <- predict(km_ld, testpoint, "SK")

mean((testy-yhat_ad$mean)^2)
# [1] 0.2286188
sqrt(0.2286188/mean((testy-mean(y_AD))^2))
# [1] 0.47446

mean((testy-yhat_dd$mean)^2)
# [1] 0.7917358
sqrt(0.7917358/mean((testy-mean(y_DD))^2))
# [1] 0.8699686

mean((testy-yhat_ld$mean)^2)
# [1] 0.2480734
sqrt(0.2480734/mean((testy-mean(y_LD))^2))
# [1] 0.4938444


######### 24 levels
UD_AD2 <- read.table("~/Desktop/R_script/AD/design/AD24_4(24).txt", header = T)
y_AD2 <- apply(UD_AD2, 1, hart4)

UD_DD2 <- read.table("~/Desktop/R_script/AD/design/DD24_4(24).txt", header = T)
y_DD2 <- apply(UD_DD2, 1, hart4)

UD_LD2 <- read.table("~/Desktop/R_script/AD/design/LD24_4(24).txt", header = T)
y_LD2 <- apply(UD_LD2, 1, hart4)

km_ad2 <-  km(design =  UD_AD2, response = y_AD2)
yhat_ad2 <- predict(km_ad2, testpoint, "SK")

km_dd2 <- km(design =  UD_DD2, response = y_DD2)
yhat_dd2 <- predict(km_dd2, testpoint, "SK")

km_ld2 <- km(design =  UD_LD2, response = y_LD2)
yhat_ld2 <- predict(km_ld2, testpoint, "SK")

mean((testy-yhat_ad2$mean)^2)
# [1] 0.1355462
sqrt(0.1355462/mean((testy-mean(y_AD2))^2))
# [1] 0.3651401

mean((testy-yhat_dd2$mean)^2)
# [1] 0.6040542
sqrt(0.6040542/mean((testy-mean(y_DD2))^2))
# [1] 0.7637567

mean((testy-yhat_ld2$mean)^2)
# [1] 0.2611325
sqrt(0.2611325/mean((testy-mean(y_LD2))^2))
# [1] 0.4991699



