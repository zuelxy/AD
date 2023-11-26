library(DiceKriging)
franke2d <- function(xx)
{
  x1 <- xx[1]
  x2 <- xx[2]
  
  term1 <- 0.75 * exp(-(9*x1-2)^2/4 - (9*x2-2)^2/4)
  term2 <- 0.75 * exp(-(9*x1+1)^2/49 - (9*x2+1)/10)
  term3 <- 0.5 * exp(-(9*x1-7)^2/4 - (9*x2-3)^2/4)
  term4 <- -0.2 * exp(-(9*x1-4)^2 - (9*x2-7)^2)
  
  y <- term1 + term2 + term3 + term4
  return(y)
}

testpoint <- expand.grid(seq(0,1,length.out=30),seq(0,1,length.out=30))
testy <- apply(testpoint, 1, franke2d)
colnames(testpoint) <- c("V1", "V2")

########## LHD 
library(LHD)
set.seed(61)
lhd_result <- numeric()
for (l in 1:10) {
  lhd_2d <- as.data.frame((rLHD(20, 2)-0.5)/20)
  y_lhd <- apply(lhd_2d, 1, franke2d)
  km_lhd <-  km(design = lhd_2d, response = y_lhd)
  yhat_lhd <- predict(km_lhd, testpoint, "SK")
  lhd_result[l] <- sqrt(mean((testy-yhat_lhd$mean)^2)/mean((testy-mean(y_lhd))^2))
}
mean(lhd_result)
# [1] 0.2530281

library(SLHD)
# maxilhd <- as.data.frame(maximinSLHD(t=1, m=20, k=2)$Design)
# maxilhd <- (maxilhd-0.5)/20
maxilhd <- read.table("~/Desktop/R_script/AD/design/maximinLHD20_2.txt", header = T)
y_maxi <- apply(maxilhd, 1, franke2d)
km_maxi <- km(design = maxilhd, response = y_maxi)
yhat_maxi <- predict(km_maxi, testpoint, "SK")

mean((testy-yhat_maxi$mean)^2)
# [1] 0.00303345
sqrt(0.00303345/mean((testy-mean(y_maxi))^2))
# [1] 0.1925079

########## 10 levels
UD_AD <- read.table("~/Desktop/R_script/AD/design/AD20_2(10).txt", header = T)
y_AD <- apply(UD_AD, 1, franke2d)

UD_DD <- read.table("~/Desktop/R_script/AD/design/DD20_2(10).txt", header = T)
y_DD <- apply(UD_DD, 1, franke2d)

UD_LD <- read.table("~/Desktop/R_script/AD/design/LD20_2(10).txt", header = T)
y_LD <- apply(UD_LD, 1, franke2d)

km_ad <-  km(design =  UD_AD, response = y_AD)
yhat_ad <- predict(km_ad, testpoint, "SK")

km_dd <- km(design =  UD_DD, response = y_DD)
yhat_dd <- predict(km_dd, testpoint, "SK")

km_ld <- km(design =  UD_LD, response = y_LD)
yhat_ld <- predict(km_ld, testpoint, "SK")

mean((testy-yhat_ad$mean)^2)
# [1] 0.001760816
sqrt(0.001760816/mean((testy-mean(y_AD))^2))
# [1] 0.146615

mean((testy-yhat_dd$mean)^2)
# [1] 0.004016731
sqrt(0.004016731/mean((testy-mean(y_DD))^2))
# [1] 0.2202945

mean((testy-yhat_ld$mean)^2)
# [1] 0.003752159
sqrt(0.003752159/mean((testy-mean(y_LD))^2))
# [1] 0.2135834


########## 20 levels (get)
UD_AD2 <- read.table("~/Desktop/R_script/AD/design/AD20_2(20).txt", header = T)
y_AD2 <- apply(UD_AD2, 1, franke2d)

UD_DD2 <- read.table("~/Desktop/R_script/AD/design/DD20_2(20).txt", header = T)
y_DD2 <- apply(UD_DD2, 1, franke2d)

UD_LD2 <- read.table("~/Desktop/R_script/AD/design/LD20_2(20).txt", header = T)
y_LD2 <- apply(UD_LD2, 1, franke2d)

km_ad2 <- km(design =  UD_AD2, response = y_AD2)
yhat_ad2 <- predict(km_ad2, testpoint, "SK")

km_dd2 <- km(design =  UD_DD2, response = y_DD2)
yhat_dd2 <- predict(km_dd2, testpoint, "SK")

km_ld2 <- km(design =  UD_LD2, response = y_LD2)
yhat_ld2 <- predict(km_ld2, testpoint,"SK")

mean((testy-yhat_ad2$mean)^2)
# [1] 0.001298368
sqrt(0.001298368/mean((testy-mean(y_AD2))^2))
# [1] 0.1259046

mean((testy-yhat_dd2$mean)^2)
# [1] 0.008192074
sqrt(0.008192074/mean((testy-mean(y_DD2))^2))
# [1] 0.3163187

mean((testy-yhat_ld2$mean)^2)
# [1] 0.005028441
sqrt(0.005028441/mean((testy-mean(y_LD2))^2))
# [1] 0.2460336
