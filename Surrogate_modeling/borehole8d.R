library(DiceKriging)
borehole <- function(xx)
{
  rw <- 0.05 + (0.15-0.05)*xx[1]
  r  <- 100 + (50000-100)*xx[2]
  Tu <- 63070 + (115600-63070)*xx[3]
  Hu <- 990 + (1110-990)*xx[4]
  Tl <- 63.1 + (116-63.1)*xx[5]
  Hl <- 700 + (820-700)*xx[6]
  L  <- 1120 + (1680-1120)*xx[7]
  Kw <- 1500 + (15000-1500)*xx[8]
  
  frac1 <- 2 * pi * Tu * (Hu-Hl)
  
  frac2a <- 2*L*Tu / (log(r/rw)*rw^2*Kw)
  frac2b <- Tu / Tl
  frac2 <- log(r/rw) * (1+frac2a+frac2b)
  
  y <- frac1 / frac2
  return(y)
}

testpoint <- as.matrix(read.table("~/Desktop/R_script/AD/design/tp_8.txt",
                                  header = T))
testy <- apply(testpoint, 1, borehole)
colnames(testpoint) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

########## LHD 
library(LHD)
set.seed(18)
lhd_result <- numeric()
for (l in 1:10) {
  lhd_8d <- as.data.frame((rLHD(32, 8)-0.5)/32)
  y_lhd <- apply(lhd_8d, 1, borehole)
  km_lhd <-  km(design = lhd_8d, response = y_lhd)
  yhat_lhd <- predict(km_lhd, testpoint, "SK")
  lhd_result[l] <- sqrt(mean((testy-yhat_lhd$mean)^2)/(mean((testy-mean(y_lhd))^2)))
}
mean(lhd_result)
# [1] 0.2113357

# maxilhd <- as.data.frame(maximinSLHD(t=1, m=32, k=8)$Design)
# maxilhd <- (maxilhd-0.5)/32
maxilhd <- read.table("~/Desktop/R_script/AD/design/maximinLHD32_8.txt", header = T)
y_maxi <- apply(maxilhd, 1, borehole)
km_maxi <- km(design = maxilhd, response = y_maxi)
yhat_maxi <- predict(km_maxi, testpoint, "SK")

sqrt(mean((testy-yhat_maxi$mean)^2)/(mean((testy-mean(y_maxi))^2)))
# [1] 0.2024408

######### 16 levels
UD_AD <- read.table("~/Desktop/R_script/AD/design/AD32_8(16).txt", header = T)
y_AD <- apply(UD_AD, 1, borehole)

UD_DD <- read.table("~/Desktop/R_script/AD/design/DD32_8(16).txt", header = T)
y_DD <- apply(UD_DD, 1, borehole)

UD_LD <- read.table("~/Desktop/R_script/AD/design/LD32_8(16).txt", header = T)
y_LD <- apply(UD_LD, 1, borehole)

km_ad <-  km(design =  UD_AD, response = y_AD)
yhat_ad <- predict(km_ad, testpoint, "SK")

km_dd <- km(design =  UD_DD, response = y_DD)
yhat_dd <- predict(km_dd, testpoint, "SK")

km_ld <- km(design =  UD_LD, response = y_LD)
yhat_ld <- predict(km_ld, testpoint, "SK")


sqrt(mean((testy-yhat_ad$mean)^2)/(mean((testy-mean(y_AD))^2)))
# [1] 0.1490579
sqrt(mean((testy-yhat_dd$mean)^2)/(mean((testy-mean(y_DD))^2)))
# [1] 0.1678882
sqrt(mean((testy-yhat_ld$mean)^2)/(mean((testy-mean(y_LD))^2)))
# [1] 0.1815225

######### 32 levels
UD_AD2 <- read.table("~/Desktop/R_script/AD/design/AD32_8(32).txt", header = T)
y_AD2 <- apply(UD_AD2, 1, borehole)

UD_DD2 <- read.table("~/Desktop/R_script/AD/design/DD32_8(32).txt", header = T)
y_DD2 <- apply(UD_DD2, 1, borehole)

UD_LD2 <- read.table("~/Desktop/R_script/AD/design/LD32_8(32).txt", header = T)
y_LD2 <- apply(UD_LD2, 1, borehole)

km_ad2 <-  km(design =  UD_AD2, response = y_AD2)
yhat_ad2 <- predict(km_ad2, testpoint, "SK")

km_dd2 <- km(design =  UD_DD2, response = y_DD2)
yhat_dd2 <- predict(km_dd2, testpoint, "SK")

km_ld2 <- km(design =  UD_LD2, response = y_LD2)
yhat_ld2 <- predict(km_ld2, testpoint, "SK")

sqrt(mean((testy-yhat_ad2$mean)^2)/(mean((testy-mean(y_AD2))^2)))
# [1] 0.1707282
sqrt(mean((testy-yhat_dd2$mean)^2)/(mean((testy-mean(y_DD2))^2)))
# [1] 0.2875196
sqrt(mean((testy-yhat_ld2$mean)^2)/(mean((testy-mean(y_LD2))^2)))
# [1] 0.2200617

