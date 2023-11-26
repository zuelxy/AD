library(dismo)
mydata <- read.csv("D:/dataset_reg/electrical_grid.csv", row.names = 1)

size <- seq(4, 60, by=4)
maxit <- seq(140, 500, by=40)
decay <- -1:-6

########### grid search
x.grid <- c(8, 20, 32, 44, 56)
y.grid <- c(220, 420)
z.grid <- c(-1, -3, -5)
design_grid <- expand.grid(x.grid, y.grid, z.grid)
colnames(design_grid) <- NULL
design_grid <- as.matrix(design_grid)
err_g <- apply(design_grid, 1, nnetReg_cv, train = mydata, nfold = 5)
min(err_g)

############ UD_AD
AD <- read.table("D:/design/AD30_3(15106).txt", header = T)
AD_design <- round(cbind(AD[,1]*15, AD[,2]*10, AD[,3]*6)+0.5)

AD_design[,1] <- size[AD_design[,1]]
AD_design[,2] <- maxit[AD_design[,2]]
AD_design[,3] <- decay[AD_design[,3]]

err_a <- apply(AD_design, 1, nnetReg_cv, train = mydata, nfold = 5)
min(err_a)

############ UD_DD
DD <- read.table("D:/design/DD30_3(15106).txt", header = T)
DD_design <- round(cbind(DD[,1]*15, DD[,2]*10, DD[,3]*6)+0.5)

DD_design[,1] <- size[DD_design[,1]]
DD_design[,2] <- maxit[DD_design[,2]]
DD_design[,3] <- decay[DD_design[,3]]

err_d <- apply(DD_design, 1, nnetReg_cv, train = mydata, nfold = 5)
min(err_d)

############ UD_LD
LD <- read.table("D:/design/LD30_3(15106).txt", header = T)
LD_design <- round(cbind(LD[,1]*15, LD[,2]*10, LD[,3]*6)+0.5)

LD_design[,1] <- size[LD_design[,1]]
LD_design[,2] <- maxit[LD_design[,2]]
LD_design[,3] <- decay[LD_design[,3]]

err_l <- apply(LD_design, 1, nnetReg_cv, train = mydata, nfold = 5)
min(err_l)

