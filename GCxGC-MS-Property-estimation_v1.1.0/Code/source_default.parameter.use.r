setwd(inputDrive)
default.alpha <- read.csv("default.alpha.csv",header=T)
default.prop_coef <- read.csv("default.prop_coef.csv",header=T)
rownames(default.prop_coef) <- default.prop_coef$X
default.prop_coef <- default.prop_coef[,-c(1)]


alpha1 <- default.alpha$alpha1
alpha2 <- default.alpha$alpha2
alpha3 <- default.alpha$alpha3
alpha1.dush <- default.alpha$alpha1.dush
alpha2.dush <- default.alpha$alpha2.dush
beta.orth <- default.alpha$beta.orth


prop_coef <- default.prop_coef

