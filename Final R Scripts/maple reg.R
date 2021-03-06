#load data

dr<-read.table(file="maple.reg.csv", header=T, sep=",")

library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)
library(multcomp)
library(MuMIn)
library(emmeans)

plot(x=dr$total.water.l, y=dr$rate.of.decomposition, xlab="L H2O", ylab="Rate of Deciduous Decomposition")

cor(dr$total.water.l, dr$rate.of.decomposition)

M0<-lm(rate.of.decomposition ~ total.water.l, data=dr)

abline(M0)

M0

summary(M0)

modelSummary <- summary(M0)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["total.water.l", "Estimate"]  # get beta estimate for o.temp.2cm
std.error <- modelCoeffs["total.water.l", "Std. Error"]  # get std.error for o.temp.2cm
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- M0$fstatistic[1]  # fstatistic
f <- summary(M0)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

AIC(M0)
BIC(M0)

tiff(filename = 'figures/Maple Water Reg.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")


##############################DIN Reg######################################

plot(x=dr$total.mg.din, y=dr$rate.of.decomposition, xlab="Throughfall Deposition of DIN (mg)", ylab="Rate of Deciduous Decomposition (k)")

cor(dr$total.mg.din, dr$rate.of.decomposition)

M1<-lm(rate.of.decomposition ~ total.mg.din, data=dr)

abline(M1)

M1

summary(M1)

modelSummary <- summary(M1)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["total.mg.din", "Estimate"]  # get beta estimate for o.temp.2cm
std.error <- modelCoeffs["total.mg.din", "Std. Error"]  # get std.error for o.temp.2cm
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- M1$fstatistic[1]  # fstatistic
f <- summary(M1)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

AIC(M1)
BIC(M1)

tiff(filename = 'figures/Maple DIN Reg.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")
