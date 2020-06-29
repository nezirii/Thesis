#load data

cr<-read.table(file="conifer.reg.csv", header=T, sep=",")

library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)
library(multcomp)
library(MuMIn)
library(emmeans)

plot(x=cr$total.water.l, y=cr$total.mg.din, xlab="L H2O", ylab="mg DIN")

cor(cr$total.water.l, cr$total.mg.din)

M0<-lm(total.mg.din ~ total.water.l, data=cr)

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

tiff(filename = 'figures/Temp Reg.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")
