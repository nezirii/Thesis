#load data

st<-read.table(file="soil.temp.csv", header=T, sep=",")

library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)
library(multcomp)
library(MuMIn)
library(emmeans)

plot(x=st$air.temp, y=st$o.temp.2cm, xlab="Air Temperature (°C)", ylab="2cm Depth Temperature (°C)")

cor(st$air.temp, st$o.temp.2cm)

M0<-lm(o.temp.2cm ~ air.temp, data=st)

abline(M0)

M0

summary(M0)

modelSummary <- summary(M0)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["air.temp", "Estimate"]  # get beta estimate for o.temp.2cm
std.error <- modelCoeffs["air.temp", "Std. Error"]  # get std.error for o.temp.2cm
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
