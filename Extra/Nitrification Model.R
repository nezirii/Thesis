sm<-read.table(file="soil.data.3.csv", header=T, sep=",")

library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)
library(multcomp)
library(MuMIn)
library(emmeans)

#set factors
str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))

M.full<-lme(net.nitrification ~ impact, 
            random=~ 1 | location, na.action=na.omit, data=sm)

anova(M.full)