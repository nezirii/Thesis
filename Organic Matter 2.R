#load data

sm<-read.table(file="soil.data.3.csv", header=T, sep=",")

#set factors
str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))

#install packages

install.packages("nlme")
install.packages("lme4")
install.packages("lmerTest")
install.packages("dplyr")
install.packages("nortest")
install.packages("ggplot2")
library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)

#Means Plot

x <- group_by(sm, impact, f.time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pct.om.mean = mean(pct.om, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pct.om.sd=sd(pct.om, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pct.om)), # of observations, excluding NAs. 
            pct.om.se=pct.om.sd/sqrt(n))

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=pct.om.mean)) + 
  geom_errorbar(aes(ymin=pct.om.mean-pct.om.se, ymax=pct.om.mean+pct.om.se), color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab("Organic Matter (%)") +
  scale_fill_manual(name="Budworm Activity", values=c("white", "black")) +
  expand_limits(y=.25) +
  theme_bw() +
  theme(legend.justification=c(0.03,0.6),
        legend.position=c(0.03,0.15),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size= 12),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

ggsave('organic matter.tiff',
       units="in",
       width=5,
       height=4.5,
       dpi=1200,
       compression="lzw")

#Look at mixed effects model
#add random factor - refer to chapter 5 of zuur

#Look at mixed effects model
#start without random factor
M0<-gls(pct.om ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor

M1<-lme(pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting
M2<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, method="ML")

anova(M0,M2)

#M2 looks the best

#Look at residuals

E2<-residuals(M2)

plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

x<-sm$pct.om[!is.na(sm$pct.om)]#removes na values from column
E2<-residuals(M1,type="normalized")
plot(M2) #residuals vs fitted values
plot(x, E2)

#try alternate variance structures
vf1=varIdent(form=~1|impact)
vf2=varIdent(form=~1|f.time)
vf3=varPower(form=~ fitted(.))
vf4=varExp(form=~ fitted(.))
vf5=varConstPower(form=~ fitted(.))
vf6=varPower(form = ~ fitted (.)|impact)
vf7=varPower(form = ~ fitted (.)|f.time)
vf8=varExp(form=~fitted(.)|impact)
vf9=varExp(form=~fitted(.)|f.time)
vf10=varConstPower(form=~ fitted(.)|impact)
vf11=varConstPower(form=~ fitted(.)|f.time)

M2<-lme(pct.om ~ impact+f.time, random=~1|nest,
        na.action=na.omit, data=sm)

M2.1<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf1)

M2.2<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf2)

M2.3<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf3)#No Convergence

M2.4<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf4)#No Convergence

M2.5<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf5)#No Convergence

M2.6<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf6)#No Convergence

M2.7<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf7)#No Convergence

M2.8<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf8)#No Convergence

M2.9<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf9)#No Convergence

M2.10<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf10)#No Convergence

M2.11<-lme(pct.om ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf11)

anova(M2,M2.1,M2.2,M2.11)
#M2.11 is best with varIdent as a function of time

E2.11<-residuals(M2.11)

plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(location),
     E2.11, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(impact),
     E2.11, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.11))
qqline(residuals(M2.11))
ad.test(residuals(M2.11))

x<-sm$pct.om[!is.na(sm$pct.om)]#removes na values from column
E2.11<-residuals(M2.11,type="normalized")
plot(M2.11) #residuals vs fitted values
plot(x, E2.11)

summary(M2.11)

#Auto Correlation Plot
E2.11<-residuals(M2.11)
x<-!is.na(sm$pct.om)
Efull<-vector(length=length(sm$pct.om))
Efull<-NA
Efull[x]<-E2.11
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#Dealing with Temporal Correlation

M3<-gls(pct.om ~ impact+f.time, 
           na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))

M4<-gls(pct.om ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corAR1(form=~f.time))
#Doesn't work

M2.12<-lme(pct.om ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf11, correlation=corCompSymm(form=~f.time))

E2.12<-residuals(M2.12)

plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(location),
     E2.12, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(impact),
     E2.12, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.12))
qqline(residuals(M2.12))
ad.test(residuals(M2.12))

M2.13<-lme(pct.om ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf11, correlation=corAR1(form=~f.time))
#Doesn't Work

cs1<-corARMA(c(0.2), p=1, q=0)
cs2<-corARMA(c(0.3, -0.3), p=2, q=0)

M2.14<-lme(pct.om ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf11, correlation=cs1)

E2.14<-residuals(M2.14)

plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(location),
     E2.14, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(impact),
     E2.14, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.14))
qqline(residuals(M2.14))
ad.test(residuals(M2.14))

M2.15<-lme(pct.om ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf11, correlation=cs2)

anova(M2.11,M2.12,M2.14,M2.15)
#M2.12 is still the best model

E2.15<-residuals(M2.15)

plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(location),
     E2.15, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.om)) %>%dplyr::select(impact),
     E2.15, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.15))
qqline(residuals(M2.15))
ad.test(residuals(M2.15))

#Log normalized data

M0<-gls(log.pct.om ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

M1<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M2<-lmer(log.pct.om ~ impact+f.time + 
           (f.plot|location), 
         na.action=na.omit, data=sm)
#M1 is the best model

E1<-residuals(M1)

plot(filter(sm, !is.na(log.pct.om)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.pct.om)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1))
qqline(residuals(M1))
ad.test(residuals(M1))

summary(M1)

#Look at Alternate Variance Structure

M1<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M1.1<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1, method="ML")

M1.2<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2, method="ML")

M1.3<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3,method="ML")

M1.4<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4, method="ML")

M1.5<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5, method="ML")

M1.6<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6, method="ML")

M1.7<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7, method="ML")

M1.8<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8, method="ML")

M1.9<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9, method="ML")

M1.10<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10, method="ML")

M1.11<-lme(log.pct.om ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11, method="ML")

anova(M1,M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11)

#M1.10 has lower AIC and a significant p-value

E1.10<-residuals(M1.10)

plot(filter(sm, !is.na(log.pct.om)) %>%dplyr::select(location),
     E1.10, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.pct.om)) %>%dplyr::select(impact),
     E1.10, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.10))
qqline(residuals(M1.10))
ad.test(residuals(M1.10))

summary(M1.10)
#makes residuals less normal
#Try M1.5

E1.5<-residuals(M1.5)

plot(filter(sm, !is.na(log.pct.om)) %>%dplyr::select(location),
     E1.5, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.pct.om)) %>%dplyr::select(impact),
     E1.5, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.5))
qqline(residuals(M1.5))
ad.test(residuals(M1.5))
#still not as good as M1
#Try M6

E1.6<-residuals(M1.6)

plot(filter(sm, !is.na(log.pct.om)) %>%dplyr::select(location),
     E1.6, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.pct.om)) %>%dplyr::select(impact),
     E1.6, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.6))
qqline(residuals(M1.6))
ad.test(residuals(M1.6))

#M1 with log transformed data is the best model