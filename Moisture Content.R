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
install.packages("multcomp")
install.packages("MASS")
library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)
library(multcomp)
library(MASS)

#means plot

x <- group_by(sm, impact, f.time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(pct.moisture.mean = mean(pct.moisture, na.rm = TRUE), # na.rm = TRUE to remove missing values
            pct.moisture.sd=sd(pct.moisture, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(pct.moisture)), # of observations, excluding NAs. 
            pct.moisture.se=pct.moisture.sd/sqrt(n))


#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=pct.moisture.mean)) + 
  geom_errorbar(aes(ymin=pct.moisture.mean-pct.moisture.se, ymax=pct.moisture.mean+pct.moisture.se), 
  color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab("Moisture (%)") +
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

ggsave('moisture.tiff',
       units="in",
       width=5,
       height=4.5,
       dpi=1200,
       compression="lzw")

#Look at mixed effects model

#start without random factor
M0<-gls(pct.moisture ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor

M1<-lme(pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting
M2<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M0,M2)

#M2 looks the best

#Look at residuals

E2<-residuals(M2)

plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(location),
     E2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(impact),
     E2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

x<-sm$pct.moisture[!is.na(sm$pct.moisture)]#removes na values from column
E2<-residuals(M2,type="normalized")
plot(M2) #residuals vs fitted values
plot(x, E2)

#try alternate variance structures
vf1=varIdent(form=~1|impact)
vf2=varIdent(form=~1|time)
vf3=varPower(form=~ fitted(.))
vf4=varExp(form=~ fitted(.))
vf5=varConstPower(form=~ fitted(.))
vf6=varPower(form = ~ fitted (.)|impact)
vf7=varPower(form = ~ fitted (.)|time)
vf8=varExp(form=~fitted(.)|impact)
vf9=varExp(form=~fitted(.)|time)
vf10=varConstPower(form=~ fitted(.)|impact)
vf11=varConstPower(form=~ fitted(.)|time)

M2<-lme(pct.moisture ~ impact+f.time, random=~1|nest,
        na.action=na.omit, data=sm)

M2.1<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf1)

M2.2<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf2)

M2.3<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf3)

M2.4<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf4)

M2.5<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf5)

M2.6<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf6)

M2.7<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf7)

M2.8<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf8)

M2.9<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf9)

M2.10<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf10)

M2.11<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf11)

anova(M2,M2.1,M2.2,M2.3,M2.4,M2.5,M2.6,M2.7,M2.8,M2.9,M2.10,M2.11)
#M2.7 is best with varIdent as a function of time

E2.7<-residuals(M2.7)

plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(location),
     E2.7, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(impact),
     E2.7, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.7))
qqline(residuals(M2.7))
ad.test(residuals(M2.7))

x<-sm$pct.moisture[!is.na(sm$pct.moisture)]#removes na values from column
E2.7<-residuals(M2.7,type="normalized")
plot(M2.7) #residuals vs fitted values
plot(x, E2.7)

summary(M2.7)

#Auto Correlation Plot
E2.7<-residuals(M2.7)
x<-!is.na(sm$pct.moisture)
Efull<-vector(length=length(sm$pct.moisture))
Efull<-NA
Efull[x]<-E2.7
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#Dealing with Temporal Correlation

M3<-gls(pct.moisture ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))

M4<-gls(pct.moisture ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corAR1(form=~f.time))
#Doesn't work

M2.12<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=corCompSymm(form=~f.time))
#Doesn't work

M2.13<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=corAR1(form=~f.time))
#Doesn't Work

cs1<-corARMA(c(0.2), p=0, q=0)
cs2<-corARMA(c(0.2), p=0, q=1)
cs3<-corARMA(c(0.2), p=0, q=2)
cs4<-corARMA(c(0.2), p=1, q=0)
cs5<-corARMA(c(0.2), p=1, q=1)
cs6<-corARMA(c(0.2), p=1, q=2)
cs7<-corARMA(c(0.2), p=2, q=0)
cs8<-corARMA(c(0.2), p=2, q=1)
cs9<-corARMA(c(0.2), p=2, q=2)


M2.14<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs1)

M2.15<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs2)

M2.16<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs3)

M2.17<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs4)

M2.18<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs5)

M2.19<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs6)

M2.20<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs7)

M2.21<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs8)

M2.22<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs9)

anova(M2.7,M3,M2.14,M2.15)
#M2.14 is still the best model

#Log normalized data

M0<-gls(log.pct.moisture ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

M1<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M2<-lmer(log.pct.moisture ~ impact+f.time + 
           (f.plot|location), 
         na.action=na.omit, data=sm)

anova(M0,M1)
#M1 is the best model

E1<-residuals(M1)

plot(filter(sm, !is.na(log.pct.moisture)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.pct.moisture)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1))
qqline(residuals(M1))
ad.test(residuals(M1))

summary(M1)

#Try alternate variance structures

M1<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M1.1<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1, method="ML")

M1.2<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2, method="ML")

M1.3<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3, method="ML")

M1.4<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4, method="ML")

M1.5<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5, method="ML")

M1.6<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6, method="ML")

M1.7<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7, method="ML")

M1.8<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8, method="ML")

M1.9<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9, method="ML")

M1.10<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10, method="ML")

M1.11<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11, method="ML")

anova(M1,M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11)

E1.1<-residuals(M1.1)

plot(filter(sm, !is.na(log.pct.moisture)) %>%dplyr::select(location),
     E1.1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.pct.moisture)) %>%dplyr::select(impact),
     E1.1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.1))
qqline(residuals(M1.1))
ad.test(residuals(M1.1))

#After Trying models that had lower AIC scores with signifcant p-values, M1.1 was the best fit

#Run Post Hoc

model.matrix.lme <- function(M1.1, ...){
  model.matrix(terms(M1.1), data = getData(M1.1), ...)  
}
model.frame.lme <- function(M1.1, ...){
  model.frame(formula(M1.1), data = getData(M1.1), ...)  
}
terms.lme <- function(M1.1, ...){
  terms(model.frame(M1.1),...)  
}

multCompTukey <- glht(M1.1, linfct = mcp(time = "Tukey")) 

summary(multCompTukey)
