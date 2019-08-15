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
install.packages("MuMIn")
install.packages("emmeans")

library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)
library(multcomp)
library(MuMIn)
library(emmeans)

#Look at mixed effects model
#start without random factor
M0<-gls(P ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(P ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting
M2<-lme(P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M0,M2)

#M2 looks the best 

#Look at residuals

E2<-residuals(M2)

plot(filter(sm, !is.na(P)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(P)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

x<-sm$P[!is.na(sm$P)]#removes na values from column
E1<-residuals(M1,type="normalized")
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

M2<-lme(P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm)

M2.1<-lme(P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf1)

M2.2<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf2)

M2.3<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf3)

M2.4<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf4)

M2.5<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf5)

M2.6<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf6)

M2.7<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf7)

M2.8<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf8)

M2.9<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf9)

M2.10<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf10)

M2.11<-lme(P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf11)

anova(M2,M2.1,M2.2,M2.3,M2.4,M2.5,M2.6,M2.7,M2.8,M2.9,M2.10,M2.11)
#M1.10 is best with varIdent as a function of time

E2.10<-residuals(M2.10)

plot(filter(sm, !is.na(P)) %>%dplyr::select(location),
     E2.10, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(P)) %>%dplyr::select(impact),
     E2.10, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.10))
qqline(residuals(M2.10))
ad.test(residuals(M2.10))

x<-sm$P[!is.na(sm$P)]#removes na values from column
E2.10<-residuals(M2.10,type="normalized")
plot(M2.10) #residuals vs fitted values
plot(x, E2.10)

summary(M2.10)

#Auto Correlation Plot
E2.10<-residuals(M2.10)
x<-!is.na(sm$P)
Efull<-vector(length=length(sm$P))
Efull<-NA
Efull[x]<-E2.10
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#but we will group by categorical time for graphing purposes
x <- group_by(sm, impact, f.time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(P.mean = mean(P, na.rm = TRUE), # na.rm = TRUE to remove missing values
            P.sd=sd(P, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(P)), # of observations, excluding NAs. 
            P.se=P.sd/sqrt(n))
#this code defines graphing.interval as date format, but we won't use it for now
#x$graph.interval <-as.Date(as.character(x$graph.interval), format="%m/%d/%Y")

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=P.mean)) + 
  geom_errorbar(aes(ymin=P.mean-P.se, ymax=P.mean+P.se), color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab(expression(Soil~Phosphate~(mg~PO[4]~g^{-1}~soil))) +
  scale_fill_manual(name="Budworm Activity", values=c("white", "black")) +
  expand_limits(y=.25) +
  annotate("Text", x=1.505, y=.25, label="Interaction: P<0.0001", size=4) +
  annotate("Text", x=1.505, y=.23, label="Budworm Impact: P=0.0437", size=4) +
  annotate("Text", x=1.505, y=.21, label="Sampling Event: P=0.6005", size=4) +
  theme_bw() +
  theme(legend.justification=c(0.03,0.6),
        legend.position=c(0.68,0.88),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size= 12),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

#this will save the file
ggsave('srp.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")

#### Go to Chapter 6 for Violation of Indpendence

#Dealing with Temporal Correlation

M3<-gls(P ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))

M4<-gls(P ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corAR1(form=~f.time))
#Doesn't work

M2.12<-lme(P ~ impact+f.time,random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf10, correlation=corCompSymm(form=~f.time))

M2.12b<-lme(P ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))

M1.13<-lme(P ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf10, correlation=corAR1(form=~f.time))
#Doesn't Work

cs1<-corARMA(c(0.2), p=1, q=0)
cs2<-corARMA(c(0.3, -0.3), p=2, q=0)

M2.14<-lme(P ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf10, correlation=cs1)

E2.14<-residuals(M2.14)

plot(filter(sm, !is.na(P)) %>%dplyr::select(location),
     E2.14, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(P)) %>%dplyr::select(impact),
     E2.14, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.14))
qqline(residuals(M2.14))
ad.test(residuals(M2.14))

M2.15<-lme(P ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf10, correlation=cs2)

anova(M2.10,M2.12,M2.12b,M3,M2.14,M2.15)
#2.10 is best model

E3<-residuals(M3)

plot(filter(sm, !is.na(P)) %>%dplyr::select(location),
     E3, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(P)) %>%dplyr::select(impact),
     E3, xlab="Location", ylab="Residuals")

qqnorm(residuals(M3))
qqline(residuals(M3))
ad.test(residuals(M3))

#Try log normalized data

M0<-gls(log.P ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

M1<-lme(log.P ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

anova(M0,M1)

#M1 is better

M2<-lme(log.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")
AIC(M2)

E2<-residuals(M2)

plot(filter(sm, !is.na(log.P)) %>%dplyr::select(location),
     E2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.P)) %>%dplyr::select(impact),
     E2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

#log normal does not work

M2.1<-lme(log.P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf1)

M2.2<-lme(log.P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf2)

M2.3<-lme(log.P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf3)

M2.4<-lme(log.P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf4)

M2.5<-lme(log.P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf5)

M2.6<-lme(log.P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf6)

M2.7<-lme(log.P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf7)

M2.8<-lme(log.P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf8)

M2.9<-lme(log.P ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf9)

M2.10<-lme(log.P ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf10)

M2.11<-lme(log.P ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf11)

anova(M2.1,M2.2,M2.3,M2.4,M2.5,M2.6,M2.7,M2.8,M2.9,M2.10,M2.11)
#Look at 1, 4, 5, 6, 10, 11


E2.11<-residuals(M2.11)

qqnorm(residuals(M2.11))
qqline(residuals(M2.11))
ad.test(residuals(M2.11))

#try log of 5th root

M0<-gls(log.P.5th ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

M1<-lme(log.P.5th ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

anova(M0,M1)

#M1 is better

M2<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")
AIC(M2)

E2<-residuals(M2)

plot(filter(sm, !is.na(log.P.5th)) %>%dplyr::select(location),
     E2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.P.5th)) %>%dplyr::select(impact),
     E2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

M2.1<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf1)

M2.2<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf2)

M2.3<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf3)

M2.4<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf4)

M2.5<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf5)

M2.6<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf6)

M2.7<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf7)

M2.8<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf8)

M2.9<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf9)

M2.10<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf10)
#cant compute

M2.11<-lme(log.P.5th ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf11)

anova(M2.1,M2.2,M2.3,M2.4,M2.5,M2.6,M2.7,M2.8,M2.9,M2.11)
#Look at 1, 2, 4, 5, 6, 7, 8


E2.8<-residuals(M2.8)

qqnorm(residuals(M2.8))
qqline(residuals(M2.8))
ad.test(residuals(M2.8))

#Try 5th root with M1 Base model (No nesting)

M1.1<-lme(log.P.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1)

M1.2<-lme(log.P.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2)

M1.3<-lme(log.P.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3)

M1.4<-lme(log.P.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4)

M1.5<-lme(log.P.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5)

M1.6<-lme(log.P.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6)

M1.7<-lme(log.P.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7)

M1.8<-lme(log.P.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8)

M1.9<-lme(log.P.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9)

M1.10<-lme(log.P.5th ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10)

M1.11<-lme(log.P.5th ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11)

anova(M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11)
#Look at 1, 2, 4, 5, 6, 10, 11


E1.11<-residuals(M1.11)

qqnorm(residuals(M1.11))
qqline(residuals(M1.11))
ad.test(residuals(M1.11))

#Log does not work for M1 or M2
#Log of the 5th root does not work for M1 or M2

#####################################################
#Get Full Model Statistics and Make Graph
#####################################################
#final model
M.full<-lme(log.P.5th ~ impact*f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ impact | f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are building a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 8)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)

log.P.5th.emm = data.frame(cbind(xx,impact,event))
log.P.5th.emm$emmean.raw = 10^(log.P.5th.emm$emmean)-1
log.P.5th.emm$SE.raw = 10^(log.P.5th.emm$SE)-1

#this is the final table you can use for plotting
log.P.5th.emm

x = log.P.5th.emm

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "13Jul16", "13Jun16", "4Aug16", "4Aug16", "19Sep16", "19Sep16", "6Nov16", "6Nov16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(data=x, 
       aes(x=cat.time, y=emmean.raw, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean.raw, ymax=emmean.raw-SE.raw), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black","white")) +
  xlab("Sample Event") +
  ylab(expression(Soil~Phosphate~(mg~PO[4]~g^{-1}~soil))) +
  labs(fill="Budworm Activity") +
  theme_bw() +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=6),
        legend.key=element_blank(),
        legend.position=c(0.5,0.95),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))


#this will save the file
ggsave('figures/emmnetdinTFflux.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")
