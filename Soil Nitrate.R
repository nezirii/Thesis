#load data

sm<-read.table(file="soil.data.3.csv", header=T, sep=",")

sm$ng.NO3<-1000*(sm$NO3)

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
M0<-gls(NO3 ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(NO3 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting

M2<-lme(NO3 ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

#try interaction with random factor

M3<-lme(NO3 ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M4<-lme(NO3 ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2,M3,M4)

#M1

#Look at residuals

E1<-residuals(M1)

plot(filter(sm, !is.na(NO3)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(NO3)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1))
qqline(residuals(M1))
ad.test(residuals(M1))

x<-sm$NO3[!is.na(sm$NO3)]#removes na values from column
E1<-residuals(M1,type="normalized")
plot(M1) #residuals vs fitted values
plot(x, E1)

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

M1<-lme(NO3 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M1.1<-lme(NO3 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1)

M1.2<-lme(NO3 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2)

M1.3<-lme(NO3 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3)
#no convergence

M1.4<-lme(NO3 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4)
#no convergence

M1.5<-lme(NO3 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5)


M1.6<-lme(NO3 ~ impact+time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6)

M1.7<-lme(NO3 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7)

M1.8<-lme(NO3 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8)

M1.9<-lme(NO3 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9)
#no convergence

M1.10<-lme(NO3 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10)

M1.11<-lme(NO3 ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11)


anova(M1.1,M1.2, M1.5, M1.6, M1.7, M1.8, M1.10, M1.11)
#M1.11 is best

E1.11<-residuals(M1.11)

plot(filter(sm, !is.na(NO3)) %>%dplyr::select(location),
     E1.11, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(NO3)) %>%dplyr::select(impact),
     E1.11, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.11))
qqline(residuals(M1.11))
ad.test(residuals(M1.11))

x<-sm$NO3[!is.na(sm$NO3)]#removes na values from column
E1.11<-residuals(M1.11,type="normalized")
plot(M1.11) #residuals vs fitted values
plot(x, E1.11)

#Auto Correlation Plot
E1.11<-residuals(M1.11)
x<-!is.na(sm$NO3)
Efull<-vector(length=length(sm$NO3))
Efull<-NA
Efull[x]<-E1.11
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#one option is to group by graph.interval instead of time
#but we will group by categorical time for graphing purposes
x <- group_by(sm, impact, f.time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(NO3.mean = mean(NO3, na.rm = TRUE), # na.rm = TRUE to remove missing values
            NO3.sd=sd(NO3, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(NO3)), # of observations, excluding NAs. 
            NO3.se=NO3.sd/sqrt(n))
#this code defines graphing.interval as date format, but we won't use it for now
#x$graph.interval <-as.Date(as.character(x$graph.interval), format="%m/%d/%Y")

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=NO3.mean)) + 
  geom_errorbar(aes(ymin=NO3.mean-NO3.se, ymax=NO3.mean+NO3.se), color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab(expression(Soil~Nitrate~(mg~NO[3]~g^{-1}~soil))) +
  scale_fill_manual(name="Budworm Activity", values=c("white", "black")) +
  expand_limits(y=.03) +
  annotate("Text", x=6.5, y=.03, label="Interaction: P<0.0001", size=4) +
  annotate("Text", x=6.5, y=.029, label="Budworm Impact: P=0.9822", size=4) +
  annotate("Text", x=6.5, y=.028, label="Sampling Event: P<0.0001", size=4) +
  theme_bw() +
  theme(legend.justification=c(0.03,0.6),
        legend.position=c(0.03,0.88),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size= 12),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


#this will save the file
ggsave('no3.tiff',
       units="in",
       width=5,
       height=4.5,
       dpi=1200,
       compression="lzw")


######################### Violation of Indpendence ###################################

#Dealing with Temporal Correlation

M1<-lme(NO3 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M1.11<-lme(NO3 ~ impact+f.time,
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11)

M1.12<-lme(NO3 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11, correlation=corCompSymm(form=~f.time))
#Does not work

M1.13<-lme(NO3 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))


cs1<-corARMA(c(0.2), p=1, q=0)
cs2<-corARMA(c(0.3, -0.3), p=2, q=0)

M1.14<-lme(NO3 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11, correlation=cs1)

M1.15<-lme(NO3 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11, correlation=cs2)
#Does not work


anova(M1.11, M1.13, M1.14)

#M1.11 Still best

######################################Try log normalized data###############################################

sm$log.ng.NO3<-log10(sm$ng.NO3)

M1<-lme(log.NO3 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting

M2<-lme(log.NO3 ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

#try interaction with random factor

M3<-lme(log.NO3 ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M4<-lme(log.NO3 ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2,M3,M4)

#M3 is better

M3<-lme(log.NO3 ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

E3<-residuals(M3)

plot(filter(sm, !is.na(log.NO3)) %>%dplyr::select(location),
     E3, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.NO3)) %>%dplyr::select(impact),
     E3, xlab="Location", ylab="Residuals")

qqnorm(residuals(M3))
qqline(residuals(M3))
ad.test(residuals(M3))

#log normal does not work

M3.1<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf1)

M3.2<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf2)

M3.3<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf3)

M3.4<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf4)

M3.5<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf5)

M3.6<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf6)

M3.7<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf7)

M3.8<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf8)

M3.9<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf9)

M3.10<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
           na.action=na.omit, data=sm, weights=vf10)

M3.11<-lme(log.NO3 ~ impact*f.time, random=~1 | location, 
           na.action=na.omit, data=sm, weights=vf11)

anova(M3.1,M3.2,M3.3,M3.4,M3.5,M3.6,M3.7,M3.8,M3.9,M3.10,M3.11)
#Look at 1, 4, 5, 6, 10, 11


E3.9<-residuals(M3.9)

qqnorm(residuals(M3.9))
qqline(residuals(M3.9))
ad.test(residuals(M3.9))

############################try log of 5th root####################################

sm$ng.NO3.5th<-(sm$ng.NO3)^(1/5)
sm$log.ng.NO3.5th<-log10(sm$ng.NO3.5th)

M1<-lme(log.NO3.5th ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try random factor and nesting

M2<-lme(log.NO3.5th ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

#try interaction with random factor

M3<-lme(log.NO3.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try interaction with random factor and nesting

M4<-lme(log.NO3.5th ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2,M3,M4)

#M3 is better

M3<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
        na.action=na.omit, data=sm, method="ML")

E3<-residuals(M3)

plot(filter(sm, !is.na(log.NO3.5th)) %>%dplyr::select(location),
     E3, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.NO3.5th)) %>%dplyr::select(impact),
     E3, xlab="Location", ylab="Residuals")

qqnorm(residuals(M3))
qqline(residuals(M3))
ad.test(residuals(M3))

M3.1<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf1)

M3.2<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf2)

M3.3<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf3)

M3.4<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf4)

M3.5<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf5)

M3.6<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf6)

M3.7<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf7)

M3.8<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf8)

M3.9<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
          na.action=na.omit, data=sm, weights=vf9)

M3.10<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
           na.action=na.omit, data=sm, weights=vf10)

M3.11<-lme(log.NO3.5th ~ impact*f.time, random=~1 | location, 
           na.action=na.omit, data=sm, weights=vf11)

anova(M3.1,M3.2,M3.3,M3.4,M3.5,M3.6,M3.7,M3.8,M3.9,M3.10,M3.11)
#Look at 1, 2, 4, 5, 6, 7, 8


E3.9<-residuals(M3.9)

qqnorm(residuals(M3.9))
qqline(residuals(M3.9))
ad.test(residuals(M3.9))

#Try 5th root with M1 Base model (No nesting)

M1.1<-lme(log.NO3.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1)

M1.2<-lme(log.NO3.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2)

M1.3<-lme(log.NO3.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3)

M1.4<-lme(log.NO3.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4)

M1.5<-lme(log.NO3.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5)

M1.6<-lme(log.NO3.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6)

M1.7<-lme(log.NO3.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7)

M1.8<-lme(log.NO3.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8)

M1.9<-lme(log.NO3.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9)

M1.10<-lme(log.NO3.5th ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10)

M1.11<-lme(log.NO3.5th ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11)

anova(M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11)


E1.9<-residuals(M1.9)

qqnorm(residuals(M1.9))
qqline(residuals(M1.9))
ad.test(residuals(M1.9))

#Log does not work for M1 or M2
#Log of the 5th root does not work for M1 or M2

#####################################################
#Get Full Model Statistics and Make Graph
#####################################################
#final model
M.full<-lme(log.ng.NO3.5th ~ impact*f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ f.time | impact)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are building a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 8)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)

log.ng.NO3.5th.emm = data.frame(cbind(xx,impact,event))
log.ng.NO3.5th.emm$emmean.raw = (10^(log.ng.NO3.5th.emm$emmean))^5
log.ng.NO3.5th.emm$SE.raw = (10^(log.ng.NO3.5th.emm$SE))^5
#log.NO3.5th.emm$emmean.raw = (10^(log.NO3.5th.emm$emmean))^5
#etc.  that will change your plot below since the error bars will be going in the other direction



#this is the final table you can use for plotting
log.ng.NO3.5th.emm

x = log.ng.NO3.5th.emm

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "13Jun16", "13Jun16", "4Aug16", "4Aug16", "19Sep16", "19Sep16", "6Nov16", "6Nov16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(data=x, 
       aes(x=cat.time, y=emmean.raw, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean.raw, ymax=emmean.raw+SE.raw), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black","white")) +
  xlab("Sample Event") +
  ylab(expression(Soil~Nitrate~(mg~NO[3]~g^{-1}~soil))) +
  labs(fill="Budworm Activity") +
  theme_bw() +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.12,0.98),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))


#this will save the file
ggsave('figures/emmno3.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")