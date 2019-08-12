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

#Look at mixed effects model
#start without random factor
M0<-gls(NH4 ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(NH4 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M2<-lme(NH4 ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2)

#M1 looks the best with no nesting and no random effect

#Look at residuals

E1<-residuals(M1)

plot(filter(sm, !is.na(NH4)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(NH4)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1))
qqline(residuals(M1))
ad.test(residuals(M1))

x<-sm$NH4[!is.na(sm$NH4)]#removes na values from column
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

M1<-lme(NH4 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M1.1<-lme(NH4 ~ impact+f.time,
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1)

M1.2<-lme(NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2)

M1.3<-lme(NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3)

M1.4<-lme(NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4)

M1.5<-lme(NH4 ~ impact+f.time,
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5)

M1.6<-lme(NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6)

M1.7<-lme(NH4 ~ impact+f.time,
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7)

M1.8<-lme(NH4 ~ impact+time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8)

M1.9<-lme(NH4 ~ impact+f.time,
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9)

M1.10<-lme(NH4 ~ impact+f.time,
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10)

M1.11<-lme(NH4 ~ impact+f.time,
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11)


anova(M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11)
#M1.7 has lowest Significant AIC

E1.11<-residuals(M1.11)

plot(filter(sm, !is.na(NH4)) %>%dplyr::select(location),
     E1.7, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(NH4)) %>%dplyr::select(impact),
     E1.7, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.11))
qqline(residuals(M1.11))
ad.test(residuals(M1.11))

x<-sm$NH4[!is.na(sm$NH4)]#removes na values from column
E1.7<-residuals(M1.7,type="normalized")
plot(M1.7) #residuals vs fitted values
plot(x, E1.7)

summary(M1.7)

#Auto Correlation Plot
E1.7<-residuals(M1.7)
x<-!is.na(sm$NH4)
Efull<-vector(length=length(sm$NH4))
Efull<-NA
Efull[x]<-E1.7
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#one option is to group by graph.interval instead of time
#but we will group by categorical time for graphing purposes
x <- group_by(sm, impact, f.time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(NH4.mean = mean(NH4, na.rm = TRUE), # na.rm = TRUE to remove missing values
            NH4.sd=sd(NH4, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(NH4)), # of observations, excluding NAs. 
            NH4.se=NH4.sd/sqrt(n))
#this code defines graphing.interval as date format, but we won't use it for now
#x$graph.interval <-as.Date(as.character(x$graph.interval), format="%m/%d/%Y")

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=NH4.mean)) + 
  geom_errorbar(aes(ymin=NH4.mean-NH4.se, ymax=NH4.mean+NH4.se), color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab(expression(Soil~Ammonium~(mg~NH[4]~-N~g^{-1}~soil))) +
  scale_fill_manual(name="Budworm Activity", values=c("white", "black")) +
  expand_limits(y=.03) +
  annotate("Text", x=7, y=.03, label="Interaction: P<0.0001", size=4) +
  annotate("Text", x=7, y=.028, label="Budworm Impact: P=0.0117", size=4) +
  annotate("Text", x=7, y=.026, label="Sampling Event: P<0.0001", size=4) +
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
ggsave('nh4.tiff',
       units="in",
       width=5,
       height=4.5,
       dpi=1200,
       compression="lzw")


#### Go to Chapter 6 for Violation of Indpendence

#Dealing with Temporal Correlation

M3<-gls(NH4 ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))

M4<-gls(NH4 ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corAR1(form=~f.time))
#Doesn't work

M1.12<-lme(NH4 ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7, correlation=corCompSymm(form=~f.time))

M1.12b<-lme(NH4 ~ impact+f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))

M1.13<-lme(NH4 ~ impact+f.time,  
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7, correlation=corAR1(form=~f.time))
#Doesn't Work

cs1<-corARMA(c(0.2), p=1, q=0)
cs2<-corARMA(c(0.3, -0.3), p=2, q=0)

M1.14<-lme(NH4 ~ impact+f.time,  
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7, correlation=cs1)

M1.15<-lme(NH4 ~ impact+f.time,  
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7, correlation=cs2)

anova(M1.7,M1.12,M1.12b,M1.14,M1.15,M3)
#M1.15 is slightly better

E1.15<-residuals(M1.15)

plot(filter(sm, !is.na(NH4)) %>%dplyr::select(location),
     E1.15, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(NH4)) %>%dplyr::select(impact),
     E1.15, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.15))
qqline(residuals(M1.15))
ad.test(residuals(M1.15))

#Log Normalized data

#start without random factor
M0<-gls(log.NH4 ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

M1<-lme(log.NH4 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M2<-lme(log.NH4 ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2)

#M1 looks the best with no nesting and no random effect

#Look at residuals

E1<-residuals(M1)

plot(filter(sm, !is.na(log.NH4)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.NH4)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

E1.8<-residuals(M1.8)

x<-sm$log.NH4[!is.na(sm$log.NH4)]#removes na values from column
E1<-residuals(M1,type="normalized")
plot(M1) #residuals vs fitted values
plot(x, E1)

#try alternate variance structures

M1<-lme(log.NH4 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M1.1<-gls(log.NH4 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf1)

M1.2<-gls(log.NH4 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf2)

M1.3<-gls(log.NH4 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf3)

M1.4<-gls(log.NH4 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf4)

M1.5<-gls(log.NH4 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf5)

M1.6<-gls(log.NH4 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf6)

M1.7<-gls(log.NH4 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf7)

M1.8<-gls(log.NH4 ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf8)

M1.9<-gls(log.NH4 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf9)

M1.10<-gls(log.NH4 ~ impact+f.time, 
           na.action=na.omit, data=sm, weights=vf10)

M1.11<-gls(log.NH4 ~ impact+f.time, 
           na.action=na.omit, data=sm, weights=vf11)


anova(M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11)
#Try M1.3,M1.4,M1.6,M1.7,M1.8,M1.10,M1.9
#M1.8 works best

E1.8<-residuals(M1.8)

plot(filter(sm, !is.na(log.NH4)) %>%dplyr::select(location),
     E1.8, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.NH4)) %>%dplyr::select(impact),
     E1.8, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.8))
qqline(residuals(M1.8))
ad.test(residuals(M1.8))

x<-sm$log.NH4[!is.na(sm$log.NH4)]#removes na values from column
E1.8<-residuals(M1.8,type="normalized")
plot(M1.8) #residuals vs fitted values
plot(x, E1.8)

summary(M1.8)

#Normalized with Different base model

M1<-lme(log.NH4 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M1.1<-lme(log.NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1)

M1.2<-lme(log.NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2)

M1.3<-lme(log.NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3)

M1.4<-lme(log.NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4)

M1.5<-lme(log.NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5)

M1.6<-lme(log.NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6)

M1.7<-lme(log.NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7)

M1.8<-lme(log.NH4 ~ impact+time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8)

M1.9<-lme(log.NH4 ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9)

M1.10<-lme(log.NH4 ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10)

M1.11<-lme(log.NH4 ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11)

anova(M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11)

E1.1<-residuals(M1.1)

qqnorm(residuals(M1.1))
qqline(residuals(M1.1))
ad.test(residuals(M1.1))

x<-sm$log.NH4[!is.na(sm$log.NH4)]#removes na values from column
E1.1<-residuals(M1.1,type="normalized")
plot(M1.1) #residuals vs fitted values
plot(x, E1.1)

#1.1 Works but its not quite as good as M1.8 using the base model without the random factor

#####################################################
#Get Full Model Statistics and Make Graph
#####################################################
#final model
M.full<-lme(log.NH4 ~ impact+f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ impact | f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are building a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 10)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)

log.NH4.emm = data.frame(cbind(xx,impact,event))
log.NH4.emm$emmean.raw = 10^(log.NH4.emm$emmean)-1
log.NH4.emm$SE.raw = 10^(log.NH4.emm$SE)-1

#this is the final table you can use for plotting
log.NH4.emm

x = Log.NH4.emm

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15", "29Oct15", "29Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "22May16", "22May16", "21Jun16", "21Jun16", "13Jul16", "13Jul16", "21Jul16", "21Jul16", "19Sep16", "19Sep16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(data=x, 
       aes(x=cat.time, y=emmean.raw, fill=budworm)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean.raw, ymax=emmean.raw-SE.raw), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black","white")) +
  xlab("Sample Event") +
  ylab(expression(DIN~net~throughfall~flux~(kg~N~ha^{-1}))) +
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