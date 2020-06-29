#load data

st<-read.table(file="soil.temp.csv", header=T, sep=",")

#set factors
str(st)
st$f.time<-factor(st$time)
st$f.plot<-factor(st$plot)
st$nest <- with(st, factor(paste(location,f.plot)))


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
M0<-gls(temp.avg~ impact+f.time, 
        na.action=na.omit, data=st, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(temp.avg~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=st, method="ML")

#try nesting

M2<-lme(temp.avg~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=st, method="ML")

#try interaction with random factor

M3<-lme(temp.avg~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=st, method="ML")

M4<-lme(temp.avg~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=st, method="ML")

anova(M0,M1,M2,M3,M4)

#M4 has Lower AIC than M2 which was my original choice. M4 is also significantly different with p=0.0427

#Look at residuals

E4<-residuals(M4)

plot(filter(st, !is.na(temp.avg)) %>%dplyr::select(location),
     E4, xlab="Location", ylab="Residuals")
plot(filter(st, !is.na(temp.avg)) %>%dplyr::select(impact),
     E4, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4))
qqline(residuals(M4))
ad.test(residuals(M4))

x<-st$temp.avg[!is.na(st$temp.avg)]#removes na values from column
E4<-residuals(M4,type="normalized")
plot(M4) #residuals vs fitted values
plot(x, E4)

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

#Use Model 4

M4<-lme(temp.avg~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=st)

M4.1<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf1)

M4.2<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf2)

M4.3<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf3)

M4.4<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf4)

M4.5<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf5)

M4.6<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf6)

M4.7<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf7)

M4.8<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf8)

M4.9<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf9)

M4.10<-lme(temp.avg~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=st, weights=vf10)

M4.11<-lme(temp.avg~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=st, weights=vf11)

#4.3 and 4.5 go to convergence
anova(M4,M4.1,M4.2,M4.3,M4.4,M4.5,M4.6,M4.7,M4.8,M4.9,M4.10,M4.11)
#M4.9 is best with varIdent as a function of impact

E4.9<-residuals(M4.9)

plot(filter(st, !is.na(temp.avg)) %>%dplyr::select(location),
     E4.9, xlab="Location", ylab="Residuals")
plot(filter(st, !is.na(temp.avg)) %>%dplyr::select(impact),
     E4.9, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4.9))
qqline(residuals(M4.9))
ad.test(residuals(M4.9))

x<-st$P[!is.na(st$temp.avg)]#removes na values from column
EM4.9<-residuals(M4.9,type="normalized")
plot(M4.9) #residuals vs fitted values
plot(x, M4.9)

summary(M4.9)

#Auto Correlation Plot
E4.9<-residuals(M4.9)
x<-!is.na(st$temp.avg)
Efull<-vector(length=length(st$temp.avg))
Efull<-NA
Efull[x]<-E4.9
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

##################################Line Plot from Origianl Analysis#######################################

#we will group by categorical time for graphing purposes
x <- group_by(st, impact, f.time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(temp.avg.mean = mean(temp.avg, na.rm = TRUE), # na.rm = TRUE to remove missing values
            temp.avg.sd=sd(temp.avg, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(temp.avg)), # of observations, excluding NAs. 
            temp.avg.se=temp.avg.sd/sqrt(n))
#this code defines graphing.interval as date format, but we won't use it for now
#x$graph.interval <-as.Date(as.character(x$graph.interval), format="%m/%d/%Y")

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=temp.avg.mean)) + 
  geom_errorbar(aes(ymin=temp.avg.mean-temp.avg.se, ymax=temp.avg.mean+temp.avg.se), color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab(expression(Soil~Temperature~(C))) +
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


######################### Violation of Indpendence ###################################

#Dealing with Temporal Correlation

M3<-gls(temp.avg~ impact+f.time, 
        na.action=na.omit, data=st, correlation=corCompSymm(form=~f.time))

M4.9<-lme(temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf9)

M4.12<-lme(temp.avg~ impact*f.time,random=~1|nest, 
           na.action=na.omit, data=st, weights=vf9, correlation=corCompSymm(form=~f.time))

M4.13<-lme(temp.avg~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=st, correlation=corCompSymm(form=~f.time))


cs1<-corARMA(c(0.2), p=1, q=0)
cs2<-corARMA(c(0.3, -0.3), p=2, q=0)

M4.14<-lme(temp.avg~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=st, weights=vf9, correlation=cs1)

M4.15<-lme(temp.avg~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=st, weights=vf9, correlation=cs2)

anova(M4.9,M4.12,M4.13,M4.14,M4.15)

#M4.9 is still lower in AIC
#Look at M4.12
E4.12<-residuals(M4.12)

plot(filter(st, !is.na(temp.avg)) %>%dplyr::select(location),
     E4.12, xlab="Location", ylab="Residuals")
plot(filter(st, !is.na(temp.avg)) %>%dplyr::select(impact),
     E4.12, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4.12))
qqline(residuals(M4.12))
ad.test(residuals(M4.12))

anova(M4.9,M4.12)
#4.9 is best model

E3<-residuals(M3)

plot(filter(st, !is.na(temp.avg)) %>%dplyr::select(location),
     E3, xlab="Location", ylab="Residuals")
plot(filter(st, !is.na(temp.avg)) %>%dplyr::select(impact),
     E3, xlab="Location", ylab="Residuals")

qqnorm(residuals(M3))
qqline(residuals(M3))
ad.test(residuals(M3))

######################################Try log normalized data###############################################

st$log.temp.avg<-log10(st$temp.avg)

M1<-lme(log.temp.avg~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=st, method="ML")

#try nesting

M2<-lme(log.temp.avg~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=st, method="ML")

#try interaction with random factor

M3<-lme(log.temp.avg~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=st, method="ML")

M4<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=st, method="ML")

anova(M1,M2,M3,M4)

#M4 is better

AIC(M4)

E4<-residuals(M4)

plot(filter(st, !is.na(log.temp.avg)) %>%dplyr::select(location),
     E4, xlab="Location", ylab="Residuals")
plot(filter(st, !is.na(log.temp.avg)) %>%dplyr::select(impact),
     E4, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4))
qqline(residuals(M4))
ad.test(residuals(M4))

#log normal does not work

M4.1<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf1)

M4.2<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf2)

M4.3<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf3)

M4.4<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf4)

M4.5<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf5)
#Does not work

M4.6<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf6)

M4.7<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf7)

M4.8<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf8)

M4.9<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf9)

M4.10<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=st, weights=vf10)

M4.11<-lme(log.temp.avg~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=st, weights=vf11)

anova(M4.1,M4.2,M4.3,M4.4,M4.6,M4.7,M4.8,M4.9,M4.10,M4.11)

AIC(M4.11)

E4.10<-residuals(M4.10)

qqnorm(residuals(M4.10))
qqline(residuals(M4.10))
ad.test(residuals(M4.10))

############################try log of 5th root####################################

st$temp.avg.5th<-(st$temp.avg)^(1/5)
st$log.temp.avg.5th<-log10(st$temp.avg.5th)
#CPA - there was a typo that named this lower case p instead of upper case P as written in code below

M1<-lme(log.temp.avg.5th ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=st, method="ML")

#try random factor and nesting

M2<-lme(log.temp.avg.5th ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=st, method="ML")

#try interaction with random factor

M3<-lme(log.temp.avg.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=st, method="ML")

#try interaction with random factor and nesting

M4<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=st, method="ML")

anova(M1,M2,M3,M4)

#M4 is better

M4<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=st, method="ML")
AIC(M4)

E4<-residuals(M4)

plot(filter(st, !is.na(log.temp.avg.5th)) %>%dplyr::select(location),
     E4, xlab="Location", ylab="Residuals")
plot(filter(st, !is.na(log.temp.avg.5th)) %>%dplyr::select(impact),
     E4, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4))
qqline(residuals(M4))
ad.test(residuals(M4))

M4.1<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf1)

M4.2<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf2)

M4.3<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf3)

M4.4<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf4)

M4.5<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf5)
#Does not work

M4.6<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf6)

M4.7<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf7)

M4.8<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf8)

M4.9<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=st, weights=vf9)

M4.10<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=st, weights=vf10)

M4.11<-lme(log.temp.avg.5th ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=st, weights=vf11)

anova(M4.1,M4.2,M4.3,M4.4,M4.6,M4.7,M4.8,M4.9,M4.11)



E4.6<-residuals(M4.6)

qqnorm(residuals(M4.6))
qqline(residuals(M4.6))
ad.test(residuals(M4.6))

#Try 5th root with M1 Base model (No nesting)

M1.1<-lme(log.temp.avg.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=st, weights=vf1)

M1.2<-lme(log.temp.avg.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=st, weights=vf2)

M1.3<-lme(log.temp.avg.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=st, weights=vf3)

M1.4<-lme(log.temp.avg.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=st, weights=vf4)

M1.5<-lme(log.temp.avg.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=st, weights=vf5)

M1.6<-lme(log.temp.avg.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=st, weights=vf6)

M1.7<-lme(log.temp.avg.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=st, weights=vf7)

M1.8<-lme(log.temp.avg.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=st, weights=vf8)

M1.9<-lme(log.temp.avg.5th ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=st, weights=vf9)

M1.10<-lme(log.temp.avg.5th ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=st, weights=vf10)

M1.11<-lme(log.temp.avg.5th ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=st, weights=vf11)

anova(M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.11)
#Look at 1, 2, 4, 5, 6, 10, 11


E1.9<-residuals(M1.9)

qqnorm(residuals(M1.9))
qqline(residuals(M1.9))
ad.test(residuals(M1.9))



#####################################################
#Get Full Model Statistics and Make Graph
#####################################################
#final model
M.full<-lme(log.temp.avg.5th ~ impact+f.time, 
            random=~ 1 | location, na.action=na.omit, data=st, weights=vf11)

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

log.temp.avg.5th.emm = data.frame(cbind(xx,impact,event))
log.temp.avg.5th.emm$emmean.raw = (10^(log.temp.avg.5th.emm$emmean))^5
log.temp.avg.5th.emm$SE.raw = (10^(log.temp.avg.5th.emm$emmean))^5
#CPA - those are grouped wrong.  should be
#log.P.5th.emm$emmean.raw = (10^(log.P.5th.emm$emmean))^5
#etc.  that will change your plot below since the error bars will be going in the other direction



#this is the final table you can use for plotting
log.temp.avg.5th.emm

x = log.temp.avg.5th.emm

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
  ylab(expression(Soil~Temperature~(C))) +
  labs(fill="Budworm Activity") +
  theme_bw() +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.5,0.98),
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

Hi Clay