#load data

tf<-read.table(file="tf.only.summary.csv", header=T, sep=",")

#set factors
str(tf)
tf$f.time<-factor(tf$time)
tf$f.plot<-factor(tf$plot)
tf$nest <- with(tf, factor(paste(location,f.plot)))

tf$ug.doc<-1000*(tf$doc)

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
M0<-gls(doc ~ impact+f.time, 
        na.action=na.omit, data=tf, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(doc ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

#try nesting

M2<-lme(doc ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

#try interaction with random factor

M3<-lme(doc ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

M4<-lme(doc ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

anova(M0,M1,M2,M3,M4)

#M4 has Lower AIC

#Look at residuals

E4<-residuals(M4)

plot(filter(tf, !is.na(doc)) %>%dplyr::select(location),
     E4, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(doc)) %>%dplyr::select(impact),
     E4, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4))
qqline(residuals(M4))
ad.test(residuals(M4))

x<-tf$doc[!is.na(tf$doc)]#removes na values from column
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

M4<-lme(doc ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

M4.1<-lme(doc ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf1)

M4.2<-lme(doc ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf2)

M4.3<-lme(doc ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf3)

M4.4<-lme(doc ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf4)

M4.5<-lme(doc ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf5)

M4.6<-lme(doc ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf6)

M4.7<-lme(doc ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf7)
#No Good

M4.8<-lme(doc ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf8)

M4.9<-lme(doc ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf9)

M4.10<-lme(doc ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf10)

M4.11<-lme(doc ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf11)
#No Good

anova(M4.1,M4.2,M4.3,M4.4,M4.5,M4.6,M4.8,M4.9,M4.10)
#M0.7 is best with varIdent as a function of time

E4.9<-residuals(M4.9)

plot(filter(tf, !is.na(doc)) %>%dplyr::select(location),
     E4.9, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(doc)) %>%dplyr::select(impact),
     E4.9, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4.9))
qqline(residuals(M4.9))
ad.test(residuals(M4.9))

x<-tf$doc[!is.na(tf$doc)]#removes na values from column
E4.9<-residuals(M4.9,type="normalized")
plot(M4.9) #residuals vs fitted values
plot(x, E4.9)

summary(M4.9)

#Auto Correlation Plot
E4.9<-residuals(M4.9)
x<-!is.na(tf$doc)
Efull<-vector(length=length(tf$doc))
Efull<-NA
Efull[x]<-E4.9
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#Not Auto Correlated

#but we will group by categorical time for graphing purposes
x <- group_by(tf, impact, f.time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(doc.mean = mean(doc, na.rm = TRUE), # na.rm = TRUE to remove missing values
            doc.sd=sd(doc, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(doc)), # of observations, excluding NAs. 
            doc.se=doc.sd/sqrt(n))
#this code defines graphing.interval as date format, but we won't use it for now
#x$graph.interval <-as.Date(as.character(x$graph.interval), format="%m/%d/%Y")

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=doc.mean)) + 
  geom_errorbar(aes(ymin=doc.mean-doc.se, ymax=doc.mean+doc.se), color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab(expression(Soil~Phosphate~(mg~PO[4]~g^{-1}~soil))) +
  scale_fill_manual(name="Budworm Activity", values=c("white", "black")) +
  expand_limits(y=300) +
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
ggsave('doc.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")

#### Go to Chapter 6 for Violation of Indpendence

#Dealing with Temporal Correlation

M4.12<-lme(doc ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf9, correlation=corCompSymm(form=~f.time))

M4.13<-lme(doc ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf9, correlation=corAR1(form=~f.time))
#Doesn't Work

cs1<-corARMA(c(0.2), p=1, q=0)
cs2<-corARMA(c(0.3, -0.3), p=2, q=0)

M4.14<-lme(doc ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf9, correlation=cs1)

M4.15<-lme(doc ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf9, correlation=cs2)

anova(M4.9,M4.12,M4.14,M4.15)

####################################Try log normalized data################################################

tf$log.doc<-log10(tf$doc)

M0<-gls(log.doc ~ impact+f.time, 
        na.action=na.omit, data=tf, method="ML")

M1<-lme(log.doc ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

M2<-lme(log.doc ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

M3<-lme(log.doc ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

M4<-lme(log.doc ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

anova(M0,M1,M2,M3,M4)

E1<-residuals(M1)

plot(filter(tf, !is.na(doc)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(doc)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1))
qqline(residuals(M1))
ad.test(residuals(M1))

M1.1<-lme(log.doc ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=tf, weights=vf1)

M1.2<-lme(log.doc ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=tf, weights=vf2)

M1.3<-lme(log.doc ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=tf, weights=vf3)

M1.4<-lme(log.doc ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=tf, weights=vf4)

M1.5<-lme(log.doc ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=tf, weights=vf5)

M1.6<-lme(log.doc ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=tf, weights=vf6)

M1.7<-lme(log.doc ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=tf, weights=vf7)

M1.8<-lme(log.doc ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=tf, weights=vf8)

M1.9<-lme(log.doc ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=tf, weights=vf9)

M1.10<-lme(log.doc ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=tf, weights=vf10)

M1.11<-lme(log.doc ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=tf, weights=vf11)

anova(M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11)
#Look at 2


E2.2<-residuals(M2.2)

qqnorm(residuals(M2.2))
qqline(residuals(M2.2))
ad.test(residuals(M2.2))

############################try log of 5th root####################################

tf$doc.5th<-(tf$doc)^(1/5)
tf$log.doc.5th<-log10(tf$doc.5th)
#CPA - there was a typo that named this lower case p instead of upper case P as written in code below

M1<-lme(log.doc.5th ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

#try random factor and nesting

M2<-lme(log.doc.5th ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

#try interaction with random factor

M3<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

#try interaction with random factor and nesting

M4<-lme(log.doc.5th ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

anova(M1,M2,M3,M4)

#M3 is better

AIC(M3)

E3<-residuals(M3)

plot(filter(tf, !is.na(log.doc.5th)) %>%dplyr::select(location),
     E3, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(log.doc.5th)) %>%dplyr::select(impact),
     E3, xlab="Location", ylab="Residuals")

qqnorm(residuals(M3))
qqline(residuals(M3))
ad.test(residuals(M3))

M3.1<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf1)

M3.2<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf2)

M3.3<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf3)

M3.4<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf4)

M3.5<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf5)

M3.6<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf6)

M3.7<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf7)

M3.8<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf8)

M3.9<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf9)

M3.10<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf10)

M3.11<-lme(log.doc.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf,weights=vf11)

anova(M3.1,M3.2,M3.3,M3.4,M3.5,M3.6,M3.7,M3.8,M3.9,M3.10,M3.11)

#M3.2 is best, but use M3 5th Root

AIC(M3.2)

E3.2<-residuals(M3.2)

plot(filter(tf, !is.na(log.doc.5th)) %>%dplyr::select(location),
     E3.2, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(log.doc.5th)) %>%dplyr::select(impact),
     E3.2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M3.2))
qqline(residuals(M3.2))
ad.test(residuals(M3.2))

#####################################################
#Get Full Model Statistics and Make Graph
#####################################################
#final model
M.full<-lme(log.doc.5th ~ impact*f.time, 
            random=~ 1 | location, na.action=na.omit, data=tf)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ impact | f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are buildocg a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 4)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4)

log.doc.5th.emm = data.frame(cbind(xx,impact,event))
log.doc.5th.emm$emmean.raw = (10^(log.doc.5th.emm$emmean))^5
log.doc.5th.emm$SE.raw = (10^(log.doc.5th.emm$SE))^5
#CPA - those are grouped wrong.  should be
#log.din.5th.emm$emmean.raw = (10^(log.din.5th.emm$emmean))^5
#etc.  that will change your plot below since the error bars will be going in the other direction



#this is the final table you can use for plotting
log.doc.5th.emm

x = log.doc.5th.emm

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15","29Oct15", "29Oct15", "8Nov15", "8Nov15")
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
  ylab(expression(Throughfall~doc~(ug~N~L^{-1}))) +
  labs(fill="Budworm Activity") +
  annotate("Text", x=1.25, y=198, label="Budworm Impact: P=0.1219", size=3) +
  annotate("Text", x=1.25, y=192, label="Sample Event: P<0.0001", size=3) +
  theme_bw() +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.2,0.98),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#this will save the file
ggsave('figures/emm doc.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")