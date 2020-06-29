#load data

tf<-read.table(file="tf.only.summary.csv", header=T, sep=",")

#set factors
str(tf)
tf$f.time<-factor(tf$time)
tf$f.plot<-factor(tf$plot)
tf$nest <- with(tf, factor(paste(location,f.plot)))


tf$ug.srp<-1000*(tf$srp)

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
M0<-gls(srp ~ impact+f.time, 
        na.action=na.omit, data=tf, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(srp ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

#try nesting

M2<-lme(srp ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

#try interaction with random factor

M3<-lme(srp ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

M4<-lme(srp ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

anova(M0,M1,M2,M3,M4)

#M2 has Lower AIC

#Look at residuals

E2<-residuals(M2)

plot(filter(tf, !is.na(srp)) %>%dplyr::select(location),
     E2, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(srp)) %>%dplyr::select(impact),
     E2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

x<-tf$srp[!is.na(tf$srp)]#removes na values from column
E2<-residuals(M2,type="normalized")
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

M2<-lme(srp ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

M2.1<-lme(srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf1)

M2.2<-lme(srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf2)

M2.3<-lme(srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf3)

M2.4<-lme(srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf4)

M2.5<-lme(srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf5)

M2.6<-lme(srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf6)
#No Good

M2.7<-lme(srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf7)

M2.8<-lme(srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf8)
#No Good

M2.9<-lme(srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf9)
#No Good

M2.10<-lme(srp ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf10)
#No Good

M2.11<-lme(srp ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf11)

anova(M0.1,M0.2,M0.3,M0.4,M0.5,M0.7,M0.11)
#M0.7 is best with varIdent as a function of time

E2.7<-residuals(M2.7)

plot(filter(tf, !is.na(srp)) %>%dplyr::select(location),
     E2.7, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(srp)) %>%dplyr::select(impact),
     E2.7, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.7))
qqline(residuals(M2.7))
ad.test(residuals(M2.7))

x<-tf$srp[!is.na(tf$srp)]#removes na values from column
E2.7<-residuals(M2.7,type="normalized")
plot(M2.7) #residuals vs fitted values
plot(x, E2.7)

summary(M2.7)

#Auto Correlation Plot
E2.7<-residuals(M2.7)
x<-!is.na(tf$srp)
Efull<-vector(length=length(tf$srp))
Efull<-NA
Efull[x]<-E2.7
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#Not Auto Correlated

#but we will group by categorical time for graphing purposes
x <- group_by(tf, impact, f.time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(srp.mean = mean(srp, na.rm = TRUE), # na.rm = TRUE to remove missing values
            srp.sd=sd(srp, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(srp)), # of observations, exclusrpg NAs. 
            srp.se=srp.sd/sqrt(n))
#this code defines graphing.interval as date format, but we won't use it for now
#x$graph.interval <-as.Date(as.character(x$graph.interval), format="%m/%d/%Y")

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=srp.mean)) + 
  geom_errorbar(aes(ymin=srp.mean-srp.se, ymax=srp.mean+srp.se), color="black", width=0.1, position=pd) + 
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
ggsave('srp.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")


####################################Try log normalized data################################################

tf$log.srp<-log10(tf$srp)

M0<-gls(log.srp ~ impact+f.time, 
        na.action=na.omit, data=tf, method="ML")

M1<-lme(log.srp ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

M2<-lme(log.srp ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

M3<-lme(log.srp ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

M4<-lme(log.srp ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

anova(M0,M1,M2,M3,M4)

#M2 Better

E2<-residuals(M2)

plot(filter(tf, !is.na(srp)) %>%dplyr::select(location),
     E2, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(srp)) %>%dplyr::select(impact),
     E2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

M2.1<-lme(log.srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf1)

M2.2<-lme(log.srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf2)

M2.3<-lme(log.srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf3)
#No good

M2.4<-lme(log.srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf4)

M2.5<-lme(log.srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf5)

M2.6<-lme(log.srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf6)
#No good

M2.7<-lme(log.srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf7)
#No good

M2.8<-lme(log.srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf8)

M2.9<-lme(log.srp ~ impact+f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf9)
#No good

M2.10<-lme(log.srp ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf10)

M2.11<-lme(log.srp ~ impact+f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf11)
#No Good

anova(M2.1,M2.2,M2.4,M2.5,M2.8,M2.10)
#Look at 2


E2.2<-residuals(M2.2)

qqnorm(residuals(M2.2))
qqline(residuals(M2.2))
ad.test(residuals(M2.2))

############################try log of 5th root####################################

tf$srp.5th<-(tf$srp)^(1/5)
tf$log.srp.5th<-log10(tf$srp.5th)
#CPA - there was a typo that named this lower case p instead of upper case P as written in code below

M1<-lme(log.srp.5th ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

#try random factor and nesting

M2<-lme(log.srp.5th ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

#try interaction with random factor

M3<-lme(log.srp.5th ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

#try interaction with random factor and nesting

M4<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

anova(M1,M2,M3,M4)

#M2 is better

AIC(M2)

E2<-residuals(M2)

plot(filter(tf, !is.na(log.srp.5th)) %>%dplyr::select(location),
     E2, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(log.srp.5th)) %>%dplyr::select(impact),
     E2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

M2.1<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf1)

M2.2<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf2)

M2.3<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf3)

M2.4<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf4)
#No Good

M2.5<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf5)
#No good

M2.6<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf6)

M2.7<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf7)
#No good

M2.8<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf8)
#No good

M2.9<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=tf, weights=vf9)
#No good

M2.10<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf10)
#No good

M2.11<-lme(log.srp.5th ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=tf, weights=vf11)
#No good

anova(M2.1,M2.2,M2.3,M2.6,M2.11)

E2.2<-residuals(M2.2)

plot(filter(tf, !is.na(log.srp.5th)) %>%dplyr::select(location),
     E2.2, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(log.srp.5th)) %>%dplyr::select(impact),
     E2.2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.2))
qqline(residuals(M2.2))
ad.test(residuals(M2.2))

#####################################################
#Get Full Model Statistics and Make Graph
#####################################################
#final model
M.full<-lme(log.srp ~ impact*f.time, random=~1|nest, 
            na.action=na.omit, data=tf)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ impact | f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are builsrpg a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 10)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)#missing time 3 like with ammonium, so same problem here.

log.srp.emm = data.frame(cbind(xx,impact,event))
log.srp.emm$emmean.raw = (10^(log.srp.emm$emmean))^5
log.srp.emm$SE.raw = (10^(log.srp.emm$SE))^5
#CPA - those are grouped wrong.  should be
#etc.  that will change your plot below since the error bars will be going in the other direction



#this is the final table you can use for plotting
log.srp.emm

x = log.srp.emm

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15","29Oct15", "29Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "4Jun16", "4Jun16", "21Jun16", "21Jun16", "13Jul16", "13Jul16", "21Jul16", "21Jul16", "9Sep16", "9Sep16")
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
  ylab(expression(Throughfall~srp~(ug~N~L^{-1}))) +
  labs(fill="Budworm Activity") +
  annotate("Text", x=2, y=110, label="Budworm Impact: P=0.1219", size=3) +
  annotate("Text", x=2, y=106, label="Sample Event: P<0.0001", size=3) +
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
ggsave('figures/emm srp.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")