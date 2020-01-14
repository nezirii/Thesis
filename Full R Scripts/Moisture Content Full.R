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
  annotate("Text", x=1.5, y=37, label="Interaction: P<0.0001", size=4) +
  annotate("Text", x=1.5, y=35.8, label="Budworm Impact: P=0.8606", size=4) +
  annotate("Text", x=1.5, y=34.6, label="Sampling Event: P<0.0001", size=4) +
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

#add random factor - refer to chapter 5 of zuur

M1<-lme(pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting

M2<-lme(pct.moisture ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

#try interaction with random factor

M3<-lme(pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M4<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M0,M1,M2,M3,M4)

#M4 looks the best

#Look at residuals

E4<-residuals(M4)

plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(location),
     E4, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(impact),
     E4, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4))
qqline(residuals(M4))
ad.test(residuals(M4))

x<-sm$pct.moisture[!is.na(sm$pct.moisture)]#removes na values from column
E4<-residuals(M4,type="normalized")
plot(M4) #residuals vs fitted values
plot(x, E4)

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

M4<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

M4.1<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf1)

M4.2<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf2)

M4.3<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf3)

M4.4<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf4)

M4.5<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf5)

M4.6<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf6)

M4.7<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf7)

M4.8<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf8)

M4.9<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf9)

M4.10<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf10)

M4.11<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf11)

anova(M4.1,M4.2,M4.3,M4.4,M4.5,M4.6,M4.7,M4.8,M4.9,M4.10,M4.11)
#M4.7 is best with varIdent as a function of time

E4.7<-residuals(M4.7)

plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(location),
     E4.7, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(impact),
     E4.7, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4.7))
qqline(residuals(M4.7))
ad.test(residuals(M4.7))

x<-sm$pct.moisture[!is.na(sm$pct.moisture)]#removes na values from column
E4.7<-residuals(M4.7,type="normalized")
plot(M4.7) #residuals vs fitted values
plot(x, E4.7)

summary(M4.7)

#Auto Correlation Plot
E4.7<-residuals(M4.7)
x<-!is.na(sm$pct.moisture)
Efull<-vector(length=length(sm$pct.moisture))
Efull<-NA
Efull[x]<-E4.7
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#Dealing with Temporal Correlation

M3<-gls(pct.moisture ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))

M4<-gls(pct.moisture ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corAR1(form=~f.time))
#Doesn't work

M4.12<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=corCompSymm(form=~f.time))

M4.13<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=corAR1(form=~f.time))

cs1<-corARMA(c(0.2), p=0, q=0)
cs2<-corARMA(c(0.2), p=0, q=1)
cs4<-corARMA(c(0.2), p=1, q=0)


M4.14<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs1) #No

M4.15<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs2)

M4.16<-lme(pct.moisture ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf7, correlation=cs4)

summary(M4.12)

E4.12<-residuals(M4.12)

plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(location),
     E4.12, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(pct.moisture)) %>%dplyr::select(impact),
     E4.12, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4.12))
qqline(residuals(M4.12))
ad.test(residuals(M4.12))

x<-sm$pct.moisture[!is.na(sm$pct.moisture)]#removes na values from column
E4.12<-residuals(M4.7,type="normalized")
plot(M4.12) #residuals vs fitted values
plot(x, E4.12)

summary(M4.12)

#M4.12 is still the best model AIC -489

#################################################Log normalized data#############################################

sm$log.pct.moisture<-log10(sm$pct.moisture)

M1<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting

M2<-lme(log.pct.moisture ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

#try interaction with random factor

M3<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M4<-lme(log.pct.moisture ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2,M3,M4)

#########M4 is best

E4<-residuals(M4)

plot(filter(sm, !is.na(log.pct.moisture)) %>%dplyr::select(location),
     E4, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.pct.moisture)) %>%dplyr::select(impact),
     E4, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4))
qqline(residuals(M4))
ad.test(residuals(M4))

summary(M4)

#Try alternate variance structures

M4<-lme(log.pct.moisture ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M4.1<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1, method="ML")

M4.2<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2, method="ML")

M4.3<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3, method="ML")

M4.4<-lme(log.pct.moisture ~ impact8f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4, method="ML")

M4.5<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5, method="ML")

M4.6<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6, method="ML")

M4.7<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7, method="ML")

M4.8<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8, method="ML")

M4.9<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9, method="ML")

M4.10<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10, method="ML")

M4.11<-lme(log.pct.moisture ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11, method="ML")

anova(M4,M4.1,M4.2,M4.3,M4.5,M4.6,M4.7,M4.8,M4.9,M4.10,M4.11)

E4.7<-residuals(M4.7)

plot(filter(sm, !is.na(log.pct.moisture)) %>%dplyr::select(location),
     E4.7, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.pct.moisture)) %>%dplyr::select(impact),
     E4.7, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4.7))
qqline(residuals(M4.7))
ad.test(residuals(M4.7))

####################### Log normalized M4 best

#####################################################
#Get Full Model Statistics and Make Graph
#####################################################
#final model
M.full<-lme(log.pct.moisture ~ impact+f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are building a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 8)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)

log.pct.moisture.emm = data.frame(cbind(xx,impact,event))
log.pct.moisture.emm$emmean.raw = (10^(log.pct.moisture.emm$emmean))
log.pct.moisture.emm$SE.raw = (10^(log.pct.moisture.emm$SE))


#this is the final table you can use for plotting
log.pct.moisture.emm

x = log.pct.moisture.emm

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
  ylab(" % Soil Moisture") +
  labs(fill="Budworm Activity") +
  theme_bw() +
  annotate("Text", x=2, y=29.5, label="Budworm Impact: P=0.8616", size=3) +
  annotate("Text", x=2, y=28.5, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", x=1, y=12, label="a", size=3) +
  annotate("Text", x=3, y=18, label="a", size=3) +
  annotate("Text", x=4, y=26, label="a", size=3) +
  annotate("Text", x=5, y=16, label="a", size=3) +
  annotate("Text", x=6.1, y=30, label="a", size=3) +
  annotate("Text", x=8, y=28, label="a", size=3) +
  annotate("Text", x=2, y=27.5, label="b", size=3) +
  annotate("Text", x=3, y=17, label="b", size=3) +
  annotate("Text", x=4, y=25, label="b", size=3) +
  annotate("Text", x=5, y=15, label="b", size=3) +
  annotate("Text", x=6.1, y=29, label="b", size=3) +
  annotate("Text", x=8, y=27, label="b", size=3) +
  annotate("Text", x=3, y=16, label="c", size=3) +
  annotate("Text", x=5, y=14, label="c", size=3) +
  annotate("Text", x=6.1, y=28, label="c", size=3) +
  annotate("Text", x=7, y=19, label="c", size=3) +
  annotate("Text", x=4, y=24, label="d", size=3) +
  annotate("Text", x=5, y=13, label="d", size=3) +
  annotate("Text", x=6.1, y=27, label="d", size=3) +
  annotate("Text", x=7, y=18, label="d", size=3) +
  annotate("Text", x=5, y=12, label="e", size=3) +
  annotate("Text", x=6.1, y=26, label="e", size=3) +
  annotate("Text", x=7, y=17, label="e", size=3) +
  annotate("Text", x=8, y=26, label="e", size=3) +
  annotate("Text", x=6.1, y=25, label="f", size=3) +
  annotate("Text", x=8, y=25, label="f", size=3) +
  annotate("Text", x=7, y=16, label="g", size=3) +
  annotate("Text", x=8, y=24, label="g", size=3) +
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
ggsave('figures/emm Moisture Content.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")

