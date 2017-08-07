#load data

tf<-read.table(file="tf.only.summary.csv", header=T, sep=",")

#set factors
str(tf)
tf$f.time<-factor(tf$time)
tf$f.plot<-factor(tf$plot)
tf$f.collector<-factor(tf$collector)
tf$nest <- with(tf, factor(paste(location,f.plot)))


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
M0<-gls(doc ~ impact+f.time, 
        na.action=na.omit, data=tf, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(doc ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

#try nesting
M2<-lme(doc ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=tf, method="ML")

anova(M0,M2)

#M0 looks the best 

#Look at residuals

E0<-residuals(M0)

plot(filter(tf, !is.na(doc)) %>%dplyr::select(location),
     E0, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(doc)) %>%dplyr::select(impact),
     E0, xlab="Location", ylab="Residuals")

qqnorm(residuals(M0))
qqline(residuals(M0))
ad.test(residuals(M0))

x<-tf$doc[!is.na(tf$doc)]#removes na values from column
E0<-residuals(M0,type="normalized")
plot(M0) #residuals vs fitted values
plot(x, E0)

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

M0<-gls(doc ~ impact+f.time, 
        na.action=na.omit, data=tf)

M0.1<-gls(doc ~ impact+f.time, 
          na.action=na.omit, data=tf, weights=vf1)

M0.2<-gls(doc ~ impact+f.time, 
          na.action=na.omit, data=tf, weights=vf2)

M0.3<-gls(doc ~ impact+f.time, 
          na.action=na.omit, data=tf, weights=vf3)

M0.4<-gls(doc ~ impact+f.time, 
          na.action=na.omit, data=tf, weights=vf4)

M0.5<-gls(doc ~ impact+f.time, 
          na.action=na.omit, data=tf, weights=vf5)

M0.6<-gls(doc ~ impact+f.time, 
          na.action=na.omit, data=tf, weights=vf6)

M0.7<-gls(doc ~ impact+f.time, 
          na.action=na.omit, data=tf, weights=vf7)
#No Convergence

M0.8<-gls(doc ~ impact+f.time, 
          na.action=na.omit, data=tf, weights=vf8)
#No Convergence

M0.9<-gls(doc ~ impact+f.time, 
          na.action=na.omit, data=tf, weights=vf9)
#No Convergence

M0.10<-gls(doc ~ impact+f.time, 
           na.action=na.omit, data=tf, weights=vf10)

M0.11<-gls(doc ~ impact+f.time, 
           na.action=na.omit, data=tf, weights=vf11)
#No Convergence

anova(M0,M0.1,M0.2,M0.3,M0.4,M0.5,M0.6,M0.10)
#M1.10 is best with varIdent as a function of time

E0.10<-residuals(M0.10)

plot(filter(tf, !is.na(doc)) %>%dplyr::select(location),
     E0.10, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(doc)) %>%dplyr::select(impact),
     E0.10, xlab="Location", ylab="Residuals")

qqnorm(residuals(M0.10))
qqline(residuals(M0.10))
ad.test(residuals(M0.10))

x<-tf$doc[!is.na(tf$doc)]#removes na values from column
E0.10<-residuals(M0.10,type="normalized")
plot(M0.10) #residuals vs fitted values
plot(x, E0.10)

summary(M0.10)

#Auto Correlation Plot
E0.10<-residuals(M0.10)
x<-!is.na(tf$doc)
Efull<-vector(length=length(tf$doc))
Efull<-NA
Efull[x]<-E0.10
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

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

M3<-gls(doc ~ impact+f.time, 
        na.action=na.omit, data=tf, correlation=corCompSymm(form=~f.time))

M4<-gls(doc ~ impact+f.time, 
        na.action=na.omit, data=tf, correlation=corAR1(form=~f.time))
#Doesn't work

M1.12<-lme(doc ~ impact+f.time, random=~1|location, 
           na.action=na.omit, data=tf, weights=vf10, correlation=corCompSymm(form=~f.time))

M1.13<-lme(doc ~ impact+f.time, random=~1|location, 
           na.action=na.omit, data=tf, weights=vf10, correlation=corAR1(form=~f.time))
#Doesn't Work

cs1<-corARMA(c(0.2), p=1, q=0)
cs2<-corARMA(c(0.3, -0.3), p=2, q=0)

M1.14<-lme(doc ~ impact+f.time, random=~1|location, 
           na.action=na.omit, data=tf, weights=vf10, correlation=cs1)

E1.14<-residuals(M1.14)

plot(filter(tf, !is.na(doc)) %>%dplyr::select(location),
     E1.14, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(doc)) %>%dplyr::select(impact),
     E1.14, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.14))
qqline(residuals(M1.14))
ad.test(residuals(M1.14))

M1.15<-lme(doc ~ impact+f.time, random=~1|location, 
           na.action=na.omit, data=tf, weights=vf10, correlation=cs2)

anova(M1.10,M3,M1.12,M1.14,M1.15)
#try looking at M1.15 and M3

E3<-residuals(M3)

plot(filter(tf, !is.na(doc)) %>%dplyr::select(location),
     E3, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(doc)) %>%dplyr::select(impact),
     E3, xlab="Location", ylab="Residuals")

qqnorm(residuals(M3))
qqline(residuals(M3))
ad.test(residuals(M3))

#Try log normalized data

M0<-gls(log.doc ~ impact+f.time, 
        na.action=na.omit, data=tf, method="ML")

M1<-lme(log.doc ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=tf, method="ML")

anova(M0,M1)

#M1 is better

M2<-lmer(log.doc ~ impact+f.time + 
           (f.plot|location), 
         na.action=na.omit, data=tf)
AIC(M2)

E2<-residuals(M2)

plot(filter(tf, !is.na(doc)) %>%dplyr::select(location),
     E2, xlab="Location", ylab="Residuals")
plot(filter(tf, !is.na(doc)) %>%dplyr::select(impact),
     E2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

#log normal does not work
#try log of 5th root