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
M0<-gls(TN.inorg ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(TN.inorg ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M2<-lme(TN.inorg ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2)

#M1 looks the best with no nesting and no random effect

#Look at residuals

E1<-residuals(M1)

plot(filter(sm, !is.na(TN.inorg)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(TN.inorg)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1))
qqline(residuals(M1))
ad.test(residuals(M1))

x<-sm$TN.inorg[!is.na(sm$TN.inorg)]#removes na values from column
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

M1<-gls(TN.inorg ~ impact+f.time, 
        na.action=na.omit, data=sm, method="REML")

M1.1<-gls(TN.inorg ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf1)

M1.2<-gls(TN.inorg ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf2)

M0.3<-gls(TN.inorg ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf3)
#No Convergence

M1.4<-gls(TN.inorg ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf4)
#No Convergence

M1.5<-gls(TN.inorg ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf5)

M1.6<-gls(TN.inorg ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf6)

M1.7<-gls(TN.inorg ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf7)

M1.8<-gls(TN.inorg ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf8)
#No Convergence

M1.9<-gls(TN.inorg ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf9)
#No Convergence

M1.10<-gls(TN.inorg ~ impact+f.time, 
           na.action=na.omit, data=sm, weights=vf10)
#No Convergence

M1.11<-gls(TN.inorg ~ impact+f.time, 
           na.action=na.omit, data=sm, weights=vf11)

anova(M1, M1.1, M1.2, M1.5, M1.6, M1.7, M1.11)
#M1.2 is best with varIdent as a function of time

E1.2<-residuals(M1.2)

plot(filter(sm, !is.na(TN.inorg)) %>%dplyr::select(location),
     E1.2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(TN.inorg)) %>%dplyr::select(impact),
     E1.2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.2))
qqline(residuals(M1.2))
ad.test(residuals(M1.2))

x<-sm$TN.inorg[!is.na(sm$TN.inorg)]#removes na values from column
E1.2<-residuals(M1.2,type="normalized")
plot(M1.2) #residuals vs fitted values
plot(x, E1.2)

summary(M1.2)

#Auto Correlation Plot
E1.2<-residuals(M1.2)
x<-!is.na(sm$pct.moisture)
Efull<-vector(length=length(sm$pct.moisture))
Efull<-NA
Efull[x]<-E1.2
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#one option is to group by graph.interval instead of time
#but we will group by categorical time for graphing purposes
x <- group_by(sm, impact, f.time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(TN.inorg.mean = mean(TN.inorg, na.rm = TRUE), # na.rm = TRUE to remove missing values
            TN.inorg.sd=sd(TN.inorg, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(TN.inorg)), # of observations, excluding NAs. 
            TN.inorg.se=TN.inorg.sd/sqrt(n))
#this code defines graphing.interval as date format, but we won't use it for now
#x$graph.interval <-as.Date(as.character(x$graph.interval), format="%m/%d/%Y")

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=TN.inorg.mean)) + 
  geom_errorbar(aes(ymin=TN.inorg.mean-TN.inorg.se, ymax=TN.inorg.mean+TN.inorg.se), color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab(expression(Total~Soil~Inorganic~N~(mg~N~g^{-1}~soil))) +
  scale_fill_manual(name="Budworm Activity", values=c("white", "black")) +
  expand_limits(y=.03) +
  annotate("Text", x=6, y=.05, label="Interaction: P<0.0001", size=4) +
  annotate("Text", x=6, y=.048, label="Budworm Impact: P=0.4361", size=4) +
  annotate("Text", x=6, y=.046, label="Sampling Event: P<0.0001", size=4) +
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
ggsave('tn inorg.tiff',
       units="in",
       width=5,
       height=4.5,
       dpi=1200,
       compression="lzw")

#Auto Correlation
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
M0<-gls(log.TN.inorg ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

M1<-lme(log.TN.inorg ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M2<-lme(log.TN.inorg ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M0)

#M1 looks the best with no nesting and no random effect

#Look at residuals

E1<-residuals(M1)

plot(filter(sm, !is.na(log.TN.inorg)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.TN.inorg)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1))
qqline(residuals(M1))
ad.test(residuals(M1))

x<-sm$log.TN.inorg[!is.na(sm$log.TN.inorg)]#removes na values from column
E1<-residuals(M1,type="normalized")
plot(M1) #residuals vs fitted values
plot(x, E1)


M1<-gls(log.TN.inorg ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="REML")

M1.1<-lme(log.TN.inorg ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1)

M1.2<-lme(log.TN.inorg ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2)

M1.3<-lme(log.TN.inorg ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3)

M1.4<-lme(log.TN.inorg ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4)

M1.5<-lme(log.TN.inorg ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5)

M1.6<-lme(log.TN.inorg ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6)

M1.7<-lme(log.TN.inorg ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7)

M1.8<-lme(log.TN.inorg ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8)

M1.9<-lme(log.TN.inorg ~ impact+f.time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9)

M1.10<-lme(log.TN.inorg ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10)

M1.11<-lme(log.TN.inorg ~ impact+f.time, 
           random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11)

anova(M1.1,M1.2,M1.3,M1.4,M1.5,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11)
#Try M1.2,M1.3,M1.4,M1.6,M1.7,M1.8,M1.9,M1.10,M1.11
#Base model with no random factor in GLS was no good either
#Log normalizing does not work

E1.11<-residuals(M1.11)

plot(filter(sm, !is.na(TN.inorg)) %>%dplyr::select(location),
     E1.2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(TN.inorg)) %>%dplyr::select(impact),
     E1.2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.11))
qqline(residuals(M1.11))
ad.test(residuals(M1.11))

x<-sm$TN.inorg[!is.na(sm$TN.inorg)]#removes na values from column
E1.2<-residuals(M1.2,type="normalized")
plot(M1.2) #residuals vs fitted values
plot(x, E1.2)

summary(M1.2)