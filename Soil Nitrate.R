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
M0<-gls(NO3 ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(NO3 ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M2<-lme(NO3 ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M0,M2)

#M0 looks the best with no nesting and no random effect

#Look at residuals

E0<-residuals(M0)

plot(filter(sm, !is.na(NO3)) %>%dplyr::select(location),
     E0, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(NO3)) %>%dplyr::select(impact),
     E0, xlab="Location", ylab="Residuals")

qqnorm(residuals(M0))
qqline(residuals(M0))
ad.test(residuals(M0))

x<-sm$NO3[!is.na(sm$NO3)]#removes na values from column
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

M0<-gls(NO3 ~ impact+f.time, 
        na.action=na.omit, data=sm, method="REML")

M0.1<-gls(NO3 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf1)

M0.2<-gls(NO3 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf2)

M0.3<-gls(NO3 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf3)

M0.4<-gls(NO3 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf4)

M0.5<-gls(NO3 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf5)
#no covergence

M0.6<-gls(NO3 ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf6)

M0.7<-gls(NO3 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf7)
#no convergence

M0.8<-gls(NO3 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf8)

M0.9<-gls(NO3 ~ impact+f.time, 
          na.action=na.omit, data=sm, weights=vf9)

M0.10<-gls(NO3 ~ impact+f.time, 
           na.action=na.omit, data=sm, weights=vf10)
#no convergence

M0.11<-gls(NO3 ~ impact+f.time, 
           na.action=na.omit, data=sm, weights=vf11)
#no covergence

anova(M0,M0.1,M0.2,M0.3,M0.4, M0.6, M0.8, M0.9)
#M0.2 is best with varIdent as a function of time

E0.2<-residuals(M0.2)

plot(filter(sm, !is.na(NO3)) %>%dplyr::select(location),
     E0.2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(NO3)) %>%dplyr::select(impact),
     E0.2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M0.2))
qqline(residuals(M0.2))
ad.test(residuals(M0.2))

x<-sm$NO3[!is.na(sm$NO3)]#removes na values from column
E0.2<-residuals(M0.2,type="normalized")
plot(M0.2) #residuals vs fitted values
plot(x, E0.2)

#Auto Correlation Plot
E0.2<-residuals(M0.2)
x<-!is.na(sm$NO3)
Efull<-vector(length=length(sm$NO3))
Efull<-NA
Efull[x]<-E0.2
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")



summary(M0.2)

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

anova(M0.2)

#### Go to Chapter 6 for Violation of Indpendence
