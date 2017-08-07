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
M0<-gls(N.P ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(N.P ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M2<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M0,M2)

#M2 looks the best

#Look at residuals

E2<-residuals(M2)

plot(filter(sm, !is.na(N.P)) %>%dplyr::select(location),
     E2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(N.P)) %>%dplyr::select(impact),
     E2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

x<-sm$N.P[!is.na(sm$N.P)]#removes na values from column
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

M2.0<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm)

M2.1<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf1)

M2.2<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf2)

M2.3<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf3)
#No Convergence

M2.4<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf4)
#No Convergence

M2.5<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf5)
#No Convergence

M2.6<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf6)
#No Convergence

M2.7<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf7)
#No Convergence

M2.8<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf8)
#No Convergence

M2.9<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf9)
#No Convergence

M2.10<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf10)
#No Convergence

M2.11<-lme(N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf11)
#No Convergence

anova(M2.0, M2.1, M2.2)
#M0.7 is best with varIdent as a function of time

E2.2<-residuals(M2.2)

plot(filter(sm, !is.na(N.P)) %>%dplyr::select(location),
     E2.2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(N.P)) %>%dplyr::select(impact),
     E2.2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2.2))
qqline(residuals(M2.2))
ad.test(residuals(M2.2))

x<-sm$N.P[!is.na(sm$N.P)]#removes na values from column
E2.2<-residuals(M2.2,type="normalized")
plot(M2.2) #residuals vs fitted values
plot(x, E2.2)

summary(M2.2)

#Auto Correlation Plot
E2.2<-residuals(M2.2)
x<-!is.na(sm$N.P)
Efull<-vector(length=length(sm$N.P))
Efull<-NA
Efull[x]<-E2.2
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#Barely Auto Correlated

#one option is to group by graph.interval instead of time
#but we will group by categorical time for graphing purposes
x <- group_by(sm, impact, time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(N.P.mean = mean(N.P, na.rm = TRUE), # na.rm = TRUE to remove missing values
            N.P.sd=sd(N.P, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(N.P)), # of observations, excluding NAs. 
            N.P.se=N.P.sd/sqrt(n))
#this code defines graphing.interval as date format, but we won't use it for now
#x$graph.interval <-as.Date(as.character(x$graph.interval), format="%m/%d/%Y")

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=N.P.mean)) + 
  geom_errorbar(aes(ymin=N.P.mean-N.P.se, ymax=N.P.mean+N.P.se), color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab("Soil Inorganic N:P Molar Ratio") +
  scale_fill_manual(name="Budworm Activity", values=c("white", "black")) +
  expand_limits(y=.25) +
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

ggsave('np.tiff',
       units="in",
       width=5,
       height=4.5,
       dpi=1200,
       compression="lzw")
