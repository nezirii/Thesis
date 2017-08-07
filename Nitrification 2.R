#load data

sm<-read.table(file="nt.summary.csv", header=T, sep=",")

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
M0<-gls(net.nitrification ~ impact+time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M2<-lme(net.nitrification ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M0,M2)

#M0 looks the best with no nesting and no random effect

#Look at residuals

E0<-residuals(M0)

plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(location),
     E0, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(impact),
     E0, xlab="Location", ylab="Residuals")

qqnorm(residuals(M0))
qqline(residuals(M0))
ad.test(residuals(M0))

x<-sm$net.nitrification[!is.na(sm$net.nitrification)]#removes na values from column
E0<-residuals(M0,type="normalized")
plot(M0) #residuals vs fitted values
plot(x, E0)

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

M0<-gls(net.nitrification ~ impact+time, 
        na.action=na.omit, data=sm, method="REML")

M0.1<-gls(net.nitrification ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf1)

M0.2<-gls(net.nitrification ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf2)

M0.3<-gls(net.nitrification ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf3)

M0.4<-gls(net.nitrification ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf4)

M0.5<-gls(net.nitrification ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf5)

M0.6<-gls(net.nitrification ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf6)

M0.7<-gls(net.nitrification ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf7)

M0.8<-gls(net.nitrification ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf8)

M0.9<-gls(net.nitrification ~ impact+time, 
          na.action=na.omit, data=sm, weights=vf9)

M0.10<-gls(net.nitrification ~ impact+time, 
           na.action=na.omit, data=sm, weights=vf10)

M0.11<-gls(net.nitrification ~ impact+time, 
           na.action=na.omit, data=sm, weights=vf11)
#no covergence

anova(M0, M0.1, M0.2, M0.3, M0.4, M0.5, M0.6, M0.7, M0.8, M0.9, M0.10)
#M0.3 is best with varIdent as a function of time

E0.3<-residuals(M0.3)

plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(location),
     E0.3, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(impact),
     E0.3, xlab="Location", ylab="Residuals")

qqnorm(residuals(M0.3))
qqline(residuals(M0.3))
ad.test(residuals(M0.3))

x<-sm$net.nitrification[!is.na(sm$net.nitrification)]#removes na values from column
E0.3<-residuals(M0.3,type="normalized")
plot(M0.3) #residuals vs fitted values
plot(x, E0.3)

summary(M0.3)

#make a new vector with the categorical times
cat.time<-c("Apr 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

x <- group_by(sm, time, impact) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(net.nitrification.mean = abs(mean(net.nitrification, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            net.nitrification.sd=abs(sd(net.nitrification, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(net.nitrification)), # of observations, excluding NAs. 
            net.nitrification.se=net.nitrification.sd/sqrt(n))

ggplot(data=x, 
       aes(x=time, y=net.nitrification.mean, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=net.nitrification.mean, ymax=net.nitrification.mean+net.nitrification.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("white","black")) +
  xlab("Sample Month and Year") +
  ylab("Net Nitrification")  + 
  ylim(0,.0005) +
  labs(fill="Budworm Activity") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=12),
        legend.key=element_blank(), 
        legend.position=c(0.8,0.95), 
        legend.text=element_text(size=12), 
        legend.background=element_blank(), 
        legend.direction="horizontal", 
        legend.key.size=unit(0.3, "cm"), 
        axis.title.y=element_text(size=12), 
        axis.title.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12))

ggsave('nitrification.tiff',
       units="in",
       width=5,
       height=4.5,
       dpi=1200,
       compression="lzw")
##in the first lines of this block, aes(x= specifies the x axis, y is the response, fill is the coloration of the grouped columns