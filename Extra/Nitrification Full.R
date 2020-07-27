#load data

sm<-read.table(file="soil.data.3.csv", header=T, sep=",")

#set factors
str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))


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
M0<-gls(net.nitrification ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(net.nitrification ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting

M2<-lme(net.nitrification ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

#try interaction with random factor

M3<-lme(net.nitrification ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M4<-lme(net.nitrification ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2,M3,M4)

#M1 looks the best

#Look at residuals

E1<-residuals(M1)

plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1))
qqline(residuals(M1))
ad.test(residuals(M1))

x<-sm$net.nitrification[!is.na(sm$net.nitrification)]#removes na values from column
E1<-residuals(M1,type="normalized")
plot(M1) #residuals vs fitted values
plot(x, E1)

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

M1<-lme(net.nitrification ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M1.1<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf1)

M1.2<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2)

M1.3<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf3)

M1.4<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf4)
#no covergence

M1.5<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf5)

M1.6<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6)

M1.7<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf7)

M1.8<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf8)
#no covergence

M1.9<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9)
#no covergence

M1.10<-lme(net.nitrification ~ impact+time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10)

M1.11<-lme(net.nitrification ~ impact+time, 
          random=~ 1 | location, na.action=na.omit, data=sm, weights=vf11)


anova(M1.1, M1.2, M1.3, M1.5, M1.6, M1.7, M1.10, M1.11)
#M1.2 is best

E1.2<-residuals(M1.2)

plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(location),
     E1.2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(impact),
     E1.2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.2))
qqline(residuals(M1.2))
ad.test(residuals(M1.2))

x<-sm$net.nitrification[!is.na(sm$net.nitrification)]#removes na values from column
E1.2<-residuals(M1.2,type="normalized")
plot(M1.2) #residuals vs fitted values
plot(x, E1.2)

summary(M1.2)

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
  annotate("Text", x=1.5, y=.0005, label="Interaction: P=0.6938", size=4) +
  annotate("Text", x=1.5, y=.00048, label="Budworm Impact: P=0.0982", size=4) +
  annotate("Text", x=1.5, y=.00046, label="Sampling Event: P<0.0001", size=4) +
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

###########################Dealing with Temporal Correlation#########################################


M1<-lme(net.nitrification ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))

M1.2<-lme(net.nitrification ~ impact+time, 
        random=~ 1 | location, na.action=na.omit, data=sm, weights=vf2)

M1.12<-lme(net.nitrification ~ impact+f.time,
        random=~ 1| location, na.action=na.omit, data=sm, weights=vf2, correlation=corCompSymm(form=~f.time))

M1.13<-lme(net.nitrification ~ impact+f.time, 
        random=~ 1 | location ,na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))


cs1<-corARMA(c(0.2), p=1, q=0)
cs2<-corARMA(c(0.3, -0.3), p=2, q=0)

M1.14<-lme(net.nitrification ~ impact+f.time, 
        random=~1 | location, na.action=na.omit, data=sm, weights=vf2, correlation=cs1)

M1.15<-lme(net.nitrification ~ impact+f.time, 
        random=~1 | location, na.action=na.omit, data=sm, weights=vf2, correlation=cs2)

anova(M1.2, M1.12, M1.13, M1.14, M1.15)

#M1.12 is still lower in AIC
#Look at M1.12
E1.12<-residuals(M1.12)

plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(location),
     E1.12, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(net.nitrification)) %>%dplyr::select(impact),
     E1.12, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1.12))
qqline(residuals(M1.12))
ad.test(residuals(M1.12))


######################################Try log normalized data###############################################

sm$log.net.nitrification<-log10(sm$net.nitrification)+1

M1<-lme(log.net.nitrification ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting

M2<-lme(log.net.nitrification ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

#try interaction with random factor

M3<-lme(log.net.nitrification ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M4<-lme(log.net.nitrification ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2,M3,M4)

#M3 is better

M3<-lme(log.net.nitrification ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")
AIC(M3)

E3<-residuals(M3)

plot(filter(sm, !is.na(log.net.nitrification)) %>%dplyr::select(location),
     E3, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.net.nitrification)) %>%dplyr::select(impact),
     E3, xlab="Location", ylab="Residuals")

qqnorm(residuals(M3))
qqline(residuals(M3))
ad.test(residuals(M3))

#log normal works!



#####################################################
#Get Full Model Statistics and Make Graph
#####################################################
#final model
M.full<-lme(log.net.nitrification ~ impact*f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ f.time | impact)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are building a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 2)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2)

log.net.nitrification.emm = data.frame(cbind(xx,impact,event))
log.net.nitrification.emm$emmean.raw = (10^(log.net.nitrification.emm$emmean))
log.net.nitrification.emm$SE.raw = (10^(log.net.nitrification.emm$emmean))

#this is the final table you can use for plotting
log.net.nitrification.emm

x = log.net.nitrification.emm

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("8May16", "8May16", "6Nov16", "6Nov16")
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
  ylab("Net Nitrification") +
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