#load data

sm<-read.table(file="soil.data.3.csv", header=T, sep=",")

#set factors
str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))

sm$ug.N.P<-1000*(sm$N.P)


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
M0<-gls(ug.N.P ~ impact+f.time, 
        na.action=na.omit, data=sm, method="ML")

#add random factor - refer to chapter 5 of zuur

M1<-lme(ug.N.P ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting

M2<-lme(ug.N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

#try interaction with random factor

M3<-lme(ug.N.P ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M4<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2,M3,M4)

#M4 looks the best

#Look at residuals

E4<-residuals(M4)

plot(filter(sm, !is.na(ug.N.P)) %>%dplyr::select(location),
     E4, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(ug.N.P)) %>%dplyr::select(impact),
     E4, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4))
qqline(residuals(M4))
ad.test(residuals(M4))

x<-sm$ug.N.P[!is.na(sm$ug.N.P)]#removes na values from column
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

M4<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

M4.1<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf1)

M4.2<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf2)

M4.3<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf3)

M4.4<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf4)

M4.5<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf5)

M4.6<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf6)

M4.7<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf7)

M4.8<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf8)

M4.9<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf9)

M4.10<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf10)

M4.11<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, weights=vf11)

anova(M4.1, M4.2,M4.3,M4.5, M4.9)

E4.9<-residuals(M4.9)

plot(filter(sm, !is.na(ug.N.P)) %>%dplyr::select(location),
     E4.9, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(ug.N.P)) %>%dplyr::select(impact),
     E4.9, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4.9))
qqline(residuals(M4.9))
ad.test(residuals(M4.9))

x<-sm$ug.N.P[!is.na(sm$ug.N.P)]#removes na values from column
E4.9<-residuals(M4.9,type="normalized")
plot(M4.9) #residuals vs fitted values
plot(x, E4.9)

summary(M4.9)

#Auto Correlation Plot
E4.9<-residuals(M4.9)
x<-!is.na(sm$ug.N.P)
Efull<-vector(length=length(sm$ug.N.P))
Efull<-NA
Efull[x]<-E4.9
acf(Efull, na.action=na.pass,
    main="Auto-correlation plot for residuals")

#Barely Auto Correlated

#one option is to group by graph.interval instead of time
#but we will group by categorical time for graphing purposes
x <- group_by(sm, impact, time) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(ug.N.P.mean = mean(ug.N.P, na.rm = TRUE), # na.rm = TRUE to remove missing values
            ug.N.P.sd=sd(ug.N.P, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(ug.N.P)), # of observations, excluding NAs. 
            ug.N.P.se=ug.N.P.sd/sqrt(n))
#this code defines graphing.interval as date format, but we won't use it for now
#x$graph.interval <-as.Date(as.character(x$graph.interval), format="%m/%d/%Y")

#make a new vector with the categorical times
cat.time<-c("Sep 15", "Oct 15", "Nov 15", "Apr 16", "Jun 16", "Aug 16", "Sep 16", "Nov 16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

ggplot(x, aes(x=cat.time, y=ug.N.P.mean)) + 
  geom_errorbar(aes(ymin=ug.N.P.mean-ug.N.P.se, ymax=ug.N.P.mean+ug.N.P.se), color="black", width=0.1, position=pd) + 
  geom_line(position=pd, color="black", aes(group=impact)) +
  geom_point(size=3, pch=21, aes(fill=impact)) +
  xlab("Sample Month and Year") +
  ylab("Soil Inorganic N:P Molar Ratio") +
  scale_fill_manual(name="Budworm Activity", values=c("white", "black")) +
  expand_limits(y=.25) +
  annotate("Text", x=7, y=1.25, label="Interaction: P<0.0001", size=4) +
  annotate("Text", x=7, y=1.15, label="Budworm Impact: P=0.0299", size=4) +
  annotate("Text", x=7, y=1.05, label="Sampling Event: P<0.0001", size=4) +
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

######################### Violation of Indpendence ###################################

#Dealing with Temporal Correlation

M3<-gls(ug.N.P ~ impact+f.time, 
        na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))

M4.9<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
          na.action=na.omit, data=sm, weights=vf9)

M4.12<-lme(ug.N.P ~ impact*f.time,random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf9, correlation=corCompSymm(form=~f.time))

M4.13<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, correlation=corCompSymm(form=~f.time))


cs1<-corARMA(c(0.2), p=1, q=0)
cs2<-corARMA(c(0.3, -0.3), p=2, q=0)

M4.14<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf9, correlation=cs1)

M4.15<-lme(ug.N.P ~ impact*f.time, random=~1|nest, 
           na.action=na.omit, data=sm, weights=vf9, correlation=cs2)

#M4.15 goes to convergence
anova(M4.9 ,M4.12, M4.13, M4.14)

#M4.9 is still lower in AIC
#Look at M4.14
E4.14<-residuals(M4.14)

plot(filter(sm, !is.na(ug.N.P)) %>%dplyr::select(location),
     E4.14, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(ug.N.P)) %>%dplyr::select(impact),
     E4.14, xlab="Location", ylab="Residuals")

qqnorm(residuals(M4.14))
qqline(residuals(M4.14))
ad.test(residuals(M4.14))

anova(M4.9,M4.14)
#4.6 is best model

E3<-residuals(M3)

plot(filter(sm, !is.na(ug.N.P)) %>%dplyr::select(location),
     E3, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(ug.N.P)) %>%dplyr::select(impact),
     E3, xlab="Location", ylab="Residuals")

qqnorm(residuals(M3))
qqline(residuals(M3))
ad.test(residuals(M3))

######################################Try log normalized data###############################################

sm$log.ug.N.P<-log10(sm$ug.N.P)

M1<-lme(log.ug.N.P ~ impact+f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

#try nesting

M2<-lme(log.ug.N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

#try interaction with random factor

M3<-lme(log.ug.N.P ~ impact*f.time, 
        random=~ 1 | location, na.action=na.omit, data=sm, method="ML")

M4<-lme(log.ug.N.P ~ impact*f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")

anova(M1,M2,M3,M4)

#M2 is better

M2<-lme(log.ug.N.P ~ impact+f.time, random=~1|nest, 
        na.action=na.omit, data=sm, method="ML")
AIC(M2)

E2<-residuals(M2)

plot(filter(sm, !is.na(log.ug.N.P)) %>%dplyr::select(location),
     E2, xlab="Location", ylab="Residuals")
plot(filter(sm, !is.na(log.ug.N.P)) %>%dplyr::select(impact),
     E2, xlab="Location", ylab="Residuals")

qqnorm(residuals(M2))
qqline(residuals(M2))
ad.test(residuals(M2))

#Log normal works!

#####################################################
#Get Full Model Statistics and Make Graph
#####################################################
#final model
M.full<-lme(log.ug.N.P ~ impact+f.time, random=~1|nest, 
            na.action=na.omit, data=sm, method="ML")

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ f.time | impact)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are building a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 8)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)

log.ug.N.P.emm = data.frame(cbind(xx,impact,event))
log.ug.N.P.emm$emmean.raw = (10^(log.ug.N.P.emm$emmean))
log.ug.N.P.emm$SE.raw = (10^(log.ug.N.P.emm$SE))
#etc.  that will change your plot below since the error bars will be going in the other direction



#this is the final table you can use for plotting
log.ug.N.P.emm

x = log.ug.N.P.emm

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
  ylab("Nitrogen to Phosphorous Ratio (ug)") +
  labs(fill="Budworm Activity") +
  annotate("Text", x=2, y=450, label="Budworm Impact: P=0.0057", size=3) +
  annotate("Text", x=2, y=435, label="Sample Event: P<0.0001", size=3) +
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
ggsave('figures/emm N to P Ratio.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")
