library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)
library(multcomp)
library(MuMIn)
library(emmeans)
library(gridExtra)

sm<-read.table(file="soil.data.3.csv", header=T, sep=",")

vf9=varExp(form=~fitted(.)|time)

str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))

sm$ug.NH4<-1000*(sm$NH4)

sm$log.ug.NH4<-log10(sm$ug.NH4)

#final model
M.full<-lme(log.ug.NH4 ~ impact+f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm, weights=vf9)

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

log.ug.NH4.emm = data.frame(cbind(xx,impact,event))
log.ug.NH4.emm$emmean.raw = 10^(log.ug.NH4.emm$emmean)
log.ug.NH4.emm$SE.raw = 10^(log.ug.NH4.emm$SE)

#this is the final table you can use for plotting
log.ug.NH4.emm

x = log.ug.NH4.emm

#sorted table for tukeys

xx <- group_by(x, event) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(NH4.mean = mean(emmean.raw, na.rm = TRUE)) # na.rm = TRUE to remove missing values

sort(xx$NH4.mean, index.return=T) #Shows sample event lowest to highest

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "13Jun16", "13Jun16", "4Aug16", "4Aug16", "19Sep16", "19Sep16", "6Nov16", "6Nov16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

p.1 =
ggplot(data=x, 
       aes(x=cat.time, y=emmean.raw, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean.raw-SE.raw, ymax=emmean.raw+SE.raw), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("gray","white")) +
  #xlab("Sample Event") +
  ylab(expression(Soil~Ammonium~(μg~N~g^{-1}))) +
  labs(fill="Budworm Activity") +
  annotate("Text", x=1, y=6, label="a", size=3) +
  annotate("Text", x=2, y=8, label="a", size=3) +
  annotate("Text", x=2.1, y=8, label="b", size=3) +
  annotate("Text", x=3, y=11, label="b", size=3) +
  annotate("Text", x=3.1, y=11, label="c", size=3) +
  annotate("Text", x=4, y=16, label="b", size=3) +
  annotate("Text", x=5, y=4.5, label="c", size=3) +
  annotate("Text", x=6, y=7, label="a", size=3) +
  annotate("Text", x=7, y=9, label="c", size=3) +
  annotate("Text", x=8, y=15, label="d", size=3) +
  annotate("Text", hjust=0, x=.79, y=14, label="Budworm Impact: P=0.33", size=3) +
  annotate("Text", hjust=0, x=.79, y=13, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", x=0.6, y=15, label="A", size=5) +
  theme_bw() +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.2,0.95),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

#####################################Nitrate################################

str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))

sm$ng.NO3<-1000*(sm$NO3)

sm$log.ng.NO3<-log10(sm$ng.NO3)
sm$ng.NO3.5th<-(sm$ng.NO3)^(1/5)
sm$log.ng.NO3.5th<-log10(sm$ng.NO3.5th)

#final model
M.full<-lme(log.ng.NO3.5th ~ impact*f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ impact | f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are building a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 8)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)

log.ng.NO3.5th.emm = data.frame(cbind(xx,impact,event))
log.ng.NO3.5th.emm$emmean.raw = (10^(log.ng.NO3.5th.emm$emmean))^5
log.ng.NO3.5th.emm$SE.raw = (10^(log.ng.NO3.5th.emm$SE))^5
#log.NO3.5th.emm$emmean.raw = (10^(log.NO3.5th.emm$emmean))^5
#etc.  that will change your plot below since the error bars will be going in the other direction



#this is the final table you can use for plotting
log.ng.NO3.5th.emm

x = log.ng.NO3.5th.emm

#sorted table for tukeys

xx <- group_by(x, event) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(NO3.mean = mean(emmean.raw, na.rm = TRUE)) # na.rm = TRUE to remove missing values

sort(xx$NO3.mean, index.return=T) #Shows sample event lowest to highest

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "13Jun16", "13Jun16", "4Aug16", "4Aug16", "19Sep16", "19Sep16", "6Nov16", "6Nov16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

p.2 =
ggplot(data=x, 
       aes(x=cat.time, y=emmean.raw, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean.raw-SE.raw, ymax=emmean.raw+SE.raw), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("gray","white")) +
  #xlab("Sample Event") +
  ylim(-2.5,16) +
  ylab(expression(Soil~Nitrate~(μg~NO[3]~-N~g^{-1}~soil))) +
  labs(fill="Budworm Activity") +
  annotate("Text", hjust=0, x=.79, y=14, label="Budworm Impact: P=0.76", size=3) +
  annotate("Text", hjust=0, x=.79, y=12.5, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", hjust=0, x=.79, y=11, label="Interaction: P=0.0030", size=3) +
  annotate("Text", x=6, y=3, label="*", size=4) +
  annotate("Text", x=8, y=14, label="*", size=4) +
  annotate("Text", x=0.6, y=15.5, label="B", size=5) +
  theme_bw() +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.2,0.96),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

##############################################SRP############################################

vf6=varPower(form = ~ fitted (.)|impact)

str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))

sm$ug.P<-1000*(sm$P)

sm$log.ug.P<-log10(sm$ug.P)
sm$ug.P.5th<-(sm$ug.P)^(1/5)
sm$ug.log.P.5th<-log10(sm$ug.P.5th)

#final model
M.full<-lme(ug.log.P.5th ~ impact+f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm, weights=vf6)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ impact | f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are building a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 8)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8)

log.ug.P.5th.emm = data.frame(cbind(xx,impact,event))
log.ug.P.5th.emm$emmean.raw = (10^(log.ug.P.5th.emm$emmean))^5
log.ug.P.5th.emm$SE.raw = (10^(log.ug.P.5th.emm$SE))^5

#this is the final table you can use for plotting
log.ug.P.5th.emm

x = log.ug.P.5th.emm

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "13Jun16", "13Jun16", "4Aug16", "4Aug16", "19Sep16", "19Sep16", "6Nov16", "6Nov16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

p.3 =
ggplot(data=x, 
       aes(x=cat.time, y=emmean.raw, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean.raw-SE.raw, ymax=emmean.raw+SE.raw), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("gray","white")) +
  xlab("Sample Event") +
  ylab(expression(Soil~SRP~(μg~PO[4]^{-3}~-P~g^{-1}~soil))) +
  labs(fill="Budworm Activity") +
  annotate("Text", x=4.4, y=125, label="Budworm Impact: P=0.047", size=3) +
  annotate("Text", x=6.5, y=125, label="Sample Event: P=0.91", size=3) +
  annotate("Text", x=0.6, y=120, label="C", size=5) +
  theme_bw() +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.2,0.95),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#Make a two panel figure
gA <- ggplotGrob(p.1)  # set up figure
gB <- ggplotGrob(p.2)  # set up figure
gC <- ggplotGrob(p.3)  # set up figure

maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5], gC$widths[2:5])  # set up figure

gA$widths[2:5] <- as.list(maxWidth)  # set up figure
gB$widths[2:5] <- as.list(maxWidth)  # set up figure
gC$widths[2:5] <- as.list(maxWidth)  # set up figure

#maxHeight = grid::unit.pmax(gA$heights[7:9], gB$heights[7:9])

#gA$heights[7:9] <- as.list(maxHeight)  # set up figure
#gB$heights[7:9] <- as.list(maxHeight) 
#gC$heights[7:9] <- as.list(maxHeight) 

tiff(filename = 'figures/Soil_Chemistry.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")
grid.arrange(gA, gB, gC, nrow=3, ncol=1)  # push plot to device
dev.off()  # close device
