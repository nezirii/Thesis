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

str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))

sm$log.pct.moisture<-log10(sm$pct.moisture)

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

#sorted table for tukeys

xx <- group_by(x, event) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(log.pct.moisture.mean = mean(emmean.raw, na.rm = TRUE)) # na.rm = TRUE to remove missing values

sort(xx$log.pct.moisture.mean, index.return=T) #Shows sample event lowest to highest

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
  xlab("Sample Event") +
  ylab(" % Soil Moisture") +
  labs(fill="Budworm Activity") +
  theme_bw() +
  annotate("Text", x=4.5, y=28, label="Budworm Impact: P=0.86", size=3) +
  annotate("Text", x=4.5, y=26.5, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", x=0.6, y=28, label="A", size=5) +
  annotate("Text", x=1, y=12, label="a", size=3) +
  annotate("Text", x=6.1, y=29, label="a", size=3) +
  annotate("Text", x=2.1, y=26.5, label="d", size=3) +
  annotate("Text", x=2.1, y=27.7, label="a", size=3) +
  annotate("Text", x=3, y=16, label="c", size=3) +
  annotate("Text", x=4, y=24, label="c", size=3) +
  annotate("Text", x=5, y=12, label="b", size=3) +
  annotate("Text", x=6.1, y=28, label="e", size=3) +
  annotate("Text", x=7, y=16, label="a", size=3) +
  annotate("Text", x=8, y=24, label="c", size=3) +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.5,0.97),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#################################Organic Matter##############################

str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))

sm$pct.om.5th<-(sm$pct.om)^(1/5)
sm$log.pct.om.5th<-log10(sm$pct.om.5th)

vf10=varConstPower(form=~ fitted(.)|impact)

#final model
M.full<-lme(log.pct.om.5th ~ impact+f.time, 
            random=~ 1 | location, na.action=na.omit, data=sm, weights=vf10)

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

log.pct.om.5th.emm = data.frame(cbind(xx,impact,event))
log.pct.om.5th.emm$emmean.raw = (10^(log.pct.om.5th.emm$emmean))^5
log.pct.om.5th.emm$SE.raw = (10^(log.pct.om.5th.emm$SE))^5

#this is the final table you can use for plotting
log.pct.om.5th.emm

x = log.pct.om.5th.emm

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
  xlab("Sample Event") +
  ylab("% Organic Matter") +
  labs(fill="Budworm Activity") +
  theme_bw() +
  annotate("Text", x=2, y=11.9, label="Budworm Activity: P=0.49", size=3) +
  annotate("Text", x=2, y=11.3, label="Sample Event: P=0.70", size=3) +
  annotate("Text", x=0.6, y=12, label="B", size=5) +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(.2,0.97),
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


maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])  # set up figure

gA$widths[2:5] <- as.list(maxWidth)  # set up figure
gB$widths[2:5] <- as.list(maxWidth)  # set up figure

#maxHeight = grid::unit.pmax(gA$heights[7:9], gB$heights[7:9])

#gA$heights[7:9] <- as.list(maxHeight)  # set up figure
#gB$heights[7:9] <- as.list(maxHeight) 

tiff(filename = 'figures/soil h20 and organics.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")
grid.arrange(gA, gB, nrow=2, ncol=1)  # push plot to device
dev.off()  # close device