#load data

sm<-read.table(file="soil.data.3.csv", header=T, sep=",")

library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)
library(multcomp)
library(MuMIn)
library(emmeans)

#set factors

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
M.full.em = emmeans(M.full, ~ f.time | impact)

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

ggplot(data=x, 
       aes(x=cat.time, y=emmean.raw, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean.raw, ymax=emmean.raw+SE.raw), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("black","white")) +
  xlab("Sample Event") +
  ylab(expression(Soil~Ammonium~(ug~NH[4]~-N~g^{-1}~soil))) +
  labs(fill="Budworm Activity") +
  annotate("Text", x=1, y=5, label="a", size=3) +
  annotate("Text", x=2, y=8.5, label="a", size=3) +
  annotate("Text", x=2, y=8, label="b", size=3) +
  annotate("Text", x=3, y=12, label="b", size=3) +
  annotate("Text", x=3, y=11.5, label="c", size=3) +
  annotate("Text", x=4, y=17.5, label="b", size=3) +
  annotate("Text", x=5, y=4.5, label="c", size=3) +
  annotate("Text", x=6, y=6, label="a", size=3) +
  annotate("Text", x=7, y=8, label="c", size=3) +
  annotate("Text", x=8, y=13.5, label="d", size=3) +
  annotate("Text", x=2, y=17.5, label="Budworm Impact: P=0.3326", size=3) +
  annotate("Text", x=2, y=17, label="Sample Event: P<0.0001", size=3) +
  theme_bw() +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.2,0.97),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))

#this will save the file
ggsave('figures/emm soil NH4.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")
