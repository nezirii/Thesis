#load data

st<-read.table(file="soil.temp.csv", header=T, sep=",")

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
str(st)
st$f.time<-factor(st$time)
st$f.plot<-factor(st$plot)
st$nest <- with(st, factor(paste(location,f.plot)))

st$log.a.temp.10cm<-log10(st$a.temp.10cm)
st$a.temp.10cm.5th<-(st$a.temp.10cm)^(1/5)
st$log.a.temp.10cm.5th<-log10(st$a.temp.10cm.5th)

M.full<-lme(log.a.temp.10cm.5th ~ impact*f.time, random=~1|nest, 
            na.action=na.omit, data=st)

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

log.a.temp.10cm.5th.emm = data.frame(cbind(xx,impact,event))
log.a.temp.10cm.5th.emm$emmean.raw = (10^(log.a.temp.10cm.5th.emm$emmean))^5
log.a.temp.10cm.5th.emm$SE.raw = (10^(log.a.temp.10cm.5th.emm$SE))^5
#CPA - those are grouped wrong.  should be
#log.P.5th.emm$emmean.raw = (10^(log.P.5th.emm$emmean))^5
#etc.  that will change your plot below since the error bars will be going in the other direction



#this is the final table you can use for plotting
log.a.temp.10cm.5th.emm

x = log.a.temp.10cm.5th.emm

#sorted table for tukeys

xx <- group_by(x, event) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(a.temp.10cm.mean = mean(emmean.raw, na.rm = TRUE)) # na.rm = TRUE to remove missing values

sort(xx$a.temp.10cm.mean, index.return=T) #Shows sample event lowest to highest

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
  ylab(expression(Soil~Temperature~at~10~cm~(C))) +
  labs(fill="Budworm Activity") +
  theme_bw() +
  annotate("Text", x=1.25, y=20, label="Budworm Impact: P=0.2312", size=3) +
  annotate("Text", x=1.25, y=19, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", x=1.25, y=18, label="Interaction: P<0.0001", size=3) +
  annotate("Text", x=1, y=10, label="a", size=3) +
  annotate("Text", x=2, y=14, label="b", size=3) +
  annotate("Text", x=3, y=13, label="c", size=3) +
  annotate("Text", x=4, y=12, label="b", size=3) +
  annotate("Text", x=5, y=17, label="b", size=3) +
  annotate("Text", x=5, y=16.5, label="d", size=3) +
  annotate("Text", x=6, y=9, label="a", size=3) +
  annotate("Text", x=7, y=10, label="d", size=3) +
  annotate("Text", x=8, y=5, label="e", size=3) +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.12,0.98),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_text(size=8),
        axis.text.x=element_text(size=8))


#this will save the file
ggsave('figures/emm temp 10 cm.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")
