#load data

tf<-read.table(file="tf.only.summary.csv", header=T, sep=",")

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

#set factors
str(tf)
tf$f.time<-factor(tf$time)
tf$f.plot<-factor(tf$plot)
tf$nest <- with(tf, factor(paste(location,f.plot)))

tf$ug.din<-1000*(tf$din)

tf$log.ug.din<-log10(tf$ug.din)
tf$ug.din.5th<-(tf$ug.din)^(1/5)
tf$log.ug.din.5th<-log10(tf$ug.din.5th)

#final model
M.full<-lme(log.ug.din.5th ~ impact+f.time, 
            random=~ 1 | location, na.action=na.omit, data=tf)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ impact | f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are building a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 10)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)

log.ug.din.5th.emm = data.frame(cbind(xx,impact,event))
log.ug.din.5th.emm$emmean.raw = (10^(log.ug.din.5th.emm$emmean))^5
log.ug.din.5th.emm$SE.raw = (10^(log.ug.din.5th.emm$SE))^5
#CPA - those are grouped wrong.  should be
#log.din.5th.emm$emmean.raw = (10^(log.din.5th.emm$emmean))^5
#etc.  that will change your plot below since the error bars will be going in the other direction



#this is the final table you can use for plotting
log.ug.din.5th.emm

x = log.ug.din.5th.emm

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15","29Oct15", "29Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "4Jun16", "4Jun16", "21Jun16", "21Jun16", "13Jul16", "13Jul16", "21Jul16", "21Jul16", "9Sep16", "9Sep16")
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
  ylab(expression(Throughfall~DIN~(ug~N~L^{-1}))) +
  labs(fill="Budworm Activity") +
  annotate("Text", x=2, y=110, label="Budworm Impact: P=0.1219", size=3) +
  annotate("Text", x=2, y=106, label="Sample Event: P<0.0001", size=3) +
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
ggsave('figures/emm Din.tiff',
       units="in",
       width=5.5,
       height=4.5,
       dpi=1200,
       compression="lzw")