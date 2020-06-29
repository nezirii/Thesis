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

tf<-read.table(file="tf.summary.csv", header=T, sep=",")

#set factors
str(tf)
tf$f.time<-factor(tf$time)
tf$f.plot<-factor(tf$plot)
tf$nest <- with(tf, factor(paste(location,f.plot)))

tf$log.srp<-log10(tf$srp)

#final model
M.full<-lme(log.srp ~ impact*f.time, random=~1|nest, 
            na.action=na.omit, data=tf)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are builsrpg a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 10)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)

log.srp.emm = data.frame(cbind(xx,impact,event))

#this is the final table you can use for plotting
log.srp.emm

x = log.srp.emm

xx <- group_by(x, event) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(srp.mean = mean(emmean, na.rm = TRUE)) # na.rm = TRUE to remove missing values

sort(xx$srp.mean, index.return=T) #Shows sample event lowest to highest

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15","29Oct15", "29Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "4Jun16", "4Jun16", "21Jun16", "21Jun16", "13Jul16", "13Jul16", "21Jul16", "21Jul16", "9Sep16", "9Sep16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

p.1 =
ggplot(data=x, 
       aes(x=cat.time, y=emmean, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("gray","white")) +
  #xlab("Sample Event") +
  ylab(expression(Throughfall~SRP~(log~mg~P~L^{-1}))) +
  labs(fill="Budworm Activity") +
  annotate("Text", hjust=0, x=.88, y=0.8,label="Budworm Impact: P=0.43", size=3) +
  annotate("Text", hjust=0, x=.88, y=0.9, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", x=0.6, y=1, label="A", size=5) +
  annotate("Text", x=1, y=0.4, label="c", size=4) +
  annotate("Text", x=2, y=0.2, label="c", size=4) +
  annotate("Text", x=3, y=0.1, label="b", size=4) +
  annotate("Text", x=3.15, y=0.1, label="c", size=4) +
  annotate("Text", x=4, y=0.9, label="b", size=4) +
  annotate("Text", x=5, y=0.5, label="a", size=4) +
  annotate("Text", x=6, y=0.3, label="b", size=4) +
  annotate("Text", x=7, y=0.2, label="c", size=4) +
  annotate("Text", x=8, y=0.1, label="d", size=4) +
  annotate("Text", x=9, y=0.9, label="b", size=4) +
  annotate("Text", x=10, y=0.5, label="c", size=4) +
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

###############################DOC#################################################

#set factors
str(tf)
tf$f.time<-factor(tf$time)
tf$f.plot<-factor(tf$plot)
tf$nest <- with(tf, factor(paste(location,f.plot)))

tf$log.doc<-log10(tf$doc)
tf$doc.5th<-(tf$doc)^(1/5)
tf$log.doc.5th<-log10(tf$doc.5th)

#final model
M.full<-lme(log.doc.5th ~ impact*f.time, 
            random=~ 1 | location, na.action=na.omit, data=tf)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are buildocg a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 10)
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)

log.doc.5th.emm = data.frame(cbind(xx,impact,event))
log.doc.5th.emm$emmean.raw = (10^(log.doc.5th.emm$emmean))^5
log.doc.5th.emm$SE.raw = (10^(log.doc.5th.emm$SE))^5
#CPA - those are grouped wrong.  should be
#log.din.5th.emm$emmean.raw = (10^(log.din.5th.emm$emmean))^5
#etc.  that will change your plot below since the error bars will be going in the other direction

#this is the final table you can use for plotting
log.doc.5th.emm

x = log.doc.5th.emm

xx <- group_by(x, event) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(doc.mean = mean(emmean.raw, na.rm = TRUE)) # na.rm = TRUE to remove missing values

sort(xx$doc.mean, index.return=T) #Shows sample event lowest to highest

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15","29Oct15", "29Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "4Jun16", "4Jun16", "21Jun16", "21Jun16", "13Jul16", "13Jul16", "21Jul16", "21Jul16", "9Sep16", "9Sep16")
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
  ylab(expression(Throughfall~DOC~(mg~C~L^{-1}))) +
  labs(fill="Budworm Activity") +
  annotate("Text", hjust=0, x=.88, y=355, label="Budworm Impact: P=0.26", size=3) +
  annotate("Text", hjust=0, x=.88, y=375, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", x=0.6, y=400, label="B", size=5) +
  annotate("Text", x=1, y=160, label="c", size=4) +
  annotate("Text", x=2, y=150, label="c", size=4) +
  annotate("Text", x=3, y=100, label="b", size=4) +
  annotate("Text", x=3.15, y=100, label="c", size=4) +
  annotate("Text", x=4, y=300, label="a", size=4) +
  annotate("Text", x=5, y=150, label="b", size=4) +
  annotate("Text", x=6, y=160, label="b", size=4) +
  annotate("Text", x=7, y=115, label="d", size=4) +
  annotate("Text", x=8, y=100, label="d", size=4) +
  annotate("Text", x=9, y=300, label="b", size=4) +
  annotate("Text", x=10, y=150, label="c", size=4) +
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


maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])  # set up figure

gA$widths[2:5] <- as.list(maxWidth)  # set up figure
gB$widths[2:5] <- as.list(maxWidth)  # set up figure

#maxHeight = grid::unit.pmax(gA$heights[7:9], gB$heights[7:9])

#gA$heights[7:9] <- as.list(maxHeight)  # set up figure
#gB$heights[7:9] <- as.list(maxHeight) 

tiff(filename = 'figures/TF_SRP_DOC.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")
grid.arrange(gA, gB, nrow=2, ncol=1)  # push plot to device
dev.off()  # close device
