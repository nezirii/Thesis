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

tf$ug.nh4<-1000*(tf$nh4)

tf$log.ug.nh4<-log10(tf$ug.nh4)

#final model
M.full<-lme(log.ug.nh4 ~ impact*f.time, random=~1|nest, 
            na.action=na.omit, data=tf)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ impact | f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are builnh4g a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 10)#you only have 9 sample periods in your data because you are missing time = 3, so this line isn't working since it's trying to put in 10 values.  Nothing below it will work either.
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)

log.ug.nh4.emm = data.frame(cbind(xx,impact,event))

#this is the final table you can use for plotting
log.ug.nh4.emm

x = log.ug.nh4.emm

xx <- group_by(x, event) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(NO3.mean = mean(emmean, na.rm = TRUE)) # na.rm = TRUE to remove missing values

sort(xx$NO3.mean, index.return=T) #Shows sample event lowest to highest


cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15","29Oct15", "29Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "4Jun16", "4Jun16", "21Jun16", "21Jun16", "13Jul16", "13Jul16", "21Jul16", "21Jul16", "9Sep16", "9Sep16")

x$cat.time<-as.character(cat.time)

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
  ylab(expression(Throughfall~Ammonium~(log~μg~N~L^{-1}))) +
  labs(fill="Budworm Activity") +
  annotate("Text", hjust=0, x=3.93, y=2.3, label="Budworm Impact: P=0.012", size=3) +
  annotate("Text", hjust=0, x=3.93, y=2.2, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", hjust=0, x=3.93, y=2.1, label="Interaction: P<0.0001", size=3) +
  annotate("Text", x=0.6, y=2.4, label="A", size=5) +
  theme_bw() +
  annotate("Text", x=1, y=1.8, label="*", size=4) +
  annotate("Text", x=6, y=1.9, label="*", size=4) +
  annotate("Text", x=7, y=2, label="*", size=4) +
  annotate("Text", x=8, y=2.4, label="*", size=4) +
  annotate("Text", x=9, y=1.45, label="*", size=4) +
  geom_hline(yintercept=0)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=8),
        legend.key=element_blank(),
        legend.position=c(0.5,0.96),
        legend.text=element_text(size=8),
        legend.background=element_blank(),
        legend.direction="horizontal",
        legend.key.size=unit(0.3, "cm"),
        axis.title.y=element_text(size=8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

####################### Now do NO3 ############################

tf$ug.no3.no2<-1000*(tf$no3.no2)

tf$log.ug.no3.no2<-log10(tf$ug.no3.no2)

#final model
M.full<-lme(log.ug.no3.no2 ~ impact*f.time, random=~1|nest, 
            na.action=na.omit, data=tf)

anova(M.full)

#this extracts what you need to look at pairwise differences and make a graphic
M.full.em = emmeans(M.full, ~ impact | f.time)

#this shows each pairwise difference (high v. low budworm at each sample event
pairs(M.full.em)

#the next several lines are builno3.no2g a table you can use in ggplot
xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

impact = rep((letters[seq(from = 1, to = 2)]), 10)#you only have 8 times contrasted...aren't there 10 sample periods?  For the same reason as ammonium, this line and subsequent lines won't work since your trying to make it out of 10 times instead of 8.  but it should be 10 in teh original data
impact<-recode(impact, "a" ="High")
impact<-recode(impact, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)

log.ug.no3.no2.emm = data.frame(cbind(xx,impact,event))
log.ug.no3.no2.emm$emmean.raw = 10^(log.ug.no3.no2.emm$emmean)
log.ug.no3.no2.emm$SE.raw = 10^(log.ug.no3.no2.emm$SE)

#this is the final table you can use for plotting
log.ug.no3.no2.emm

x = log.ug.no3.no2.emm

xx <- group_by(x, event) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(NO3.mean = mean(emmean.raw, na.rm = TRUE)) # na.rm = TRUE to remove missing values

sort(xx$NO3.mean, index.return=T) #Shows sample event lowest to highest

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15","29Oct15", "29Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "4Jun16", "4Jun16", "21Jun16", "21Jun16", "13Jul16", "13Jul16", "21Jul16", "21Jul16", "19Sep16", "19Sep16")
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
  ylab(expression(Throughfall~Nitrate~(μg~N~L^{-1}))) +
  labs(fill="Budworm Activity") +
  annotate("Text", hjust=0, x=.89, y=150, label="Budworm Impact: P=0.63", size=3) +
  annotate("Text", hjust=0, x=.89, y=142, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", hjust=0, x=.89, y=134, label="Interaction: P<0.0001", size=3) +
  theme_bw() +
  annotate("Text", x=5, y=160, label="*", size=4) +
  annotate("Text", x=8, y=160, label="*", size=4) +
  annotate("Text", x=9, y=70, label="*", size=4) +
  annotate("Text", x=0.6, y=160, label="B", size=5) +
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

tiff(filename = 'figures/TF_N.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")
grid.arrange(gA, gB, nrow=2, ncol=1)  # push plot to device
dev.off()  # close device
