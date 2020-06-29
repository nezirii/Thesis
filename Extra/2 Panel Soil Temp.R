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

st<-read.table(file="soil.temp.csv", header=T, sep=",")

#set factors
str(st)
st$f.time<-factor(st$time)
st$f.plot<-factor(st$plot)
st$nest <- with(st, factor(paste(location,f.plot)))

#final model
M.full<-lme(o.temp.2cm~ impact*f.time, random=~1|nest, 
            na.action=na.omit, data=st)

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

o.temp.2cm.emm = data.frame(cbind(xx,impact,event))


#this is the final table you can use for plotting
o.temp.2cm.emm

x = o.temp.2cm.emm

#sorted table for tukeys

xx <- group_by(x, event) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(o.temp.2cm.mean = mean(emmean, na.rm = TRUE)) # na.rm = TRUE to remove missing values

sort(xx$o.temp.2cm.mean, index.return=T) #Shows sample event lowest to highest

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
       aes(x=cat.time, y=emmean, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("gray","white")) +
  #xlab("Sample Event") +
  ylab(expression(Soil~Temperature~at~2~cm~~(C))) +
  labs(fill="Budworm Activity") +
  annotate("Text", hjust=0, x=1, y=22, label="Budworm Impact: P=0.28", size=3) +
  annotate("Text", hjust=0, x=1, y=21, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", hjust=0, x=1, y=20, label="Interaction: P<0.0001", size=3) +
  annotate("Text", x=4, x=0.6, y=22, label="A", size=5) +
  theme_bw() +
  annotate("Text", x=2, y=10, label="*", size=4) +
  annotate("Text", x=4, y=12, label="*", size=4) +
  annotate("Text", x=5, y=17, label="*", size=4) +
  annotate("Text", x=6, y=23, label="*", size=4) +
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
        axis.title.x=element_blank(),
        axis.text.x=element_blank())

############################10 cm######################################

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
M.full.em = emmeans(M.full, ~ impact | f.time)

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

p.2 =
ggplot(data=x, 
       aes(x=cat.time, y=emmean.raw, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean.raw-SE.raw, ymax=emmean.raw+SE.raw), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("gray","white")) +
  xlab("Sample Event") +
  ylab(expression(Soil~Temperature~at~10~cm~(C))) +
  labs(fill="Budworm Activity") +
  theme_bw() +
  annotate("Text", hjust=0, x=1, y=16, label="Budworm Impact: P=0.23", size=3) +
  annotate("Text", hjust=0, x=1, y=15, label="Sample Event: P<0.0001", size=3) +
  annotate("Text", hjust=0, x=1, y=14, label="Interaction: P<0.0001", size=3) +
  annotate("Text", x=0.6, y=16, label="B", size=5) +
  annotate("Text", x=2, y=11, label="*", size=4) +
  annotate("Text", x=3, y=6, label="*", size=4) +
  annotate("Text", x=6, y=17, label="*", size=4) +
  annotate("Text", x=7, y=11, label="*", size=4) +
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

#Make a two panel figure
gA <- ggplotGrob(p.1)  # set up figure
gB <- ggplotGrob(p.2)  # set up figure


maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])  # set up figure

gA$widths[2:5] <- as.list(maxWidth)  # set up figure
gB$widths[2:5] <- as.list(maxWidth)  # set up figure

#maxHeight = grid::unit.pmax(gA$heights[7:9], gB$heights[7:9])

#gA$heights[7:9] <- as.list(maxHeight)  # set up figure
#gB$heights[7:9] <- as.list(maxHeight) 

tiff(filename = 'figures/Soil_Temp.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")
grid.arrange(gA, gB, nrow=2, ncol=1)  # push plot to device
dev.off()  # close device
