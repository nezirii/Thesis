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
str(sm)
sm$f.time<-factor(sm$time)
sm$f.plot<-factor(sm$plot)
sm$nest <- with(sm, factor(paste(location,f.plot)))

t.test(sm$net.nitrification, 
       sm$time,
       paired=TRUE,
       conf.level=0.95)

#Bar Plot

#make a new vector with the categorical times.  you'll need to adjust this 
#for your soil graphics
cat.time<-c("16Apr16", "16Apr16", "6Nov16", "6Nov16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

x <- group_by(sm, cat.time, impact) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(net.nitrification.mean = abs(mean(net.nitrification, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            net.nitrification.sd=abs(sd(net.nitrification, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(net.nitrification)), # of observations, excluding NAs. 
            net.nitrification.se=net.nitrification.sd/sqrt(n))

ggplot(data=x, 
       aes(x=cat.time, y=net.nitrification.mean, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=net.nitrification.mean, ymax=net.nitrification.mean+net.nitrification.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("white","black")) +
  xlab("Sample Event") +
  ylab("Net Nitrification")  +
  ylim(0,.00015) +
  labs(fill="Budworm Activity") +
  annotate("Text", x=1, y=.00015, label="Paired t-test: P<0.0001", size=4) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=12),
        legend.key=element_blank(), 
        legend.position=c(0.7,0.95), 
        legend.text=element_text(size=12), 
        legend.background=element_blank(), 
        legend.direction="horizontal", 
        legend.key.size=unit(0.3, "cm"), 
        axis.title.y=element_text(size=12), 
        axis.title.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12))
##in the first lines of this block, aes(x= specifies the x axis, y is the response, fill is the coloration of the grouped columns

ggsave('net nitrification bar plot.tiff',
       units="in",
       width=5,
       height=3.25,
       dpi=1200,
       compression="lzw")