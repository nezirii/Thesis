#load data

fl<-read.table(file="frass.litter.csv", header=T, sep=",")

#set factors
str(fl)
fl$f.interval<-factor(fl$interval)
fl$f.rep<-factor(fl$rep)
fl$nest <- with(fl, factor(paste(site,f.rep)))

library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)

#Means Plot

x1 <- group_by(fl, treatment, f.interval) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(frass.N.m2.d.mean = mean(frass.N.m2.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            frass.N.m2.d.sd=sd(frass.N.m2.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(frass.N.m2.d)), # of observations, excluding NAs. 
            frass.N.m2.d.se=frass.N.m2.d.sd/sqrt(n))

x2 <- group_by(fl, treatment, f.interval) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(litter.N.m2.d.mean = mean(litter.N.m2.d, na.rm = TRUE), # na.rm = TRUE to remove missing values
            litter.N.m2.d.sd=sd(litter.N.m2.d, na.rm = TRUE),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(litter.N.m2.d)), # of observations, excluding NAs. 
            litter.N.m2.d.se=litter.N.m2.d.sd/sqrt(n))

#make a new vector with the categorical intervals
cat.interval<-c("6 Jul 15", "23 Jul 15", "28 Aug 15", "11 Oct 15", "19 Nov 15")
#force the new vector to be characters
x$cat.interval<-as.character(cat.interval)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.interval<-factor(x$cat.interval, levels=unique(x$cat.interval))

pd=position_dodge(0.1)

ggplot(data=fl, aes(x=cat.interval)) + 
  #geom_errorbar(aes(ymin=frass.N.m2.d.mean-frass.N.m2.d.se, ymax=frass.N.m2.d.mean+frass.N.m2.d.se), color="black", width=0.1, position=pd) + 
  geom_line(aes(y=x1, colour = "black")) +
  geom_line(aes(y=x2, colour = "gray"))  +
  geom_point(size=3, pch=21, aes(fill=treatment)) +
  xlab("Sample Event") +
  ylab("Frass of Litter Fall mg N d") +
  scale_fill_manual(name="Budworm Activity", values=c("white", "black")) +
  expand_limits(y=.25) +
  annotate("Text", x=7.6, y=1.1, label="GLS: P<0.0001", size=4) +
  theme_bw() +
  theme(legend.justification=c(0.03,0.6),
        legend.position=c(0.03,0.15),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size= 12),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

ggsave('frass litter.tiff',
       units="in",
       width=5,
       height=4.5,
       dpi=1200,
       compression="lzw")