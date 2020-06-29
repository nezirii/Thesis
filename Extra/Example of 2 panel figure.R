#####################################################
#R Script to Analyze Net SRP TF Data Set
#from Bailey's Thesis
#
#23-Sep-17
#####################################################

#####################################################
#Load Packages
#####################################################
install.packages("nlme")
install.packages("lme4")
install.packages("lmerTest")
install.packages("dplyr")
install.packages("nortest")
install.packages("ggplot2")
install.packages("multcomp")
install.packages("gridExtra")

library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)
library(multcomp)
library(gridExtra)

#####################################################
#Run TF Water Flux Model
#####################################################
tf = read.table(file="tf.summary.csv", header=T, sep=",")

tf$f.sample.event=as.factor(tf$sample.event)
tf$f.rep=as.factor(tf$rep)

tf$nest <- with(tf, factor(paste(site,f.rep)))

tf<-subset(tf, type=="TF")

tf$l.col.volume.mm=log10(tf$col.volume.mm+1)

M.full<-lme(l.col.volume.mm ~ budworm*f.sample.event,  
            random = ~1|nest, na.action=na.omit, data=tf)

M.full.em = emmeans(M.full, ~ budworm | f.sample.event)

xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

budworm = rep((letters[seq(from = 1, to = 2)]), 10)
budworm<-recode(budworm, "a" ="High")
budworm<-recode(budworm, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)

TF.Water.flux.emm = data.frame(cbind(xx,budworm,event))
TF.Water.flux.emm$emmean.raw = 10^(TF.Water.flux.emm$emmean)-1
TF.Water.flux.emm$SE.raw = 10^(TF.Water.flux.emm$SE)-1

TF.Water.flux.emm

x = TF.Water.flux.emm

#make a new vector with the categorical times
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15", "29Oct15", "29Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "22May16", "22May16", "21Jun16", "21Jun16", "13Jul16", "13Jul16", "21Jul16", "21Jul16", "19Sep16", "19Sep16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

#change "low" to "background"
levels(x$budworm) <- c(levels(x$budworm), "Background") 
x$budworm[x$budworm=="Low"]  <- "Background" 

pd=position_dodge(0.1)

p.1 = 
  ggplot(data=x, aes(x=cat.time, y=emmean.raw, fill=budworm)) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean.raw, ymax=emmean.raw+SE.raw), width=0.2, 
                position=position_dodge(0.9)) +  
  xlab("Sample Event") +
  ylab(expression(Throughfall~water~flux~(mm))) +
  ylim(0, 60) +
  scale_fill_manual(name="Budworm Activity", values=c("black", "white")) +
  theme_bw() +
  theme(legend.justification=c(0.03,0.6),
        legend.position=c(0.65,0.80),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size= 12),
        legend.text=element_text(size=12),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) + 
  #annotate("text", x=1, y=13, label="bc", size=4) +
  #annotate("text", x=2, y=9, label="ab", size=4) +
  #annotate("text", x=2.7, y=14, label="c", size=4) +
  #annotate("text", x=3.8, y=59, label="d", size=4) +
  #annotate("text", x=5.1, y=8, label="a", size=4) +
  #annotate("text", x=6, y=20, label="c", size=4) +
  #annotate("text", x=7, y=12, label="bc", size=4) +
  #annotate("text", x=7.9, y=8, label="a", size=4) +
  annotate("text", x=8.8, y=26, label="*", size=4) +
  #annotate("text", x=10.2, y=6, label="a", size=4) +
  annotate("text", x=1, y=58, label="A", size=6)
  #annotate("text", x=0.8, y=60, label="budworm,", size=4, hjust=0) +
  #annotate("text", x=0.8, y=56, label="p=0.74", size=4, hjust=0) +
  #annotate("text", x=0.8, y=52, label="sample event,", size=4, hjust=0) +
  #annotate("text", x=0.8, y=48, label="p<0.0001", size=4, hjust=0) +
  #annotate("text", x=0.8, y=44, label="budworm*", size=4, hjust=0) +
  #annotate("text", x=1, y=40, label="sample event,", size=4, hjust=0) +
  #annotate("text", x=0.8, y=36, label="p<0.0001", size=4, hjust=0)


#####################################################
#Run NTF Water Flux Model
#####################################################

vf8=varIdent(form = ~1|f.sample.event)

#best overall model was non-normalized, nested, and alternate variance
M.full<-lme(net.h2o.flux ~ budworm*f.sample.event,  
            random = ~1|nest, na.action=na.omit, data=tf, weights=vf8)

M.full.em = emmeans(M.full, ~ budworm | f.sample.event)

xx = as.data.frame(summary(M.full.em))[c('emmean', 'SE')]

budworm = rep((letters[seq(from = 1, to = 2)]), 10)
budworm<-recode(budworm, "a" ="High")
budworm<-recode(budworm, "b" ="Low")
event = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10)

NTF.Water.flux.emm = data.frame(cbind(xx,budworm,event))

NTF.Water.flux.emm

#change "low" to "background"
levels(x$budworm) <- c(levels(x$budworm), "Background") 
x$budworm[x$budworm=="Low"]  <- "Background" 

x = NTF.Water.flux.emm

#make a new vector with the categorical times
cat.time<-c("11Sep15", "11Sep15", "11Oct15", "11Oct15", "29Oct15", "29Oct15", "8Nov15", "8Nov15", "8May16", "8May16", "22May16", "22May16", "21Jun16", "21Jun16", "13Jul16", "13Jul16", "21Jul16", "21Jul16", "19Sep16", "19Sep16")
#force the new vector to be characters
x$cat.time<-as.character(cat.time)
#force the new vector to be ordered in the order you gave it instead of alphabetical
x$cat.time<-factor(x$cat.time, levels=unique(x$cat.time))

pd=position_dodge(0.1)

p.2 = 
  ggplot(data=x, aes(x=cat.time, y=emmean, fill=budworm)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean), width=0.2, 
                position=position_dodge(0.9)) +  
  xlab("Sample Event") +
  ylab(expression(Net~throughfall~water~flux~(mm))) +
  ylim(-40,10) +
  scale_fill_manual(name="Budworm Activity", values=c("black", "white")) +
  theme_bw() +
  theme(legend.position="none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_text(size= 12),
        legend.text=element_text(size=12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  annotate("text", x=1, y=-28, label="*", size=4) +
  annotate("text", x=2, y=-18, label="+", size=4) +
  annotate("text", x=3, y=-30, label="*", size=4) +
  #annotate("text", x=4, y=-16, label="a", size=4) +
  #annotate("text", x=5, y=5, label="b", size=4) +
  annotate("text", x=6, y=-20, label="*", size=4) +
  #annotate("text", x=7, y=5, label="b", size=4) +
  annotate("text", x=8, y=-12, label="*", size=4) +
  #annotate("text", x=9, y=13, label="b", size=4) +
  #annotate("text", x=10, y=5, label="b", size=4) +
  annotate("text", x=1, y=8, label="B", size=6) +
  #annotate("text", x=0.8, y=60, label="budworm,", size=4, hjust=0) +
  #annotate("text", x=0.8, y=51, label="p=0.43", size=4, hjust=0) +
  #annotate("text", x=0.8, y=42, label="sample event,", size=4, hjust=0) +
  #annotate("text", x=0.8, y=33, label="p<0.0001", size=4, hjust=0) +
  #annotate("text", x=0.8, y=24, label="budworm*sample event,", size=4, hjust=0) +
  #annotate("text", x=0.8, y=15, label="p<0.0001", size=4, hjust=0) +
  geom_hline(yintercept=0)

#Make a two panel figure
gA <- ggplotGrob(p.1)  # set up figure
gB <- ggplotGrob(p.2)  # set up figure


maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])  # set up figure

gA$widths[2:5] <- as.list(maxWidth)  # set up figure
gB$widths[2:5] <- as.list(maxWidth)  # set up figure

#maxHeight = grid::unit.pmax(gA$heights[7:9], gB$heights[7:9])

#gA$heights[7:9] <- as.list(maxHeight)  # set up figure
#gB$heights[7:9] <- as.list(maxHeight) 

tiff(filename = 'figures/new figures/TF_NTF_WaterFlux.tiff', #open plotting device
     width = 6.5,
     height = 6.0,
     units = "in",
     res = 1200,
     compression = "lzw")
grid.arrange(gA, gB, nrow=2, ncol=1)  # push plot to device
dev.off()  # close device
