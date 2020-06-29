#load data

decomp=read.table(file="decomp.rates.2.5.csv", header=T, sep=",")

#set factors

decomp$f.plot<-factor(decomp$plot)

library(nlme)
library(lme4)
library(lmerTest)
library(dplyr)
library(nortest)
library(ggplot2)

#start with linear model

M0<-lm(rate.of.decomposition ~ 
         impact+leaf.type+location+
         f.plot, data = decomp)
#I revised this just to clean it up.  you didn't all those factors
#and you're already using the data argument so don't need to use
#decomp$ to call your variables

M1<-lm(rate.of.decomposition ~ 
         impact*leaf.type+location, data = decomp, na.action=na.omit)
#plot shouldn't be a main factor since it's basically a rep
#sart with the interaction term so it's equivalent to M2 below

E1<-residuals(M1)

plot(E1)

summary(M1)


#Look at mixed effects model
#add random factor - refer to chapter 5 of zuur

M2<-lme(rate.of.decomposition ~ impact*leaf.type, 
        random=~ 1 | location, data=decomp, na.action=na.omit)

E2<-residuals(M2)
plot(E2)
summary(M2)

#test M1 and M2 to see if random effects are better than using location as a block 
anova(M0,M1,M2)
#something is wrong with Stand Up in the first model

#Here an alternate first model that doesn't use location at all
M1<-gls(rate.of.decomposition ~ 
          impact*leaf.type, data = decomp, na.action=na.omit)

E1<-residuals(M1)

plot(E1)

summary(M1)
anova(M1)

#test M1 and M2 to see if random effects are better than using location as a block 
anova(M1,M2)

#looks like random effects don't matter much

M3<-lmer(rate.of.decomposition ~ impact*leaf.type + 
           (plot|location), 
         data=decomp)

E3<-residuals(M3)

plot(E3)

AIC(M1,M2,M3)#M1 is the Best

summary(M1)

E1<-residuals(M1)

plot(E1)

plot(filter(decomp, !is.na(rate.of.decomposition)) %>%dplyr::select(location),
     E1, xlab="Location", ylab="Residuals")
plot(filter(decomp, !is.na(rate.of.decomposition)) %>%dplyr::select(impact),
     E1, xlab="Location", ylab="Residuals")

qqnorm(residuals(M1))
qqline(residuals(M1))
ad.test(residuals(M1))

#Bar Plot

x <- group_by(decomp, leaf.type, impact) %>%  # Grouping function causes subsequent functions to aggregate by season and reach
  summarize(rate.of.decomposition.mean = abs(mean(rate.of.decomposition, na.rm = TRUE)), # na.rm = TRUE to remove missing values
            rate.of.decomposition.sd=abs(sd(rate.of.decomposition, na.rm = TRUE)),  # na.rm = TRUE to remove missing values
            n = sum(!is.na(rate.of.decomposition)), # of observations, excluding NAs. 
            rate.of.decomposition.se=rate.of.decomposition.sd/sqrt(n))

ggplot(data=x, 
       aes(x=leaf.type, y=rate.of.decomposition.mean, fill=impact)) + 
  geom_bar(stat="identity", position=position_dodge(), color = "black") + 
  geom_errorbar(aes(ymin=rate.of.decomposition.mean, ymax=rate.of.decomposition.mean+rate.of.decomposition.se), width=0.2, 
                position=position_dodge(0.9)) + 
  scale_fill_manual(values=c("white","black")) +
  xlab("Leaf Type") +
  ylab("Decomposition Rate (-k)")  + #I changed your y axis label
  ylim(0,.0015) +
  labs(fill="Budworm Activity") +
  annotate("Text", x=.77, y=.0014, label="Budworm Impact: P=0.0024", size=4) +
  annotate("Text", x=0.68, y=.00135, label="Leaf Type: P=0.68", size=4) +
  annotate("Text", x=.95, y=.0013, label="Budworm Impact:Leaf Type Interaction: P=0.79", size=4) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_text(size=12),
        legend.key=element_blank(), 
        legend.position=c(0.2,0.95), 
        legend.text=element_text(size=12), 
        legend.background=element_blank(), 
        legend.direction="horizontal", 
        legend.key.size=unit(0.3, "cm"), 
        axis.title.y=element_text(size=12), 
        axis.title.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.text.x=element_text(size=12))
##in the first lines of this block, aes(x= specifies the x axis, y is the response, fill is the coloration of the grouped columns

ggsave('figures/decomp by leaf.tiff',
       units="in",
       width=5,
       height=4.5,
       dpi=1200,
       compression="lzw")

