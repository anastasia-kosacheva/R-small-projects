# We are going to use a data set called InsectSprays. 6 different insect sprays 
#(1 IndependentVariable with 6 levels) were tested to see if 
#there was a difference in the number of insectsfound 
#in the field after each spraying (Dependent Variable).
# Results from an experiment to compare **yields (as measured by dried weight of plants)** 
#obtained under a control and two different treatment conditions.
# data frame with **30 observations on 2 variables.**
#VARIABLES
# [,1]	weight   - numeric	-- dried weight of plants
# [,2]	group	- factor	-- with levels 'Ctrl','trt1','trt2'

#loading data
France <- read.spss("ESS7FR.sav", use.value.labels = T, to.data.frame = T) 
dim(France)
names(badges) <- c("transID", "userID", "badge", "date")

badges[["userID"]] <- as.factor(badges[["userID"]])
# number of rows
nrow(PlantGrowth)
#inside into attributes of DS.
str(PlantGrowth)
#Load Libraries
library(MASS)
library(gplots)
library(ggplot2)
library(dplyr)
#Carry out one way Anova test with 1 Independent variable group with 3 factors and 1 dependent variable weight

one_way_anova <- aov(weight~group,PlantGrowth)
#Summary - ANOVA table
summary(one_way_anova)

# In this instance we see that there is a significant effect of treatment group on dried weight of plantswith 95% CI . 
# However, there are 3 treatments. We would like to know which of these treatments has significant effect
# on dried weight of plant 
# We need a post-hoc test. R provides a simple function to carry out the Tukey HSD test.
TukeyHSD(one_way_anova)

# The table/output shows us the difference between pairs, 
# the 95% confidence interval(s) and the p-value of the pairwise comparisons.

#Data Visualization 
#plot means of weight agianst groups with differnt Treatment groups
plotmeans(PlantGrowth$weight~PlantGrowth$group,xlab="Group",
          ylab="weight", main="Visualize group variance")

#Box plot distribution: looking at outliers
p <- ggplot(PlantGrowth, aes(x = group, y = weight, fill = group))  
p +  geom_boxplot() +
  ggtitle("PlantGrowthData")


# Plotting 95% confidence intervals
aov.out <-aov(data=d, y~x)
anova(aov.out)
testres<-TukeyHSD(aov.out)
tky = as.data.frame(TukeyHSD(aov.out)$x)

# Plotting the data
ggplot(tky, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), label=c("p<0.01","p<0.05","Non-Significant")))) 
+geom_hline(yintercept=0, lty="11", colour="grey30") 
+geom_errorbar(aes(tky$pair, ymin=lwr, ymax=upr), width=0.2) 
+geom_point(aes(tky$pair, diff)) 
+labs(colour="") 
+xlab("***") 
+ylab("***") 
+coord_flip()

# Saving visualization
png("1_survival_by_class.png", width=800, height=600)
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)

# Resetting
dev.off()

#Getting anova model
omega_sq<-function(aovm){sum_stats<-summary(aovm)[[1]] 
SSm<-sum_stats[["SumSq"]][1] 
SSr<-sum_stats[["SumSq"]][2]
DFm<-sum_stats[["Df"]][1] 
MSr<-sum_stats[["MeanSq"]][2] 
W2 <-(SSm-DFm*MSr)/(SSm+SSr+MSr) 
return(W2) 
}
omega_sq(anovamodel)