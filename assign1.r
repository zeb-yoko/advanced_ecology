####BIOL 850 Assignment 1####
##Load libraries##
library(tidyverse)
##Load data##
dat <- read.csv("waterfowl.csv")
str(dat)
##select strata 45##
s45 <- filter(dat$STRATUM == "45")

##filter for mallard population in 45##
dat.MALL<- dat %>% 
	filter(STRATUM == "45") %>% 
	filter(SPECIES == "Mallard") %>% 
	top_n(1, POP)

str(dat)
head(dat)

##FILTER FOR 93 STRATUM##
dat.93 <-dat %>% 
	filter(STRATUM == "45") %>% 
	filter(YEAR == "1993") %>% 
	top_n(1, POP)
dat.93

##Number of Ponds##
##In species???##
dat.pond <- dat %>% 
	filter(STRATUM == "45") %>% 
	filter(SPECIES == "Pond") %>% 
	summarize(avg = mean(POP))
dat.pond


##line grraph of mallards##
dat.graph <- dat %>% 
	filter(STRATUM == 45 | STRATUM == 46 & SPECIES == "Mallard")
dat.graph$STRATUM <-as.factor(dat.graph$STRATUM)

View(dat)


mals <- ggplot(dat.graph, aes(x=YEAR, y=POP, color = STRATUM)) +
						geom_point() + geom_smooth(method = 'lm')
mals

##boxplot of mallard by strata##

mallxstr <- ggplot(dat.graph, aes(x=STRATUM, y=POP, color = STRATUM))+
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4)
	#xlab("Source Region") + ylab("Log Stomata Ratio")+
	#scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
mallxstr

##Linear regression ponds to mallard##
?lm
##Subset original dataframe to create mallard df##
mallard <- subset(dat, dat$SPECIES == "Mallard")
##Use subset to create df of pond data##
pond <- subset(dat, dat$SPECIES == "Pond")
##Model formula##
model<- lm(mallard$POP~pond$POP) 
summary(model)

