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
##Make STRATUM a factor instead of numeric##
dat.graph$STRATUM <- as.factor(dat.graph$STRATUM)

##ggplot scatterplot for #4 ##
mals <- ggplot(dat.graph, aes(x=YEAR, y=POP, color = STRATUM)) +
						geom_point() + geom_smooth(method = 'lm')
mals

##boxplot of mallard by strata##

mallxstr <- ggplot(dat.graph, aes(x=STRATUM, y=POP, color = STRATUM))+
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4)
	xlab("Stratum") + ylab("Population")
	#scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
mallxstr

##Linear regression ponds to mallard##
##Subset original dataframe to create mallard df##
mallard <- subset(dat, dat$SPECIES == "Mallard")
##Use subset to create df of pond data##
pond <- subset(dat, dat$SPECIES == "Pond")
##Model formula##
model<- lm(mallard$POP~pond$POP) 
summary(model)

library(vegan)

##Shannnon Diversity index (H)##
##Per year in both strata##
##Row import errors#
dat<- read.csv("waterfowl.csv", header=T)
dat<- read.table("waterfowl.csv", header = T, row.names = NULL)
dat.45 <- dat %>% 
		filter(STRATUM == 45) %>%
		select(-AOU)
dat.46 <- dat %>% 
	filter(STRATUM == 46) %>% 
	select(-AOU)
H.45<- diversity(dat.45, groups=SPECIES)

H.BCI<- diversity(BCI)
View(BCI)
diversity(dat.45)
##ERROR:##
##> diversity(dat.45)##
##Error in diversity(dat.45) : input data must be non-negative##
> 