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
	filter(SPECIES !="Pond") %>% 
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

##ggplot scatterplot for Q4 ##
mals <- ggplot(dat.graph, aes(x=YEAR, y=POP, color = STRATUM)) +
						geom_point() + geom_smooth(method = 'lm')
mals

##boxplot of mallard by strata##
##Q5##
mallxstr <- ggplot(dat.graph, aes(x=STRATUM, y=POP, color = STRATUM))+
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4)+
	xlab("Stratum") + ylab("Population")+
	scale_color_manual(values = c("Red", "Blue"))
mallxstr

##Linear regression: ponds to mallard##
##Subset original dataframe to create mallard df##
mallard <- subset(dat, dat$SPECIES == "Mallard")
##Use subset to create df of pond data##
pond <- subset(dat, dat$SPECIES == "Pond")
##Model formula##
model<- lm(mallard$POP~pond$POP) 
summary(model)


##Shannnon Diversity index (H)##
library(vegan)
##Per year in both strata##
##NOT WORKING, NEGATIVE VALUES?##
##REIMPORT##
dat<- read.csv("waterfowl (1).csv", header=T)
##SEPARATE BY STRATUM##
dat.45 <- dat %>% 
		filter(STRATUM == 45) %>%
		select(-AOU)

dat.46 <- dat %>% 
	filter(STRATUM == 46) %>% 
	select(-AOU)
##CALCULATE BY DIVERSITY##
H.45<- diversity(dat.45)
##ERROR:##
##> diversity(dat.45)##
##Error in diversity(dat.45) : input data must be non-negative##
> 
 ##Troubleshooting##
##DOUBLE/TRIPLE CHECK NO NEGATIVES##
dat.45$POP <- abs(dat.45$POP)
dat.45$YEAR<- abs(dat.45$YEAR)
dat.45$STRATUM<- abs(dat.45$STRATUM)
dat.45$STRATUM<- as.factor(dat.45$STRATUM)
## PEEK AT DATA##
str(dat.45)
##RENAME COLUMNS JUST IN CASE##
colnames(dat)[1] = "YEAR"
colnames(dat)[2] = "STRATUM"
colnames(dat)[3] = "SPECIES"
colnames(dat)[4] = "POP"
## PEEK AT DATA##
str(dat.45)

H<-diversity(dat.45, index = "shannon", MARGIN = 1)
#> H<-diversity(dat.45, index = "shannon", MARGIN = 1)
#Error in diversity(dat.45, index = "shannon", MARGIN = 1) : 
#  input data must be non-negative

##AGAIN##
##reload
dat<- read.csv("waterfowl.csv")
dat$POP <- abs(dat$POP)
dat$YEAR<- abs(dat$YEAR)
dat$STRATUM<- abs(dat$STRATUM)
dat$STRATUM<- as.factor(dat$STRATUM)
str(dat)
colnames(dat)[1] = "YEAR"
colnames(dat)[2] = "STRATUM"
colnames(dat)[3] = "AOU"
colnames(dat)[4] = "SPECIES"
colnames(dat)[5] = "POP"
str(dat)
H<-diversity(dat, index = "shannon", MARGIN = 1)
head(dat)
##try some dplyr tools##
##separate out species to indv columns##
specs <-	dat %>% 
	spread(SPECIES, POP, fill = 0)
str(specs)
##diversity takes matrix, not df##
h<-matrix(unlist(specs))
##makes it a 1 row matrix##
h
##select rows until it works/stops working##
h.45<- dat %>%
	filter(STRATUM == 45) %>% 
	select(POP,YEAR)

H.45<- diversity(h.45)
H.45


