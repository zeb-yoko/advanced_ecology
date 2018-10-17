####BIOL 850 Assignment 1####
##Load libraries##
library(tidyverse)
library(vegan)
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
dat.MALL
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
############

##line graph of mallards##
dat.graph <- dat %>% 
	filter(STRATUM == 45 | STRATUM == 46 & SPECIES == "Mallard")
##Make STRATUM a factor instead of numeric##
dat.graph$STRATUM <- as.factor(dat.graph$STRATUM)
##ggplot scatterplot for Q4 ##
mals <- ggplot(dat.graph, aes(x=YEAR, y=POP, color = STRATUM)) +
						geom_point() + geom_smooth(method = 'lm') +
			scale_color_manual(values = c("red3", "royalblue3"))
mals
#############


##boxplot of mallard by strata##
##Q5##
mallxstr <- ggplot(dat.graph, aes(x=STRATUM, y=POP, color = STRATUM))+
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4)+
	xlab("Stratum") + ylab("Population")+
	scale_color_manual(values = c("red3", "royalblue3"))
mallxstr
##############

##Linear regression: ponds to mallard##
##Subset original dataframe to create mallard df##
mallard <- subset(dat, dat$SPECIES == "Mallard")
##Use subset to create df of pond data##
pond <- subset(dat, dat$SPECIES == "Pond")
##Model formula##
model<- lm(mallard$POP~pond$POP) 
summary(model)
##############

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
	##Troubleshooting##
##DOUBLE/TRIPLE CHECK NO NEGATIVES##
##absolute values##
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


######################################
###ignore section between lines 111-143###
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
##diversity takes matrix, not dataframe##
h<-matrix(unlist(specs))
##makes it a 1 row matrix##
h
##Species column makes it stop working, dont include##
h.45<- dat %>%
	filter(STRATUM == 45) %>% 
	select(POP,YEAR)

H.45<- diversity(h.45)
H.45
################
##end ignored section##
################################################################

##Made datatables in excel##
##pivot table to organize data like example data (BCI)##
##load in pivoted data##
#######Strata 45########
str.45<- read.csv("stratum45.csv")
str(str.45)
##fix column name##
colnames(str.45)[1]= "YEAR"
##Pond isn't a species, take out##
str.45<- str.45 %>% 
	select(-Pond)
##diversty index##
div.45 <-diversity(str.45)
div.45
##look at diversity data##
nrow(div.45)
##not a data frame##
##make a data frame##
div.45<-as.data.frame(div.45)
nrow(div.45)

##Install years in df##
##"for loop"##
count <- 1
for(i in 1:nrow(div.45)){
	div.45$YEAR[i] <- (1954 + count)
	count<- count + 1
	print(i)
	print(count)
}
str(div.45)
##plot it (45)##
shannon.45 <- ggplot(div.45, aes(x=YEAR, y=div.45)) +
						geom_point(col = "royalblue3") + 
					geom_smooth(method = 'lm', col = "red") +
						ylab("Stratum 45 diversity index")
shannon.45


#######Strata 46#######
##load in pivoted data##
str.46<- read.csv("stratum46.csv")
str(str.46)
##fix column name##
colnames(str.46)[1]= "YEAR"
##Pond isn't a species, take out##
str.46<- str.46 %>% 
	select(-Pond)
##diversty index##
div.46 <-diversity(str.46)
div.46
##look at diversity data##
nrow(div.46)
##not a data frame##
##make a data frame##
div.46<-as.data.frame(div.46)
nrow(div.46)

##Install years in df##
##"for loop"##
count <- 1
for(i in 1:nrow(div.46)){
	div.46$YEAR[i] <- (1954 + count)
	count<- count + 1
	print(i)
	print(count)
}
str(div.46)
##plot it (46)##
shannon.46 <- ggplot(div.46, aes(x=YEAR, y=div.46)) +
						geom_point(col="red") + 
	geom_smooth(method = 'lm', col = "royalblue") +
	ylab("Stratum 46 diversity index")
shannon.46

##Combined to one graph with one regression##
shannon.comparison <- ggplot(div.45, aes(x=YEAR, y=div.45)) +
	geom_point(col="red3")+					
	geom_point(x=div.46$YEAR, y = div.46$div.46, col = "royalblue3") + 
	geom_smooth(method = 'lm', col = "black") + 
	ylab("Diversity index for 45 & 46")
shannon.comparison

