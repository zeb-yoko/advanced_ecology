####BIOL 850 Assignment 1####
##Load libraries##
library(tidyverse)
##Load data##
data <- read.csv("waterfowl.csv")
str(data)
##select strata 45##
s45 <- filter(data$STRATUM == "45")

data %>% 
	filter(STRATUM == "45") %>% 
	filter(SPECIES == "Mallard") %>% 
	top_n(1, POP)


head(data)
##line grraph of mallards##
mals <- ggplot(s45, aes()+
	geom_line()
mals

##boxplot of mallard by strata##

mallxstr <- ggplot(data, aes(x=data$, y =log(sto.1$B.T_Ratio), color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Log Stomata Ratio")+
	theme(axis.text.x = element_text(angle=90, vjust = .25))+
	annotate('text', x=1, y=1.25,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=1.25,label='B', col="#005979",size=7)+
	annotate('text', x=3, y=1.25,label='A', col="#005979",size=7)+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
stolg
