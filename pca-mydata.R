setwd("C:/Users/zippy/OneDrive/NDSU/advanced_ecology")  #replace with your directory
# load your libraries (vegan and other useful stuff)
library (vegan)
library (tidyverse)

#load the community dataset we will use for the tutorial
data(varespec)

summary(varespec) #look at a summary of the data

head(varespec) # look at the first 6 rows of the data

PCA<- rda(varespec, scale = F)

barplot(as.vector(PCA$CA$eif)/sum(PCA$CA$eig))

##rda(variable, scale =T/F)
##T = scales down variable sif of differenc magnitudes

a <- read.csv("climate_summary.csv")
colnames(a)[1] = "Source"
#PCA climate variables
row.has.na <- apply(a, 1, function(x){any(is.na(x))}) #which rows have NA values
sum(row.has.na) #1 row (population) has NA values
a2<-a[!row.has.na,] #remove rows that have NA
#Verify columns labeled correctly
Region<-a2[,2]
Climate<-a2[,3:28]
PCA<-(rda(Climate, scale=T))
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig))
PCA		  

plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-2,2)) # biplot of axis 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points"), xlim=c(-2,2), ylim=c(-2,2))
	 