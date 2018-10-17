#set your working directory  
setwd("your/working/directory/")  #replace with your directory

# load your libraries (vegan and other useful stuff)
library (vegan)
library (tidyverse)

#load the community dataset we will use for the tutorial
data(varespec)

summary(varespec) #look at a summary of the data

head(varespec) # look at the first 6 rows of the data

PCA <- rda(varespec, scale = FALSE)
# Use scale = TRUE if your variables are on different scales (e.g. for abiotic variables).
# Here, all species are measured on the same scale 
# So use scale = FALSE

# For visual purposes: plot a bar plot of relative eigenvalues. This is the percentage variance explained by each axis
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig)) 
# How much of the variance in our dataset is explained by the first principal component?
# Calculate the percent of variance explained by first two axes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%, this is ok.
# Also try to do it for the first three axes

# Now, we`ll plot our results with the baseR plot function
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")

sitePCA <- PCA$CA$u # Site scores
speciesPCA <- PCA$CA$v # Species scores

# In a biplot of a PCA, species' scores are drawn as arrows 
# that point in the direction of increasing values for that variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10)) # biplot of axis 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points"))

library(ggbiplot)

Example_nmds <- metaMDS(varespec, k=2)  
# k represents the number of dimensions
# note: the default dissimilarity index used is Bray-Curtis.  
#You could use others (see Appendix2)
# metaMDS(communitydata, distance = “bray”, k=2,)  
#distance uses the function vegdist

example_nmds=metaMDS(varespec,k=2, try = 30)
example_nmds 
stressplot(example_nmds)

ordiplot(example_nmds,type="none") # type= “none” suppresses points or text
orditorp(example_nmds, display = "species", col = "red", air = 0.01)
orditorp(example_nmds, display = "sites", cex = 1.1, air = 0.01) 

data(varechem)

# The function envfit will add the environmental variables as vectors to the ordination plot
ef <- envfit(example_nmds, varechem, permu = 999)
ef

# The two last columns are of interest: the squared correlation coefficient and the associated p-value

# Plot the vectors of the significant correlations and interpret the plot
plot(example_nmds, type = "t", display = "sites")
plot(ef, p.max = 0.05)

group = c(rep("Group1", 12), rep("Group2", 12))
# Create a vector of color values with same length as the vector of group values
colors = c(rep("red", 12), rep("blue", 12))

# Plot convex hulls with colors based on the group identity
ordiplot(example_nmds, type = "n")
for(i in unique(group)) {
  ordihull(example_nmds$point[grep(i, group),], draw="polygon",
           groups = group[group == i],col = colors[grep(i,group)],label=F) } 

orditorp(example_nmds, display = "species", col = "red", air = 0.01)
orditorp(example_nmds, display = "sites", col = c(rep("red",12),
                                           rep("blue", 12)), air = 0.01, cex = 1.25)
##challenge##
data(dune)
metaMDS( type=Jaccard)
ggplot(dune