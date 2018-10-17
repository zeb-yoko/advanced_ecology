##load in pivoted data##
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
str(div)
##plot it (45)##
shannon.45 <- ggplot(div.45, aes(x=YEAR, y=div.45)) +
						geom_point() + geom_smooth(method = 'lm')
shannon.45



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
str(div)
##plot it (46)##
shannon.46 <- ggplot(div.46, aes(x=YEAR, y=div.46)) +
						geom_point() + geom_smooth(method = 'lm')
shannon.46

