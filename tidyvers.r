read_csv()
#loads as tbl_df; better?

vignette("tibble")

 dato <-read_csv("NV_CG_2017.csv")
View(dato) 

midwest_census <- read_csv("https://raw.githubusercontent.com/unolibraries/workshops/master/data-manipulation-r/midwest-census.csv")
midwest_census[is.na(midwest_census)] <- 0

#ctrl + shift + m#
%>% 
	
midwest_census %>% 
	select(year, state, totalPopulation)
midwest <-midwest_census %>% 
	select(-GISJOIN)

midwest_census %>% 
  filter(state == "Nebraska")

midwest_census %>% 
  filter(year > 1890)

midwest %>% 
  filter(totalPopulation >= 1000)
View(midwest)
btw.yr <- midwest %>% 
  filter(year >= 1820) %>% 
	filter(year<=1870)
##OR##
midwest_census %>%
  filter(year > 1820 & year < 1870)
midwest_census %>% 
	filter(year ==1820, state == "Kansas")
