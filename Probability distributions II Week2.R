#install 'Gapminder' package from Github 
#contains the life expectancy, GDP per capita, 
#and population by country, every five years, from 1952 to 2007
library(devtools)
install_github("jennybc/gapminder")
library(gapminder)
?gapminder# find more about the dataframe
data(gapminder) # loads data
head(gapminder)# returns first few lines of the data

