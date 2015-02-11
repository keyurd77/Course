#install 'Gapminder' package from Github 
#contains the life expectancy, GDP per capita, 
#and population by country, every five years, from 1952 to 2007
library(devtools)
install_github("jennybc/gapminder")
library(gapminder)
?gapminder# find more about the dataframe
data(gapminder) # loads data
head(gapminder)# returns first few lines of the data
#Create a vector 'x' of the life expectancies of each 
#country for the year 1952. Plot a histogram of these 
#life expectancies to see the spread of the different countries.
View(gapminder) #see the table
p<- gapminder[gapminder$year<1953,c(1,4)] # created a vector 'p' extracted the column
#year with values less than 1953 for all rows (as I require only 1952) and combined 
#columns 1 (country) and 4 (lifeexpectancy)
p # see the extracted data
plot(p) # plot

