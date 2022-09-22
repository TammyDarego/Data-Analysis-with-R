# Loading the libraries
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(corrplot)
library(tm)
library(wordcloud)

#Importing the dataset
chocolate_bars <- read.csv("../input/chocolate-bar-ratings/chocolate_bars.csv")

#Returns the structure of the dataset
str(chocolate_bars)

# Query to return any duplicate reviews
get_dupes(chocolate_bars)

#Returns a detailed summary of the dataset
summary(chocolate_bars)

#Filtering out missing values
chocolate_bars <- chocolate_bars %>% filter(num_ingredients != "NA")

#Plotting the Average rating of chocolate by origins
bean_origin_rating <- chocolate_bars %>% group_by(bean_origin) %>% summarise(Average_rating = mean(rating))

ggplot(data = bean_origin_rating, aes(x = bean_origin, y = Average_rating, fill = bean_origin)) + 
  geom_col(color = "black") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  xlab("Bean Origin") + ylab("Average Rating")

#Plotting the average rating of chocolate by manufacturers
company_location_rating <- chocolate_bars %>% group_by(company_location) %>% summarise(Average_rating = mean(rating))

ggplot(data = company_location_rating, aes(x = company_location, y = Average_rating, fill = company_location)) + 
  geom_col(color = "black") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  xlab("Manufacturer Location") + ylab("Average Rating")

#A wordcloud depicting the attributes of the chocolate
characteristics <- chocolate_bars$review
characteristics <- Corpus(VectorSource(characteristics))
characteristics <- characteristics %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace)

wordcloud(characteristics, min.freq = 1, random.order = FALSE, scale = c(3.5,0.2), colors=brewer.pal(8, "RdYlGn"))

#Plotting the average rating by year
year_reviewed_rating <- chocolate_bars %>% group_by(year_reviewed) %>% summarise(Average_rating = mean(rating))

ggplot(data = year_reviewed_rating, aes(x = year_reviewed, y = Average_rating)) + 
  geom_line(size = 2.5, color = "red") + 
  geom_point(size = 2.5) + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  xlab("Year") + ylab("Average Rating")