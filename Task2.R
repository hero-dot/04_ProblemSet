library(rvest)
library(xml2)
library(magrittr)
library(dplyr)
library(ggplot2)

rawData = NULL
vintages = seq(2012,2015,1)
countries = c("US","Germany","Austria","France","Australia","Canada")

for (vintage in vintages) 
{
  for (country in countries) 
  {
    for (page in seq(1,15,1))
    {
      url <- paste0("http://www.winemag.com/varietals/riesling/?s=&drink_type=wine&wine_type=White&varietal=Riesling&country=",country,"&vintage=",vintage,"&page=",page,"&sort_by=pub_date_web")
      content <- read_html(url)
      content %>% html_nodes(".review-item") %>% html_text -> tempData
      
      if (length(tempData) != 0) {tempData <- t(sapply(strsplit(tempData,"\n"),function (x) x))
      Titles <- removeWhitespace(tempData[,4])
      Rating <- sapply(strsplit(as.character(tempData[,7]),"Points"),function(x)as.numeric(x))
      Origin <- removeWhitespace(tempData[,8])
      Price  <- formatPrice(tempData[,9]) 
      Awards <- removeWhitespace(tempData[,10])
      Vintage <- rep(vintage,length(Price))
      Country <- rep(country,length(Price))
      tempData <- data.frame(Origin,Rating,Price,Titles,Awards,Vintage,Country)
      
      rawData = rbind(rawData,tempData)}
    }
  }
}

#Format the Price from factor to numeric
formatPrice <- function(Price)
  {
temp <- sapply(gsub("N/A","NA",Price),function(x)x)
temp <- sapply(gsub(" ","",temp),function(x)x)
temp <- sapply(gsub("[$]","",temp),function(x)x)
return(as.numeric(temp))
}

removeWhitespace <- function(List)
{
  temp <- sapply(gsub(" ","",List),function(x)x)
  return(temp)
}


# a. Viszalization of the price-rating relationship 
#    add the origin
#    add the vintage

rawData%>%
  select(Country, Price, Rating, Vintage)%>%
  filter(!is.na(Price))%>%
  group_by(Country)%>%
  ggplot(.,aes(Rating,Price))+
  geom_point(aes(colour = Country, stat="identity"))+
  facet_grid(~Vintage)->graph2
  
graph2

# b. Develop a simple linear regression model
#

reg <- lm(Rating~Price, data = rawData)
summary(reg)


# c. Variablentransformation
rawData%>%
  select(Country, Price, Rating, Vintage)%>%
  filter(!is.na(Price))%>%
  group_by(Country)%>%
 #mutate(Price = log(Price))
  mutate(Rating = (Rating)^0,5)#%>%
  #ggplot(.,aes(Rating,Price))+
  #geom_point(aes(colour = Country, stat="identity"))+
  #facet_grid(~Vintage)->graph2c

#graph2c

reg <- lm(Rating~Price, data = transformatedRawData)
summary(reg)

# Durch die logarithmische Transformation des Preises verbessert sich R^2 nur minimal
# von 0,3369 auf 0,3895. Die zusätzliche sqrt Transformation des Ratings würde R^2 immerhin
# auf 0,5 bringen, allerdings ist der Graph2c danach nur noch eine senkrechte Gerade.




# d. Multi regression model using backward selection
#
#
