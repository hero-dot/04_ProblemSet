library(rvest)
library(xml2)
library(magrittr)
library(dplyr)
library(ggplot2)

# Helper Functions
# Format the Price from factor to numeric
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

# Try to read data from file
wineData <- try(read.csv("riesilingData.csv"))

# Scrape, if reading not possible
if (class(wineData) == "try-error")
{
  # Scrape the data 
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
  
  write.csv2(rawData,file = "riesilingData.csv",sep = ",",row.names = T,col.names = T)
  rawData = wineData
  rawData = NULL
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
#


reg <- lm(logRating~logPrice, data = rawData)
summary(reg)




# d. Multi regression model using backward selection
#

rawData%>%
  select(Country, Price, Rating, Vintage)%>%
  filter(!is.na(Price)) -> rawData1

reg1 <- lm(Rating~.,data = rawData1)
summary(reg1) #R^2 = 0.4027

reg2 <- lm(Rating~Price+Country, data = rawData1)
summary(reg2) #R^2 = 0.3998

reg3 <- lm(Rating~Price, data = rawData1)
summary(reg3) #R^2 = 0.3386