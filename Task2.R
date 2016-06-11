library(rvest)
library(xml2)
library(magrittr)
library(dplyr)

rawData = NULL
vintages = seq(2012,2015,1)
for (vintage in vintages) 
{
  for (page in seq(1,30,1))
  {
    url <- paste0("http://www.winemag.com/varietals/riesling/?s=&drink_type=wine&wine_type=White&varietal=Riesling&country=Germany,US,Austria,France,Australia,Canada&vintage=",vintage,"&page=",page,"&sort_by=pub_date_web")
    content <- read_html(url)
    content %>% html_nodes(".review-item") %>% html_text -> tempData
    
    if (length(tempData) != 0) {tempData <- t(sapply(strsplit(tempData,"\n"),function (x) x))
    Titles <- tempData[,4]
    Rating <- sapply(strsplit(as.character(tempData[,7]),"Points"),function(x)as.numeric(x))
    Origin <- tempData[,8]
    Price  <- formatPrice(tempData[,9]) 
    Awards <- tempData[,10]
    Vintage <- rep(vintage,length(Price))
    tempData <- data.frame(Origin,Rating,Price,Titles,Awards,Vintage)
    
    rawData = rbind(rawData,tempData)}
    
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

# a. Viszalization of the price-rating relationship 
#    add the origin
#    add the vintage

# b. Develop a simple linear regression model
#
# c. 
#
# d. Multi regression model usin backward selection
#
#
