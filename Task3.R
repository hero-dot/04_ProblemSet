library(rvest)
library(dplyr)
library(magrittr)
library(ggplot2)

cleanFun <- function(htmlString) {
  x = gsub("\\r","", htmlString)
  x = gsub("\\n","", x)
  x = gsub("\\{","",x)
  return(x)
}

cleanFun2 <- function(x) {
  x = gsub("\\\"","", x)
  return(x)
}

convertListingEntry = function(x)
{
  zzz = x[1]
  zzz = strsplit(zzz,",\\\"")[[1]]
  zzz = zzz[-1]
  zzz = sapply(zzz,function(x){strsplit(x,":")})
  zzz = sapply(zzz,function(x){c(x[1],x[2])})
  zzz = as.data.frame(zzz)
  colnames(zzz) <- cleanFun2(as.character(unlist(zzz[1,])))
  zzz[2,][is.na(zzz[2,])] = zzz[1,][is.na(zzz[2,])]
  #zzz[2,] = cleanFun2(as.character(unlist(zzz[2,])))
  yyy = zzz[2,]
  yyy%>%
   select(mk,fr,ma,md,env,price_raw,ph,ra,fl)-> yyy
  return(yyy)
}

finalData = NULL

pages= seq(1,30,1)

for (page in pages) 
{
  url = paste0("http://fahrzeuge.autoscout24.de/?atype=C&mmvmk0=74&mmvmd0=-101&mmvco=1&fregfrom=2010&cy=D&bcol=11&zipc=D&zipr=200&ustate=N,U&sort=threetier,price&results=80&page=","1","&dtr=s")
  
  url %>%
    read_html() %>%
    html_nodes("script") %>%
    .[12] %>%
    html_text(trim=T) %>%
    cleanFun() %>%
    strsplit(.,"\\|\\|") %>%
    .[[1]] %>%
    .[[1]] %>%
    strsplit(.,"ei\\\"") %>%
    .[[1]] %>%
    .[2:81]-> Listings 
  
  data = convertListingEntry(Listings[1])
  
  Listings = Listings[2:80]
  
  for (l in Listings)
  {
    help = convertListingEntry(l)
    data = full_join(data,help)
  }
  finalData = rbind(finalData,data)
  
  if ((nrow(finalData) > 2000)) {break}
  
}

# Save the data
write.csv(finalData,file = "autoScoutData.csv")

# Variables selected
#
# mk        - Hersteller
# fr        - Erstzulassung
# ma        - Kilometerstand
# md        - Modelvariante
# env       - Verbrauch und CO2 Ausstoß
# price_raw - Verkaufspreis
#        ph - Leistung in PS
#        ra - Rating des Verkäufers

autoData <- read.csv("autoScoutData.csv")

autoData%>%
  mutate(env=sapply(strsplit(as.character(.$env),"\""),function(x) x[2]))%>%
  mutate(fc = sapply(strsplit(.$env," "),function(x) x[1]))%>%
  mutate(fc = sapply(gsub(",",".",.$fc),function(x)as.numeric(x)))%>%
  mutate(co2km = sapply(strsplit(.$env," "),function(x) as.numeric(x[5])))%>%
  mutate(mk = sapply(.$mk,function(x)cleanFun2(as.character(x))))%>%
  mutate(fr = sapply(.$fr,function(x)cleanFun2(as.character(x))))%>%
  mutate(ma = sapply(.$ma,function(x)cleanFun2(as.character(x))))%>%
  mutate(ma = sapply(.$ma, function(x)gsub("\\.","",x)))%>%
  mutate(md = sapply(.$md,function(x)cleanFun2(as.character(x))))%>%
  mutate(ph = sapply(.$ph,function(x)cleanFun2(as.character(x))))%>%
  mutate(ph = sapply(.$ph,function(x)as.numeric(x)))%>%
  mutate(ma = sapply(.$ma,function(x)as.numeric(x)))%>%
  select(-env)->autoData

attributes(autoData$ma) <- NULL
attributes(autoData$fc) <- NULL
attributes(autoData$ph) <- NULL


# a. Visualize the main value components
autoData%>%
  ggplot(., aes(price_raw,ma,color=ph))+
  geom_point()+
  facet_grid(.~md)-> graph
graph

# b. Randomly choose 800 vehicles
#    Train different linear regression models 
#    Use These models to forecast the price

autoSample <-autoData[sample(nrow(autoData),800),]

regModel <- lm(price_raw~ma+ph+fc+co2km+as.factor(md),autoSample)
summary(regModel)
# R^2 = 0.1594

regModel1 <- lm(price_raw~ma+ph+fc+co2km,autoSample)
summary(regModel1)
# R^2 = 0.1505

regModel2 <- lm(price_raw~ma+ph+co2km,autoSample)
summary(regModel2)
# R^2 = 0.151

regModel3 <- lm(price_raw~ph+fc+co2km,autoSample)
summary(regModel3)
# R^2 = 0.1447

regModel4 <- lm(price_raw~ph+co2km,autoSample)
summary(regModel4)
# R^2 = 0.1453

# predict the price with predict(lm,df)
# use vehicles not used for training the model


# c. Report the mean squared error and compare this metric
#    with the models R^2 values

mse <- function(lm)
{
  return(mean(lm$residuals^2))
}

# Mean Squared error of regModel 
mse1 <- mse(regModel)
# R^2 = 0.1594

# Mean Squared error of regModel1 
mse2 <- mse(regModel1)
# R^2 = 0.1505

# Mean Squared error of regModel 
mse3 <- mse(regModel2)
# R^2 = 0.1447

# Mean Squared error of regModel 
mse4 <- mse(regModel3)
# R^2 = 0.1447

# d. List the cars the models had most problems to predict
#    What's the underlying problem

# Unser Model hat durchweg schlechte Werte für den R^2 und eignet sich damit generell nicht gut 
# für die Vorhersage. Mit besseren Daten könnten evt. auch bessere Modelle erstellen. 
