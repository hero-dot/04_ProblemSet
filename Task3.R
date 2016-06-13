library(rvest)
library(dplyr)
library(magrittr)

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
write.csv(finalData,file = "autoScoutData.csv")

# Variables selected
# mk - Hersteller
# fr - Erstzulassung
# ma - Kilometerstand
# md - Modelvariante
# env - Verbrauch und CO2 Ausstoß
# price_raw - Verkaufspreis
# ph - Leistung in PS
# ra - Rating des Verkäufers



# To delete, but maybe still useful
test <- Listings[1]
length(convertListingEntry(test))
zzz = test[1]
zzz = strsplit(zzz,",\\\"")[[1]]
zzz = zzz[-1]
zzz = sapply(zzz,function(x){strsplit(x,":")})
zzz = sapply(zzz,function(x){c(x[1],x[2])})
zzz = as.data.frame(zzz)
colnames(zzz) <- cleanFun2(as.character(unlist(zzz[1,])))
test3 <- cleanFun2(as.character(unlist(zzz[1,])))
zzz[2,][is.na(zzz[2,])] = zzz[1,][is.na(zzz[2,])]
#zzz[2,] = cleanFun2(as.character(unlist(zzz[2,])))
return(zzz[2,])

length(cleanFun2(as.character(unlist(zzz[1,]))))
test3
test
l
