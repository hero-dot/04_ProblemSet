install.packages("rvest")
require(rvest)
require(magrittr)
require(dplyr)
library(xml2)

LinkedIn <- read_html("https://www.linkedin.com/jobs/search?keywords=Data+Analyst&locationId=de:0&orig=FCTD&start=0&count=25&trk=jobs_jserp_pagination_1")
LinkedIn1 <- read_html("https://de.linkedin.com/jobs/search?keywords=Data+Science&locationId=de:0&start=0&count=25&trk=jobs_jserp_pagination_1")
LinkedIn2 <- read_html("https://de.linkedin.com/jobs/search?keywords=Analytics&locationId=de:0&start=0&count=25&trk=jobs_jserp_pagination_1")
LinkedIn3 <- read_html("https://de.linkedin.com/jobs/search?keywords=Business+Intelligence&locationId=de:0&start=0&count=25&trk=jobs_jserp_pagination_1")



pages = 44
gesamteDaten = NULL
company = NULL
location = NULL
location1 = NULL
careerTier2 = NULL

for (page in 1:pages){
  
  url <- paste0("https://www.linkedin.com/jobs/search?keywords=Data+Analyst&locationId=de:0&start=",(page-1)*50,"&count=50&trk=jobs_jserp_pagination_",page)
  
company <- LinkedIn %>%
  html_nodes(".jserp-results .company-name .company-name-text")%>%
  html_text()

location <- LinkedIn %>%
  html_nodes(".jserp-results .job-location span")%>%
  html_text()

dfDataAnalyst <- data.frame(company, location)

gesamteDaten = rbind(gesamteDaten,dfDataAnalyst)

}

show(gesamteDaten)




#LinedIn (Data Analyst)
company <- LinkedIn %>%
  html_nodes(".C .facet-item-link span")%>%
  html_text()

company

location <- LinkedIn %>%
  html_nodes(".GC .facet-item-link span")%>%
  html_text()

location

business <- LinkedIn %>%
  html_nodes(".I .facet-item-link span")%>%
  html_text()

business

careerTier <- LinkedIn %>%
  html_nodes(".E .facet-item-link span")%>%
  html_text()

careerTier

#LinkedIn1 (Data Science)

company1 <- LinkedIn1 %>%
  html_nodes(".C .facet-item-link span")%>%
  html_text()

company1

location1 <- LinkedIn1 %>%
  html_nodes(".GC .facet-item-link span")%>%
  html_text()

location1

business1 <- LinkedIn1 %>%
  html_nodes(".I .facet-item-link span")%>%
  html_text()

business1

careerTier1 <- LinkedIn1 %>%
  html_nodes(".E .facet-item-link span")%>%
  html_text()

careerTier1



#LinkedIn2 (Analytics)

company2 <- LinkedIn2 %>%
  html_nodes(".C .facet-item-link span")%>%
  html_text()

company2

location2 <- LinkedIn2 %>%
  html_nodes(".GC .facet-item-link span")%>%
  html_text()

location2

business2 <- LinkedIn2 %>%
  html_nodes(".I .facet-item-link span")%>%
  html_text()

business2

careerTier2 <- LinkedIn2 %>%
  html_nodes(".E .facet-item-link span")%>%
  html_text()

careerTier2




#LinkedIn3 (Business Intelligence)

company3<- LinkedIn %>%
  html_nodes(".C .facet-item-link span")%>%
  html_text()

company3

location3 <- LinkedIn3 %>%
  html_nodes(".GC .facet-item-link span")%>%
  html_text()

location3

business3 <- LinkedIn3 %>%
  html_nodes(".I .facet-item-link span")%>%
  html_text()

business3

careerTier3 <- LinkedIn3 %>%
  html_nodes(".E .facet-item-link span")%>%
  html_text()

careerTier3

as.data.frame(t(company))
as.data.frame(location)
as.data.frame(business)
as.data.frame(t(careerTier))

as.data.frame(company[c(1,3,5,7,9,11,13,15,17,19)])->dfCompanyA
as.data.frame(company[c(2,4,6,8,10,12,14,16,20)]) ->dfCompanyB

dfCompanyA



tierlist <- NULL
for (i in seq(1,14,2)) 
{
  tierlist <- rbind(tierlist,careerTier2[i])
}
tieramount <- NULL
for (i in seq(2,14,2)) 
{
  tieramount <- rbind(tieramount,careerTier2[i])
}

dfTier = data.frame(tierlist,tieramount)
dfTier


businesslist <- NULL
for (i in seq(1,20,2))
{
  businesslist <- rbind(businesslist,business2[i])
}

businessamount <- NULL
for (i in seq(2,20,2))
{
  businessamount <- rbind(businessamount,business2[i])
}
dfBusiness = data.frame(businesslist,businessamount)

# a. Visualize and discuss your results.
# Idee: Für jedes Dataframe eine Darstellung.
#       drei Darstellungen jeweils nebeneinander
#       Abschließend eine kurze Diskussion 

# b. Compare the different each terms -are the reuslts similar
# Eigentilch können wir die b in die 


# c. What information is lost? Is it a problem, depending on the use case? 
