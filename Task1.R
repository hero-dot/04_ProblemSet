install.packages("rvest")
require(rvest)
require(magrittr)
require(dplyr)

LinkedIn <- html("https://www.linkedin.com/jobs/search?keywords=Data+Analyst&locationId=de:0&orig=FCTD&start=0&count=25&trk=jobs_jserp_pagination_1", as_html = TRUE)
read_html()


pages = 10
gesamteDaten = NULL

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

LinkedIn <- html("https://www.linkedin.com/jobs/search?keywords=Data+Science&location=Deutschland&trk=jobs_jserp_search_button_execute&orig=JSERP&locationId=de%3A0")
read_html()

company <- LinkedIn %>%
  html_nodes(".jserp-results .company-name .company-name-text")%>%
  html_text()

location <- LinkedIn %>%
  html_nodes(".jserp-results .job-location span")%>%
  html_text()

dfDataScience <- data.frame(company, location)



#.company-name-text , .company-name-link, company-name-line, .jserp-results, job-listing
#(".jserp-results")%>%
#html_node("#content-outlet :nth-child(2) .job-listing:nth-child(1) .company-name .company-name-text")%>%