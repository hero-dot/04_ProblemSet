install.packages("rvest")
require(rvest)
require(magrittr)

# returns Data Frame out of one list
makeDF <- function(list)
{
  descrList <- NULL
  for (i in seq(1,length(list),2))
  {
    descrList <- rbind(descrList,list[i])
  }
  countList <- NULL
  for (i in seq(2,length(list),2))
  {
    countList <- rbind(countList,list[i])
  }
  # change from factor to numeric
  countList <- sapply(countList,function(x)as.character(x))
  
  df = data.frame(descrList,countList)
  return(df)
}

searches = c("Data+Analyst","Data+Science","Analytics","Business+Intelligence")
varNames = list("Data+Analyst" = "DataAnalyst","Data+Science" = "DataScience", "Analytics" = "Analytics","Business+Intelligence"="BusinessIntelligence")

for (search in searches) 
{
  url <-paste0("https://de.linkedin.com/jobs/search?keywords=",search,"&locationId=de:0&start=0&count=25&trk=jobs_jserp_pagination_1")
  
  htmlDoc <- read_html(url)
  
  company = NULL
  location = NULL
  business = NULL
  careerTier = NULL
  
  company <- htmlDoc %>%
    html_nodes(".C .facet-display-value")%>%
    html_text()
  if (length(company)==0) 
  {
    company <- htmlDoc %>%
      html_nodes(".C .facet-item-link span")%>%
      html_text()
  }
  assign(paste0("company",varNames[[search]]),makeDF(company)) 
  
  location <- htmlDoc %>%
    html_nodes(".GC .facet-item-link span")%>%
    html_text()
  if (length(location)==0) 
  {
    location <- htmlDoc %>%
      html_nodes(".GC .facet-display-value")%>%
      html_text()
  }
  assign(paste0("location",varNames[[search]]),makeDF(location))

  business <- htmlDoc %>%
    html_nodes(".I .facet-item-link span")%>%
    html_text()
  if (length(business)==0) 
  {
    business <- htmlDoc %>%
      html_nodes(".I .facet-display-value")%>%
      html_text()
  }
  assign(paste0("business",varNames[[search]]),makeDF(business))
  
  careerTier <- htmlDoc %>%
    html_nodes(".E .facet-item-link span")%>%
    html_text()
  if (length(careerTier)==0) 
  {
    careerTier <- htmlDoc %>%
      html_nodes(".E .facet-display-value")%>%
      html_text()
  }
  assign(paste0("careerTier",varNames[[search]]),makeDF(careerTier))
}

# a. Visualize and discuss your results.
# Idee: Für jedes Dataframe eine Darstellung.
#       drei Darstellungen jeweils nebeneinander
#       Abschließend eine kurze Diskussion 

# b. Compare the different each terms -are the reuslts similar
# Eigentilch können wir die b in die 


# c. What information is lost? Is it a problem, depending on the use case? 
