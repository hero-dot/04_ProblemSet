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
#
# Leider haben wir Probleme mit dem Scraping von LinkedIn, da es den CSS-Selektor nur manchmal akzeptiert.
# Dies liegt vermutlich an der beschränkten Zugriffsanzahl auf LinkedIn. Um die Daten darstellen zu können,
# müssten wir Faktor-Daten zunächst in Strings und dann in Numerische umwandeln.
# Anschließend haben wir 16 DataFrames (4 für jeden Suchbegriff). Diese würden wir jeweils in 
# einem Histogramm (Anzahl Treffer farblich abgegrenzt) darstellen. Mittels "grid.arrange" würden wir die 
# vier Schaubilder für jeden Suchbegriff nebeneinander und die Suchbegriffe zur Vergleichbarkeit untereinander
# anordnen.


# b. Compare the different each terms -are the reuslts similar
# 
# Da wir leider keine Daten bei der Darstellung mehr zur Verfügung hatten, können wir hierzu keine konkrete 
# Aussage treffen. Ähnlich bei allen Suchbegriffen bezüglich der Standorte lässt sich sagen, dass München 
# und Berlin zu den häufigsten Standorten zählen. Bei den häufigsten Unternehmen lässt sich keine wirklichere 
# Ähnlichkeit bei den verschiedenen Suchbegriffen feststellen. Die Branche betreffend stehen IT & Services und 
# das Internet an oberster Stelle. Für Berufserfahrung kann man feststellen, dass die Häufigkeit der gesuchten  
# Berufserfahrung bei allen vier Suchbegriffen nahezu identisch ist. 


# c. 
#
# Wenn man die summary statistic zum Scrapen der Daten verwendet, gehen einerseits zahlreiche Daten verloren, da 
# hierbei nur die häufigsten Ergebnisse gezogen werden. Auf der anderen Seite kann man bei dieser Methode keine
# Rückschlüsse über Beziehungen der Daten untereinander ziehen (z.B. man weiß nicht mehr welcher Standort zu welchem 
# Unternehmen gehört). Bezieht man die Daten von allen Unterseiten, ist das Ergebnis vollständig und man kann etwaige 
# Kardinalitäten abbilden.
