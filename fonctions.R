## fonctions

library(httr)
library(purrr)
library(tidyverse)


## résumé des visites pour l'ensemble d'un site, sur une période donnée
visitsSummary <- function(idSite = 1, 
                          period = "day",
                          date = "last365",
                          url = "http://stats.data.gouv.fr/index.php?module=API") {
  request <- GET(url = url, 
                 query = list(
                   method = "VisitsSummary.get",
                   idSite = idSite,
                   period = period,
                   date = date,
                   language = "fr",
                   format = "json"))
  content(request) %>% 
    map_df(~ map_df(., ~ .), .id = "date") %>% 
    mutate(bounce_rate = stringr::str_replace(bounce_rate, "%", "") %>% as.double()) %>% 
    mutate(date = lubridate::ymd(date))
}
