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

## segmentation 
## voir https://developer.piwik.org/api-reference/reporting-api-segmentation pour les segmentations possibles

visitsSegmented <- function(idSite = 1,
                            period = "day",
                            date = "last365",
                            segment = "city==Marseille",
                            url = "http://stats.data.gouv.fr/index.php?module=API") {
  request <- GET(url = url, 
                 query = list(
                   method = "VisitsSummary.get",
                   idSite = idSite,
                   period = period,
                   date = date,
                   language = "fr",
                   segment = segment,
                   format = "json"))
  content(request) %>% 
    map_df(~ map_df(., ~ .), .id = "date") %>% 
    mutate(bounce_rate = stringr::str_replace(bounce_rate, "%", "") %>% as.double()) %>% 
    mutate(date = lubridate::ymd(date))
  
}


getPageUrls <- function(idSite = 1,
                        period = "day",
                        date = "last365",
                        url = "http://stats.data.gouv.fr/index.php?module=API") {
  request <- GET(url = url, 
                 query = list(
                   method = "Actions.getPageUrls",
                   idSite = idSite,
                   period = period,
                   date = date,
                   language = "fr",
                   format = "json"))
  content(request) %>% 
    map_df(~ map_df(., function(x) {
      data_frame(label = x$label,
                 nb_visits = x$nb_visits,
                 entry_nb_visits = ifelse(!is.null(x[["entry_nb_visits"]]),
                                           as.integer(x[["entry_nb_visits"]]),
                                           0),
                 exit_nb_visits = ifelse(!is.null(x[["exit_nb_visits"]]),
                                         as.integer(x[["exit_nb_visits"]]),
                                         0))
    }), .id = "date") %>% 
    mutate(date = lubridate::ymd(date))
}


## focus sur les téléchargements
## attention difficile d'avoir plus d'une centaine de jours d'un coup, sinon l'API plante

getDownloads <- function(idSite = 1,
                         period = "day",
                         date = "last100",
                         url = "http://stats.data.gouv.fr/index.php?module=API") {
  request <- GET(url = url, 
                 query = list(
                   method = "Actions.getDownloads",
                   idSite = idSite,
                   period = period,
                   date = date,
                   expanded = 1,
                   flat = 1,
                   format = "json"))
  content(request) %>% 
    map_df(~ map_df(., function(x) {
      data_frame(label = x$label,
                 nb_visits = x$nb_visits,
                 entry_nb_visits = ifelse(!is.null(x[["entry_nb_visits"]]),
                                          as.integer(x[["entry_nb_visits"]]),
                                          0),
                 exit_nb_visits = ifelse(!is.null(x[["exit_nb_visits"]]),
                                         as.integer(x[["exit_nb_visits"]]),
                                         0))
    }), .id = "date") %>% 
    mutate(date = lubridate::ymd(date))
}
