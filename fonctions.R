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
                   expanded = 1,
                   flat = 1,
                   filter_limit = -1,
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
                   filter_limit = -1,
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

## liens sortants - dont téléchargements sur un autre site
## attention à ne pas requêter des périodes de temps trop importantes

getOutlinks <- function(idSite = 1,
                        period = "day",
                        date = "last30",
                        url = "http://stats.data.gouv.fr/index.php?module=API") {
  request <- GET(url = url, 
                 query = list(
                   method = "Actions.getOutlinks",
                   idSite = idSite,
                   period = period,
                   date = date,
                   expanded = 1,
                   flat = 1,
                   filter_limit = -1,
                   format = "json"))
  content(request) %>% 
    map_df(~ map_df(., function(x) {
      data_frame(label = x$label,
                 nb_visits = x$nb_visits,
                 exit_nb_visits = ifelse(!is.null(x[["exit_nb_visits"]]),
                                         as.integer(x[["exit_nb_visits"]]),
                                         0))
    }), .id = "date") %>% 
    mutate(date = lubridate::ymd(date))
}

## mots-clés de recherche

getSiteSearchKeywords <- function(idSite = 1,
                                  period = "day",
                                  date = "last30",
                                  url = "http://stats.data.gouv.fr/index.php?module=API") {
  request <- GET(url = url, 
                 query = list(
                   method = "Actions.getSiteSearchKeywords",
                   idSite = idSite,
                   period = period,
                   date = date,
                   filter_limit = -1,
                   format = "json"))
  content(request) %>% 
    map_df(~ map_df(., function(x) {
      data_frame(label = x$label,
                 nb_visits = x$nb_visits,
                 exit_nb_visits = ifelse(!is.null(x[["exit_nb_visits"]]),
                                         as.integer(x[["exit_nb_visits"]]),
                                         0),
                 nb_pages_per_search = x$nb_pages_per_search,
                 bounce_rate = x$bounce_rate,
                 exit_rate = x$exit_rate)
    }), .id = "date") %>% 
      mutate(bounce_rate = stringr::str_replace(bounce_rate, "%", "") %>% as.double()) %>%
      mutate(exit_rate = stringr::str_replace(exit_rate, "%", "") %>% as.double()) %>%
      mutate(date = lubridate::ymd(date))
}
  
  ## mots-clés de recherche (referrers)
  
getReferrersSearchKeywords <- function(idSite = 1,
                                    period = "day",
                                    date = "last30",
                                    url = "http://stats.data.gouv.fr/index.php?module=API") {
    request <- GET(url = url, 
                   query = list(
                     method = "Referrers.getKeywords",
                     idSite = idSite,
                     period = period,
                     date = date,
                     expanded = 1,
                     flat = 1,
                     filter_limit = -1,
                     format = "json"))
    content(request) %>% 
      map_df(~ map_df(., function(x) {
        data_frame(label = x$label,
                   nb_visits = x$nb_visits)
      }), .id = "date") %>% 
      mutate(date = lubridate::ymd(date))
}


## providers

getProvider <- function(idSite = 1,
                                       period = "day",
                                       date = "last30",
                                       url = "http://stats.data.gouv.fr/index.php?module=API") {
  request <- GET(url = url, 
                 query = list(
                   method = "Provider.getProvider",
                   idSite = idSite,
                   period = period,
                   date = date,
                   filter_limit = -1,
                   format = "json"))
  content(request) %>% 
    map_df(~ map_df(., function(x) {
      data_frame(label = x$label,
                 nb_uniq_visitors = x$nb_uniq_visitors,
                 nb_visits = x$nb_visits)
    }), .id = "date") %>% 
    mutate(date = lubridate::ymd(date))
}

