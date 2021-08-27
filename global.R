# Import packages
require(shiny); require(shinyBS); require(shinyWidgets)
require(magrittr); require(tidyverse);
require(DT); require(plotly); require(rhandsontable)
require(heatmaply); require(shinyHeatmaply)
require(brapir)

# Import functions defined in the app
source("utils.R")

cache <<- readRDS("cache")

spbase <<- brapi_db()$sweetpotatobase

# cache <<- list()
# cache$traitInfo <- brapi_get_traits(spbase, pageSize = 9999999) %>%
#   unclass() %>% as.data.frame()
# cache$observationVariableInfo <- brapi_get_variables(spbase, pageSize = 9999999) %>%
#   unclass() %>% as.data.frame()
# cache$trials <- brapi_get_studies(spbase, pageSize = 9999999) %>%
#     unclass() %>% as.data.frame() %>% {.[-1,]} %>%
#     filter(
#       programName == "NCSU" &
#       !is.na(as.numeric(substr(studyName, 1, 1)))
#     ) %>%
#     arrange(desc(studyName))
# cache$observations <- cache$trials %$%
#   brapi_post_search_observationtables(spbase, studyDbIds = studyDbId, pageSize = 9999999) %$%
#   brapi_get_search_observationtables_searchResultsDbId(spbase, Accept = "text/csv", searchResultsDbId, pageSize = 9999999) %>%
#   unclass() %>% as.data.frame() %>% {.[-1,]} %>%
#   rename_with(~ sapply(.x, observationVariablesToTraitName)) %>%
#   mutate(across(
#     contains(cache$traitInfo$traitName) & where(is.character),
#     ~ sapply(.x, function(x){strsplit(x, ",", fixed = T)[[1]][1]})
#   )) %>%
#   mutate(across(
#     contains(cache$traitInfo$traitName) & where(isNumericLike), ~ as.numeric(.x)
#   )) %>%
#   arrange(desc(studyYear), studyName, plotNumber) %>%
# cache$traitNames <- cache$observations %>%
#   names() %>%
#   {.[. %in% cache$traitInfo$traitName]} %>%
#   c(., paste("Normalized", .))
# saveRDS(cache, "cache")

# for trait name synonyms: recommend removing this because synonyms unclear and are ambiguous for timeseries data
# names(cache$observations)[names(cache$observations) %in% cache$traitInfo$traitName] %<>% 
#   {cache$traitInfo[cache$traitInfo$traitName %in% ., "traitId"]} %>%
#   {cache$observationVariableInfo[cache$observationVariableInfo$trait.traitDbId %in% ., "synonyms"]} %>%
#   map_chr(., ~ strsplit(.x, ";", fixed = T)[[1]][1])
  
defaultTraits <<- c(
  "Baked storage root flesh overall likability estimating 0-9",
  "Storage root appearance estimating 1-9 NCSU",
  "Storage root skin predominant color estimating 1-12 NCSU",
  "Storage root dry matter content computing percent",
  "Normalized Total storage root weight per NET plot in kg",
  "Normalized Weight of total US no. 1 storage roots measuring kg per plot"
)

checks <<- c(
  "Covington",
  "Covington_G2",
  "Covington_G3",
  "Covington_G4",
  "Beauregard",
  "Beauregard_G2",
  "Beauregard_G3",
  "Beauregard_G4",
  "B94-0014_G2",
  "B94-0014_G3"
)



