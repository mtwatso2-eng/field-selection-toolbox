# Import packages
require(shiny); require(shinyBS); require(shinyWidgets)
require(magrittr); require(tidyverse);
require(DT); require(plotly); require(rhandsontable)
require(heatmaply); require(shinyHeatmaply)
require(brapir)

# Import functions defined in the app
source("utils.R")

spbase <<- brapi_db()$sweetpotatobase 

# cache <<- list()
# 
# cache$traitInfo <- brapi_get_traits(spbase, pageSize = 9999999) %>%
#   unclass() %>% as.data.frame()

# cache$observations <- brapi_get_studies(spbase, pageSize = 9999999) %>%
#   unclass() %>% as.data.frame() %>% {.[-1,]} %>%
#   filter(programName == "NCSU") %$%
#   brapi_post_search_observationtables(spbase, studyDbIds = studyDbId, pageSize = 9999999) %$%
#   brapi_get_search_observationtables_searchResultsDbId(spbase, Accept = "text/csv", searchResultsDbId, pageSize = 9999999) %>%
#   unclass() %>% as.data.frame() %>% {.[-1,]} %>%
#   mutate(across(where(isNumericLike), ~ as.numeric(.x))) %>%
#   arrange(desc(studyYear), studyName, plotNumber) %>%
#   rename_with(~ sapply(.x, observationVariablesToTraitName))

defaultTraits <- c(
  "Total storage root weight per NET plot in kg",
  "Weight of total US no. 1 storage roots measuring kg per plot",
  "Storage root skin predominant color estimating 1-12 NCSU",
  "Total vine production estimating number",
  "Storage root dry matter content computing percent|month 6|after harvest"
)



