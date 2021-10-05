# Import packages
require(shiny); require(shinyBS); require(shinyWidgets)
require(magrittr); require(tidyverse);
require(DT); require(plotly); require(rhandsontable)
require(heatmaply); require(shinyHeatmaply)
require(brapir)

httr::set_config(httr::config(ssl_verifypeer = 0L))

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
#   mutate(GenericGermplasmName = getGenericGermplasmName(germplasmName)) %>%
#   arrange(desc(studyYear), studyName, plotNumber)
# batch <- 50
# cache$parentages <- list()
# for(i in 1:(length(unique(cache$observations$germplasmDbId)) %/% batch)){
#   print(Sys.time())
#   print(paste((i*batch +1 - batch), i*batch))
#   try({
#     cache$parentages[[i]] <- cache$observations[(i*batch +1 - batch):(i*batch),] %$%
#       unique(germplasmDbId) %>%
#       brapi_post_search_germplasm(spbase, germplasmDbId = ., pageSize = 9999999) %$%
#       brapi_get_search_germplasm_searchResultsDbId(spbase, searchResultsDbId, pageSize = 9999999)
#   })
# }
# cache$parentages %<>%
#   reduce(full_join) %>%
#   unclass() %>% as.data.frame() %>%
#   rowwise() %>%
#   mutate(
#     Maternal = strsplit(pedigree, "/", fixed = T)[[1]][1],
#     Paternal = strsplit(pedigree, "/", fixed = T)[[1]][2],
#     GenericGermplasmName = getGenericGermplasmName(germplasmName)
#   ) %>%
#   group_by(GenericGermplasmName) %>%
#   slice(1) %>%
#   select(germplasmName, Maternal, Paternal, GenericGermplasmName)
# cache$parentages <- read.csv("parentages.csv", stringsAsFactors = F) %>%
  # rename(GermplasmName = Clone, Maternal = `Maternal_parent`, Paternal = `Nursery_paternal`) %>%
  # mutate(GenericGermplasmName = getGenericGermplasmName(GermplasmName))
# cache$storageData <- read.csv("storageData.csv", stringsAsFactors = F, check.names = F) %>%
#   group_by(GenericGermplasmName) %>%
#   slice(1)
# cache$diseaseData <- read.csv("diseaseData.csv", stringsAsFactors = F, check.names = F) %>%
#   mutate(across(!GermplasmName, ~as.numeric(.x))) %>%
#   mutate(GenericGermplasmName = getGenericGermplasmName(GermplasmName),) %>%
#   rowwise() %>%
#   mutate(`Streptomyces Soil Rot estimating 0-5` = mean(c(`GH SSR`, `Field SSR`), na.rm = T)) %>%
#   rename(
#     `Root Knot Nematode Meloidogyne estimating 0-5` = RKN,
#     `Reaction to Fusarium Wilt estimating 0-5` = FW,
#   )
# calculatedTraits <- c(
#   "Content of total sugars in raw storage roots computing percent",
#   "Content of starch in raw storage roots measuring percentage",
#   "Content of total carbohydrates in raw storage roots computing percent",
#   "Percent of carbohydrates that are sugars"
# )
# importedTraits <- c(
#   "Pithiness",
#   "n Pithiness"
# )
# cache$traitNames <- cache$observations %>%
#   names() %>%
#   {.[. %in% cache$traitInfo$traitName]} %>%
#   c(., paste("Normalized", .), paste("n", .)) %>%
#   c(., names(cache$parentages)) %>%
#   c(., calculatedTraits) %>%
#   c(., importedTraits)
# saveRDS(cache, "cache")

# for trait name synonyms: recommend removing this because synonyms unclear and are ambiguous for timeseries data
# names(cache$observations)[names(cache$observations) %in% cache$traitInfo$traitName] %<>% 
#   {cache$traitInfo[cache$traitInfo$traitName %in% ., "traitId"]} %>%
#   {cache$observationVariableInfo[cache$observationVariableInfo$trait.traitDbId %in% ., "synonyms"]} %>%
#   map_chr(., ~ strsplit(.x, ";", fixed = T)[[1]][1])
  
traitLists <<- list(
  "Summary Traits" = c(
    "Maternal",
    "Paternal",
    "Baked storage root flesh overall eye appeal estimating 0-9",
    "Baked storage root flesh overall likability estimating 0-9",
    "Storage root appearance estimating 1-9 NCSU",
    "Normalized Weight of total US no. 1 storage roots measuring kg per plot",
    "n Weight of total US no. 1 storage roots measuring kg per plot",
    "Pithiness",
    "n Pithiness",
    "Root Knot Nematode Meloidogyne estimating 0-5",
    "Reaction to Fusarium Wilt estimating 0-5",
    "n Reaction to Fusarium Wilt estimating 0-5",
    "Streptomyces Soil Rot estimating 0-5",
    "Storage root shape uniformity estimating 1-9 by NCSU",
    "Storage root skin predominant color estimating 1-12 NCSU",
    "Content of beta-carotene in dry weight basis in raw storage roots measuring mg per 100g",
    "Content of fructose in raw storage roots computing percent",
    "Content of glucose in raw storage roots computing percent",
    "Content of sucrose in raw storage roots computing percent",
    "Content of starch in raw storage roots measuring percentage",
    "Percent of carbohydrates that are sugars",
    "Content of asparagine in dry weight basis in storage roots measuring mg per g",
    "Storage root dry matter content computing percent"
  ),
  "Agronomic Traits" = c(
    "Normalized Weight of total US no. 1 storage roots measuring kg per plot",
    "n Weight of total US no. 1 storage roots measuring kg per plot",
    "Plant earliness estimating 1-3",
    "Adventitious buds depth estimating 1-9",
    "Length to diameter ratio computation",
    "Lenticels Number of the storage root estimating 1-9",
    "Pithiness",
    "n Pithiness",
    "Skin texture estimating 1-9",
    "Storage root appearance estimating 1-9 NCSU",
    "Storage root attachement estimating 0-9",
    "Storage root flesh color Anthocyanins estimating 0-4",
    "Storage root flesh color Carotenoids estimating 0-4",
    "Storage root shape uniformity estimating 1-9 by NCSU",
    "Storage root skin predominant color estimating 1-12 NCSU",
    "Reaction to Fusarium Wilt estimating 0-5",
    "Root Knot Nematode Meloidogyne estimating 0-5",
    "Streptomyces Soil Rot estimating 0-5"
  ),
  "Compositional Traits" = c(
    "Storage root dry matter content computing percent",
    "Storage root dry matter content computing percent",
  	"Content of fructose in raw storage roots computing percent",
    "Content of glucose in raw storage roots computing percent",
    "Content of starch in dry weight basis in raw storage roots measuring percentage",
    "Content of sucrose in raw storage roots computing percent",
    "Content of total sugars in raw storage roots computing percent",
    "Content of starch in raw storage roots measuring percentage",
    "Percent of carbohydrates that are sugars",
    "Content of amylose in dry weight basis in storage roots measuring mg per g",
    "Content of asparagine in dry weight basis in storage roots measuring mg per g",
    "Content of beta-carotene in dry weight basis in raw storage roots measuring mg per 100g",
    "Content of phenol in dry weight basis in storage roots measuring mg per g",
    "Content of total monomeric anthocyanin in dry weight basis in storage roots measuring mg per g"
  ),
  "Sensory Traits" = c(
    "Baked storage root flesh color intensity estimating 0-9",
    "Baked storage root flesh color uniformity estimating 0-9",
    "Baked storage root flesh discoloration estimating 0-9",
    "Baked storage root flesh lack of fiber estimating 0-9",	
    "Baked storage root flesh moistness estimating 0-9",	
    "Baked storage root flesh overall eye appeal estimating 0-9",	
    "Baked storage root flesh overall likability estimating 0-9",	
    "Baked storage root flesh smoothness estimating 0-9",	
    "Baked storage root flesh sweetness estimating 0-9",
    "Baked storage root flesh taste estimating 0-9",
    "Baked storage root predominant flesh color estimating NCSU",	
    "Baked storage root secondary flesh color estimating NCSU"
  )
)

checks <<- c(
  "Covington",
  "Beauregard",
  "Bonita",
  "Murasaki29",
  "NC413",
  "Japanese"
) %>%
  expand.grid(., c("", "_G2", "_G3", "_G4")) %>%
  apply(., 1, paste, collapse = "")

## OSP data
parents <<- read.csv("parents.csv", stringsAsFactors = F, check.names = F) %>%
  rowwise() %>%
  mutate(Code = toGenericPedigreeCode(Code)) %>%
  {.[!duplicated(.$Code),]} %>%
  {
    terminals <- union(.$Maternal, .$Paternal)
    rbind(
      .,
      data.frame(Code = terminals, Maternal = rep(NA, length(terminals)), Paternal = rep(NA, length(terminals)))
    )
  }

traits <<- read.csv("traits.csv", stringsAsFactors = F, check.names = F) %>%
  select(Clone, `Color Uniformity`, `Overall App.`, Vigor, Comments) %>%
  mutate(across(c(`Color Uniformity`, `Overall App.`, Vigor), ~ as.numeric(.x)))

# closeParents <- parents$Code %>%
#   map(., getCloseParents)
# saveRDS(closeParents, "closeParents")
closeParents <- readRDS("closeParents")
