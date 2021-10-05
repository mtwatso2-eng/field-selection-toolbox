tabulateResults <- function(input, cache, session){
  
  if(is.null(input$clonesOfInterest))
    return(NULL)
  
  thisTable <- cache$observations %>%
    mutate(
      `Content of total sugars in raw storage roots computing percent` = 
        `Content of fructose in raw storage roots computing percent` +
        `Content of glucose in raw storage roots computing percent` +
        `Content of sucrose in raw storage roots computing percent`,
      `Content of starch in raw storage roots measuring percentage` =
        `Content of starch in dry weight basis in raw storage roots measuring percentage` *
        `Storage root dry matter content computing percent` / 100,
      `Content of total carbohydrates in raw storage roots computing percent` = 
        `Content of total sugars in raw storage roots computing percent` +
        `Content of starch in raw storage roots measuring percentage`,
      `Percent of carbohydrates that are sugars` = 
        `Content of total sugars in raw storage roots computing percent` /
        `Content of total carbohydrates in raw storage roots computing percent` *
        100
    ) %>%
    bind_rows(., cache$diseaseData) %>%
    group_by(studyName) %>%
    mutate(across(
      contains(cache$traitNames) & where(is.numeric),
      function(x){(x / mean(x[germplasmName %in% checks], na.rm = T)) %>% ifelse(is.infinite(.), NA, .)},
      .names = "Normalized {.col}"
    )) %>%
    filter(GenericGermplasmName %in% input$clonesOfInterest) %>%
    group_by(GenericGermplasmName) %>%
    mutate(across(
      contains(cache$traitNames),
      function(x){sum(!is.na(x))},
      .names = "n {.col}"
    )) %>%
    mutate(
      across(
        contains(cache$traitNames) & where(is.numeric),
        ~ ifelse(all(is.na(.x)), NA, mean(.x, na.rm = T))
      ),
      # across(
      #   contains("percent") & !contains("Normalized") & !starts_with("n ") & where(is.numeric),
      #   ~ scales::label_percent(accuracy = 0.01)(.x / 100)
      # ),
      across(
        contains(cache$traitNames) & where(is.character),
        ~ .x %>% unique() %>% {.[!isMissing(.)]} %>% paste(collapse = "; ")
      )
    ) %>%
    slice(1) %>%
    ungroup() %>%
    left_join(., cache$parentages, by = "GenericGermplasmName") %>%
    left_join(., cache$storageData, by = "GenericGermplasmName") %>%
    mutate(
      across(
        contains(cache$traitNames) & where(is.numeric),
        ~ round(.x, 1)
      ),
    ) %>%
    select(GenericGermplasmName, matchSort(input$traitSummaryTraits, traitLists[[input$traitSummaryTraitList]]))
    
  # if(input$germplasmNamesColumnNames){
  #   thisTable %<>% 
  #     t() %>%
  #     as.data.frame(., row.names = rownames(.)) %>%
  #     setNames(object = ., nm = .[1,]) %>%
  #     {.[-1, intersect(germplasmChoices, names(.)), drop = F]}
  # }
  
  thisTable <- eval(parse(text = paste("thisTable", input$resultsPipe)))
  
  return(thisTable)

}
