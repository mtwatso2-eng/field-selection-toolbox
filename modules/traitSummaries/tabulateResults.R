tabulateResults <- function(input, cache, session){
  
  if(is.null(input$traitSummaryTraits))
    return(NULL)
  
  thisTable <- cache$observations %>%
    filter(studyName %in% input$traitSummaryTrials) %>%
    group_by(studyName) %>%
    mutate(across(
      contains(cache$traitNames) & where(is.numeric),
      function(x){(x / mean(x[germplasmName %in% checks], na.rm = T)) %>% ifelse(is.infinite(.), NA, .)},
      .names = "Normalized {.col}"
    )) %>%
    filter(germplasmName %in% input$clonesOfInterest) %>%
    group_by(germplasmName) %>%
    mutate(
      across(
        contains(cache$traitNames) & where(is.numeric),
        ~ ifelse(all(is.na(.x)), NA, round(mean(.x, na.rm = T), 2))
      ),
      across(
        contains("percent") & !contains("Normalized") & where(is.numeric),
        ~ scales::label_percent()(.x / 100)
      ),
      across(
        contains(cache$traitNames) & where(is.character),
        ~ .x %>% unique() %>% {.[!isMissing(.)]} %>% paste(collapse = "; ")
      )
    ) %>%
    slice(1) %>%
    ungroup() %>%
    select(germplasmName, input$traitSummaryTraits)
    
  if(input$germplasmNamesColumnNames){
    thisTable %<>% 
      t() %>%
      as.data.frame(., row.names = rownames(.)) %>%
      setNames(object = ., nm = .[1,]) %>%
      {.[-1, intersect(germplasmChoices, names(.)), drop = F]}
  }
  
  thisTable <- eval(parse(text = paste("thisTable", input$resultsPipe)))
  
  return(thisTable)

}
