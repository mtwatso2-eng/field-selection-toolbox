tabulateResults <- function(input, cache, session){
  
  if(is.null(input$clonesOfInterest) | is.null(input$traitSummaryTrials))
    return(NULL)
  
  thisTable <- cache$observations %>% 
    filter(
      germplasmName %in% input$clonesOfInterest &
      studyName %in% input$traitSummaryTrials
    ) %>%
    group_by(germplasmName) %>%
    mutate(
      across(
        contains(cache$traitInfo$traitName) & where(is.numeric),
        ~ ifelse(all(is.na(.x)), NA, mean(.x, na.rm = T))
      ),
      across(
        contains(cache$traitInfo$traitName) & where(is.character),
        ~ sapply(.x, function(x){strsplit(x, ",", fixed = T)[[1]][1]}) %>% 
        unique() %>% 
        paste(collapse = "; ")
      )
    ) %>%
    slice(1) %>%
    ungroup() %>%
    select(germplasmName, input$traitSummaryTraits) %>%
    t() %>%
    as.data.frame(., row.names = rownames(.)) %>%
    setNames(object = ., nm = .[1,]) %>%
    {.[-1, intersect(germplasmChoices, names(.)), drop = F]}
  
  return(thisTable)

  
}
