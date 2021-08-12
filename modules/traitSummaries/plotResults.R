plotResults <- function(input, cache, session){
  
  input$traitSummaryPlotTrait
  
  if(is.null(cache$clonesOfInterest))
    return(NULL)
  
# if(input$whatSummaryStatistic == "Means"){
  
  thisPlot <- cache$traitSummaryTrials %>%
    filter(!isMissing(!!sym(input$traitSummaryPlotTrait))) %>%
    rowwise() %>%
    mutate(across(!!sym(input$traitSummaryPlotTrait), ~ as.numeric(strsplit(.x, ",")[[1]][1]))) %>%
    select(germplasmName, !!sym(input$traitSummaryPlotTrait)) %>%
    group_by(germplasmName) %>%
    summarise(across(!!sym(input$traitSummaryPlotTrait), ~mean(.x, na.rm = T))) %>%
    arrange(!!sym(input$traitSummaryPlotTrait)) %>%
    rowwise() %>%
    {
      ggplot(., aes(x = reorder(germplasmName, -!!sym(input$traitSummaryPlotTrait)), y = !!sym(input$traitSummaryPlotTrait))) +
        geom_bar(stat="identity") +
        ggtitle(paste(paste(collapse = ", "), input$traitSummaryPlotTrait, "by clone")) +
        xlab(xAxis) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    } %>%
    ggplotly()
  
  return(thisPlot)
  
# }

}
