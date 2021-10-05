plotResults <- function(resultsTable, input, cache, session){
  
  if(is.null(resultsTable))
    return(NULL)
   
  thisPlot <- resultsTable %>%
    select(GenericGermplasmName, !!sym(input$traitSummaryPlotTrait)) %>%
    {
      ggplot(., aes(x = reorder(GenericGermplasmName, -!!sym(input$traitSummaryPlotTrait)), y = !!sym(input$traitSummaryPlotTrait))) +
        geom_bar(stat="identity") +
        ggtitle(paste(paste(collapse = ", "), input$traitSummaryPlotTrait, "by clone")) +
        xlab("GenericGermplasmName") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    } %>%
    ggplotly()

  return(thisPlot)

}
