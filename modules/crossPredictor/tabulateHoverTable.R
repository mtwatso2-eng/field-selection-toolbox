tabulateHoverTable <- function(input, cache){
  
      validate(need(input$cloneList != "",  "No clone names or wrong format"))
      
      cloneList <- input$cloneList %>%
            {strsplit(., " ", fixed = T)[[1]]} %>%
            unique()
      
      thisCross <- input$crossingMatrixHoverIndexJS %>%
        unlist() %>%
        {c(cloneList[.[1] + 1], cloneList[.[2]])}
      
      traits %>% 
        filter(Clone %in% thisCross) %>%
        t() %>%
        as.data.frame(., row.names = rownames(.)) %>%
        {setNames(object = ., .[1,])} %>%
        {.[-1,, drop = F]} %>%
        datatable()
  
}