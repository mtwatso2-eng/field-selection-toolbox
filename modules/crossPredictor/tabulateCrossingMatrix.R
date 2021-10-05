tabulateCrossingMatrix <- function(input, cache){
  
      validate(need(input$cloneList != "",  "No clone names or wrong format"))
  
      withProgress(message = "Loading data", {
        cloneList <- input$cloneList %>%
            {strsplit(., " ", fixed = T)[[1]]} %>%
            unique()
        if(input$heatmapTrait %in% c("Color Uniformity", "Overall App.", "Vigor"))
        {
          validate(need(any(cloneList %in% traits$Clone), "No trait data for clones"))
        }
        cloneTable <- cloneList %>%
          expand.grid(., ., stringsAsFactors = F) %>%
          rowwise() %>%
          mutate(
            # Cross = paste(Var1, Var2, sep = " x ", collapse = "")
            Relatedness = round(sharedCloseParentage(Var1, Var2), 2),
            `Color Uniformity` = traits %>% 
              {.[.$Clone %in% c(Var1, Var2), "Color Uniformity"]} %>% 
              {.[!is.na(.)]} %>%
              mean(),
            `Overall App.` = traits %>% 
              {.[.$Clone %in% c(Var1, Var2), "Overall App."]} %>% 
              {.[!is.na(.)]} %>%
              mean(),
            Vigor = traits %>% 
              {.[.$Clone %in% c(Var1, Var2), "Vigor"]} %>% 
              {.[!is.na(.)]} %>%
              mean()
          ) %>%
          {data.frame(matrix(.[[input$heatmapTrait]], nrow = length(cloneList), ncol = length(cloneList)))} %>%
          set_rownames(cloneList) %>%
          setNames(cloneList)
        
        datatable(
          cloneTable,
          callback = JS("
            table.on('mouseenter', 'td', function() {
              Shiny.onInputChange('crossingMatrixHoverIndexJS', this);
            });
            return table;
          ")
        ) %>%
          autoStyleInterval() %>%
          return()
  
      })
}