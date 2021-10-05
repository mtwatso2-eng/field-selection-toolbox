crossPredictor <- list(
  
  "ui" = fluidPage(
      flowLayout(
        textInput("cloneList", "Paste clones here"),
      selectInput(
        "heatmapTrait",
        "Select heatmap trait",
        choices = c("Color Uniformity", "Relatedness", "Overall App.", "Vigor"),
        selected = "Relatedness"
      )
    ),
    splitLayout(
      DTOutput("crossingMatrix"),
      DTOutput('hoverTable')
    )
  ),
  
  "server" = function(input, output, session){
    
    output$hoverTable <- renderDT({
      tabulateHoverTable(input, cache)
    })

    output$crossingMatrix <- renderDT({
        tabulateCrossingMatrix(input, cache)
      })
    
  }
  
)