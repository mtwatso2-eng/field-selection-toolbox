originalWarningOption <<- getOption("warn")
options(warn = -1)

source("global.R")
sapply(list.files(path = "modules", recursive = TRUE, pattern = "^.*\\.R$", full.names = TRUE), source)

ui <- navbarPage(id = "tabs", collapsible = TRUE, title = "Trait Toolbox",
  modulePanel("Field Trait Summaries", value = "traitSummaries"),
  modulePanel("Cross Predictor", value = "crossPredictor"),
  tags$head(tags$link(rel="shortcut icon", href="https://thumbs.dreamstime.com/b/sweet-potato-white-background-sweet-potato-batata-white-background-isolated-103677860.jpg"))
)
  
server <- function(input, output, session){

  sapply(list.files(path = "modules"), function(module){
    get(module)$server(input, output, session)
  })
  
}

shinyApp(ui = ui, server = server,  onStop(function(){options(warn = originalWarningOption)}))