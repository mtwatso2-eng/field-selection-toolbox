traitSummaries <- list(
  
  "ui" = fluidPage(
    bsCollapsePanel(title = "Advanced Options",
      flowLayout(
        pickerInput(
          "traitSummaryTraits",
          "Select summary trait(s) manually",
          choices = cache$traitNames,
          multiple = T,
          options = list(`actions-box` = TRUE)
        ),
        textAreaInput("resultsPipe", "R pipe for analysis (optional)", width = 400, height = 34)
      )
    ),
    flowLayout(
      pickerInput(
        "trialsOfInterest",
        "Select clones by trial",
        choices = cache$trials$studyName %>% unique,
        multiple = T,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        "traitSummaryTraitList",
        "Select trait list",
        choices = c("Summary Traits", "Agronomic Traits", "Compositional Traits", "Sensory Traits"),
        selected = "Summary Traits",
        multiple = T
      ),
      pickerInput(
        "traitSummaryPlotTrait",
        "Select trait for plot",
        choices = NULL
      )
    ),
    flowLayout(
      bsCollapsePanel(title = "Select clones from list",
        selectizeInput(
          "clonesOfInterest",
          "",
          choices = NULL,
          multiple = T,
          options = list("line-height" = 1)
        ),
        textInput(
          "clonesOfInterestPasted",
          "Paste clones from spreadsheet"
        )
      ),
      fluidRow(
      actionButton(
        "clonesOfInterestPrevious",
        label = HTML("<i class='glyphicon glyphicon-arrow-left'></i>")
      ),
      actionButton(
        "clonesOfInterestNext",
        label = HTML("<i class='glyphicon glyphicon-arrow-right'></i>")
      )
    )
    ),
    tabsetPanel(
      tabPanel("Table", DTOutput("resultsTable")),
      tabPanel("Plot", plotlyOutput("resultsPlot"))
    )
  ),
  
  "server" = function(input, output, session){
    
    withProgress(message = "Loading clones", {
      updateSelectizeInput(
        session,
        "clonesOfInterest",
        choices = cache$observations$germplasmName %>% unique() %>% sort() %>% getGenericGermplasmName(),
        selected = NULL
      )
    })

    observeEvent(input$trialsOfInterest, {withProgress(message = "Loading trial layout data", {
      germplasmChoices <<- cache$trials %>%
        filter(studyName %in% input$trialsOfInterest) %$%
        map(
          studyDbId,
          ~ try(brapi_get_studies_studyDbId_layouts(spbase, .x, pageSize = 9999999)$germplasmName)
        ) %>%
        unlist() %>%
        getGenericGermplasmName()
      trialPosition <<- 1
      updateSelectizeInput(
        session = session, 
        inputId = "clonesOfInterest",
        selected = germplasmChoices
      )
    })})
    
    observeEvent(input$clonesOfInterestPasted, {
      pastedCloneList <- input$clonesOfInterestPasted %>%
        {strsplit(., " ", fixed = T)[[1]]} %>%
        unique() %>%
        getGenericGermplasmName() %>%
        {.[. %in% (cache$observations$germplasmName %>% unique() %>% sort() %>% getGenericGermplasmName())]}
      
      updateSelectizeInput(
        session = session,
        inputId = "clonesOfInterest",
        selected = pastedCloneList
      )
    })
    
    observeEvent(c(input$traitSummaryTraitList), {
      updatePickerInput(
        session = session,
        inputId = "traitSummaryTraits",
        selected = traitLists[input$traitSummaryTraitList] %>% unlist() %>% {.[. %in% cache$traitNames]} %>% unname()
      )
      updatePickerInput(
        session = session,
        inputId = "traitSummaryPlotTrait",
        choices = traitLists[input$traitSummaryTraitList] %>% unlist() %>% {.[. %in% cache$traitNames]} %>% unname()
      )
    })
    
    observeEvent(input$clonesOfInterestPrevious, {try({
      updateSelectizeInput(
        session = session,
        inputId = "clonesOfInterest",
        selected = (trialPosition - 1) %>%
          wrappingModulus(length(germplasmChoices)) %>%
          germplasmChoices[.]
      )
      trialPosition <<- trialPosition - 1
    })})

    observeEvent(input$clonesOfInterestNext, {try({
      updateSelectizeInput(
        session = session,
        inputId = "clonesOfInterest",
        selected = (trialPosition + 1) %>%
          wrappingModulus(length(germplasmChoices)) %>%
          germplasmChoices[.]
      )
      trialPosition <<- trialPosition + 1
    })})
    
    observeEvent(c(input$traitSummaryTraits, input$clonesOfInterest, input$clonesOfInterestPasted, input$traitSummaryPlotTrait), {

      resultsTable <- tabulateResults(input, cache, session)
      
      output$resultsTable <- renderDT(
        resultsTable,
        rownames = T,
        extensions = "Buttons",
        options = list(
          lengthMenu = list(c(10, 25, 50, 100, -1),
            c("10", "25", "50","100", "All")),
          dom = "Bfrtlip",
          buttons = list(
            list(
              extend = "csv",
              filename = paste("",
              "Summary",
              exportOptions = list(modifier = list(page = "all")))
            )
          )
        )
      )
      
      output$resultsPlot <- renderPlotly(
        plotResults(resultsTable, input, cache, session)
      )
      
    })
    
  }
  
)
