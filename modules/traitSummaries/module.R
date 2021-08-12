traitSummaries <- list(
  
  "ui" = fluidPage(
    bsCollapsePanel(title = "Advanced Options",
      flowLayout(
        pickerInput(
          "traitSummaryTrials",
          "Which trials do you want to use trait data from?",
          choices = NULL,
          multiple = T,
          options = list(`actions-box` = TRUE)
        ),
        textAreaInput("resultsPipe", "R pipe for analysis (optional)", width = 400, height = 34)
      )
    ),
    flowLayout(
      pickerInput(
        "trialsOfInterest",
        "What trial are you rating?",
        choices = cache$observations$studyName %>% unique,
        multiple = T,
        options = list(`actions-box` = TRUE)
      ),
      fluidRow(
        pickerInput(
          "clonesOfInterest",
          "What plot(s) are you rating?",
          choices = NULL,
          multiple = T,
          options = list(`actions-box` = TRUE)
        ),
        actionButton(
          "clonesOfInterestPrevious", 
          label = HTML("<i class='glyphicon glyphicon-arrow-left'></i>")
        ),
        actionButton(
          "clonesOfInterestNext", 
          label = HTML("<i class='glyphicon glyphicon-arrow-right'></i>")
        ),
      ),
      selectizeInput(
        "traitSummaryTraits",
        "View what traits?",
        choices = NULL,
        multiple = T,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        "traitSummaryPlotTrait",
        "Plot which summary trait?",
        choices = NULL
      )
    ),
    tableOutput("resultsTable"),
    plotlyOutput("resultsPlot")
  ),
  
  "server" = function(input, output, session){
  
    observeEvent(input$trialsOfInterest, {
      germplasmChoices <<- cache$observations %>%
            filter(studyName %in% input$trialsOfInterest) %$%
            germplasmName
      updatePickerInput(
        session = session, 
        inputId = "clonesOfInterest",
        choices = germplasmChoices,
        selected = germplasmChoices
      )
    })
      
    observeEvent(input$clonesOfInterest, {
      trialChoices <<- cache$observations %>%
            filter(germplasmName %in% input$clonesOfInterest) %$%
            unique(studyName)
      updatePickerInput(
        session = session,
        inputId = "traitSummaryTrials",
        choices = trialChoices,
        selected = trialChoices
      )
    })
    
    observeEvent(input$traitSummaryTrials, {
      traitChoices <<- cache$observations %>% 
            filter(
              germplasmName %in% input$clonesOfInterest &
              studyName %in% input$traitSummaryTrials
            ) %>%
            select_if(~!all(isMissing(.x))) %>%
            select(contains(cache$traitInfo$traitName)) %>% 
            names()
      updateSelectizeInput(
        session = session,
        inputId = "traitSummaryTraits",
        choices = traitChoices,
        selected = defaultTraits[defaultTraits %in% traitChoices]
      )
      updatePickerInput(
        session = session,
        inputId = "traitSummaryPlotTrait",
        choices = defaultTraits[defaultTraits %in% traitChoices]
      )
    })
    
    observeEvent(input$clonesOfInterestPrevious, {
      updatePickerInput(
        session = session, 
        inputId = "clonesOfInterest",
        selected = (which(input$clonesOfInterest[1] == germplasmChoices) - 1) %>% 
          wrappingModulus(length(germplasmChoices)) %>%
          germplasmChoices[.]
      )
    })
    
    observeEvent(input$clonesOfInterestNext, {
      updatePickerInput(
        session = session, 
        inputId = "clonesOfInterest",
        selected = (which(input$clonesOfInterest[1] == germplasmChoices) + 1) %>% 
          wrappingModulus(length(germplasmChoices)) %>%
          germplasmChoices[.]
      )
    })
    
    output$resultsTable <- renderTable(
      tabulateResults(input, cache, session),
      rownames = T
    )
    
    output$resultsPlot <- renderPlotly(
      plotResults(input, cache, session)
    )
    
  }
  
)
