traitSummaries <- list(
  
  "ui" = fluidPage(
    bsCollapsePanel(title = "Advanced Options",
      flowLayout(
        checkboxInput(
          "germplasmNamesColumnNames",
          "Use germplasm names as column names?"
        ),
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
        choices = cache$trials$studyName %>% unique,
        multiple = T,
        options = list(`actions-box` = TRUE)
      ),
      fluidRow(
        selectizeInput(
          "clonesOfInterest",
          "What clone(s) are you rating?",
          choices = cache$observations$germplasmName %>% unique() %>% sort(),
          multiple = T
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
      pickerInput(
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
    DTOutput("resultsTable"),
    plotlyOutput("resultsPlot")
  ),
  
  "server" = function(input, output, session){
  
    observeEvent(input$trialsOfInterest, {withProgress(message = "Loading trial layout data", {
      germplasmChoices <<- cache$trials %>%
        filter(studyName %in% input$trialsOfInterest) %$%
        map(
          studyDbId, 
          ~ try(brapi_get_studies_studyDbId_layouts(spbase, .x, pageSize = 9999999)$germplasmName)
        ) %>%
        unlist()
      trialPosition <<- 1
      updatePickerInput(
        session = session, 
        inputId = "clonesOfInterest",
        # choices = germplasmChoices,
        selected = germplasmChoices
      )
    })})
      
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
      updatePickerInput(
        session = session,
        inputId = "traitSummaryTraits",
        choices = cache$traitNames,
        selected = defaultTraits[defaultTraits %in% cache$traitNames]
      )
      updatePickerInput(
        session = session,
        inputId = "traitSummaryPlotTrait",
        choices = defaultTraits[defaultTraits %in% cache$traitNames]
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
    
    observeEvent(c(input$traitSummaryTraits, input$clonesOfInterest), {
      
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
