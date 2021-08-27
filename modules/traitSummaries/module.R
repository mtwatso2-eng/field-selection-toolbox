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
        "Select clones by trial",
        choices = cache$trials$studyName %>% unique,
        multiple = T,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        "traitSummaryTraits",
        "Select summary trait(s)",
        choices = NULL,
        multiple = T,
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        "traitSummaryPlotTrait",
        "Select trait for plot",
        choices = NULL
      )
    ),
    flowLayout(
      bsCollapsePanel(title = "View selected clone(s)",
        selectizeInput(
          "clonesOfInterest",
          "",
          choices = NULL,
          multiple = T,
          options = list("line-height" = 1)
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
    DTOutput("resultsTable"),
    plotlyOutput("resultsPlot")
  ),
  
  "server" = function(input, output, session){
    
    withProgress(message = "Loading clones", {
      updateSelectizeInput(
        session,
        "clonesOfInterest",
        choices = cache$observations$germplasmName %>% unique() %>% sort()
      )
    })

    
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
    
    observeEvent(c(input$traitSummaryTraits, input$clonesOfInterest, input$traitSummaryPlotTrait), {
      
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
