#####  Server code  ####

server <- function(input, output) {
  # Implementation of the functionality will go here
  observeEvent(input$showModal, {
    showModal(modalDialog(
      title = "Generate Sample Data",
      tags$head(tags$style(HTML("
        /* Previous styles remain the same */
        
        .aInputs .shiny-input-container {
          background-color: yellow;
        }
        
        .bInputs .shiny-input-container {
          background-color: orange;
        }
      
        .cInputs .shiny-input-container {
          background-color: lightgray;
        }
      
        .thetaInputs .shiny-input-container {
          background-color: AntiqueWhite;
        }
        .n .shiny-input-container {
          background-color: Aqua;
        }
        .N .shiny-input-container {
          background-color: Aqua;
        }
      "))),
      size = "m", # The size argument sets it to "medium", which is closest to 360px
      tags$style(".modal-dialog { width: 340px; }"),  
      
      # Input for item parameters
      # For item parameters a, b, c
      div(class = "aInputs", 
          numericInput("a_mean", "Lognormal Mean for 'a':", value = 0, width = '300px'),
          numericInput("a_variance", "Lognormal Variance for 'a':", value = 0.2, width = '300px')
      ),
      div(class = "bInputs", 
          numericInput("b_mean", "Normal Mean for 'b':", value = 0, width = '300px'),
          numericInput("b_variance", "Normal Variance for 'b':", value = 1.0, width = '300px')
      ),
      div(class = "cInputs", 
          numericInput("c_alpha", "Beta Alpha for 'c':", value = 5, width = '300px'),
          numericInput("c_beta", "Beta Beta for 'c':", value = 17, width = '300px')
      ),
      div(class ="n", style = "text-align: center;", 
          numericInput("Item_sample_size", "Sample Size for Items:", value = 10, width = '300px')
      ),
      
      # For person parameters theta
      div(class = "thetaInputs", 
          numericInput("theta_mean", HTML("Normal Mean for &theta;:"), value = 0, width = '300px'),
          numericInput("theta_variance", HTML("Normal Variance for &theta;:"), value = 1, width = '300px')
      ),
      
      div(class = "N", style = "text-align: center;", 
          numericInput("person_sample_size", "Sample Size for Persons:", value = 100, width = '300px')
      ),
      
      # Action button to generate data
      actionButton("generateData", "Generate", style = "font-weight: bold; background-color: green; color:white;", width = '300px'), 
      easyClose = TRUE, 
      footer = NULL
    ))
  })
  

  # Logic to generate data when 'Generate' is clicked in the modal
  observeEvent(input$generateData, {
    # Here, write the logic to generate item and person data based on the input values
    # Also compute all the values and metrics to be displayed on the main pane
  })


  # Read item parameters and response data
  item_params <- reactive({
    req(input$itemParams)
    read.csv(input$itemParams$datapath)
  })

  response_data <- reactive({
    req(input$responseData)
    read.csv(input$responseData$datapath)
  })

  # Compute descriptive summary for item parameters
  output$item_params_summary <- renderTable({
    summary <- sapply(item_params(), function(col) c(Mean = mean(col), Min = min(col), Max = max(col)))
    summary <- t(summary)
    rownames(summary) <- c("Discrimination", "Difficulty", "Guessing")
    round(summary, 2)
  })

  # Frequency graphs for item parameters
  output$item_params_histogram <- renderPlot({
    # Histogram for Discrimination
    hist(item_params()$a,
      main = "Frequency Distribution of Item Parameters", xlab = "Value",
      col = rgb(1, 0, 0, 0.5), breaks = 10, xlim = range(item_params()), freq = FALSE
    )

    # Add histogram for Difficulty
    hist(item_params()$b, add = TRUE, col = rgb(0, 1, 0, 0.5), breaks = 10, freq = FALSE)

    # Add histogram for Guessing
    hist(item_params()$c, add = TRUE, col = rgb(0, 0, 1, 0.5), breaks = 10, freq = FALSE)

    # Add legend
    legend("topleft",
      legend = c("Discrimination", "Difficulty", "Guessing"),
      fill = c(rgb(1, 0, 0, 0.5), rgb(0, 1, 0, 0.5), rgb(0, 0, 1, 0.5))
    )
  })

  # Compute item test score and person test score based on CTT
  ctt_summary <- reactive({
    # Compute item scores

    item_scores <- colSums(response_data()[, -1], na.rm = TRUE)

    # Compute Cronbach's alpha for reliability
    cronbach_alpha <- psych::alpha(response_data(), check.keys = TRUE)$total$raw_alpha

    # Compute person test scores
    person_scores <- rowSums(response_data(), na.rm = TRUE)

    # Summary statistics for person test scores
    person_summary <- c(
      min = min(person_scores), max = max(person_scores),
      mean = mean(person_scores), mode = as.numeric(names(sort(table(person_scores), decreasing = TRUE)[1])),
      sd = sd(person_scores)
    )

    list(item_scores = item_scores, cronbach_alpha = cronbach_alpha, person_summary = person_summary)
  })

  # Descriptive summary for item parameters
  output$item_params_summary <- renderTable({
    summary <- data.frame(
      a = c(Min = min(item_params()$a), Mean = mean(item_params()$a), Max = max(item_params()$a)),
      b = c(Min = min(item_params()$b), Mean = mean(item_params()$b), Max = max(item_params()$b)),
      c = c(Min = min(item_params()$c), Mean = mean(item_params()$c), Max = max(item_params()$c))
    )
    round(summary, 2)
  })

  # Descriptive summary for response data
  output$response_data_summary <- renderTable({
    summary <- ctt_summary()$person_summary
    summary <- round(summary, 2)
    summary
  })

  # Render item parameters datatable
  output$item_params_table <- renderDataTable({
    datatable(item_params())
  })

  # Render response data datatable
  output$response_data_table <- renderDataTable({
    datatable(response_data())
  })

  # Compute scale coefficients when button is clicked
  observeEvent(input$computeScaleCoef, {
    # Your logic to compute scale coefficients goes here
  })

  # Convert theta to scale scores when button is clicked
  observeEvent(input$convertTheta, {
    # Your logic to convert theta values to scale scores goes here
  })


  # Download handler for CSV export
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("output", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # You'll need to modify this line to save the actual output data
      write.csv(data, file) # Replace 'data' with the name of the data frame or object you want to save
    }
  )
}
