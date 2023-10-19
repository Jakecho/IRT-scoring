ui <- fluidPage(
  tags$head(
    tags$style(HTML("
     /* Dark mode for sidebar */
    .sidebar {background-color: #333;color: #FFF;}
    /* White background for main panel */
    .main-panel {background-color: #FFF;}
    /* Green, blue, and red tabs */
    .nav-tabs > li:nth-child(1) > a {background-color: green;color: white;}
    .nav-tabs > li:nth-child(2) > a {background-color: blue;color: white;}
    .nav-tabs > li:nth-child(3) > a {background-color: red;color: white;}
    "))
  ),

  # Title
  titlePanel("IRT Scoring Method Exploration"),

  ##### Layout with left control panel and right main pane   #####
  sidebarLayout(
    sidebarPanel(
      #### Sidebar panel ####
      # Import item parameters and response data
      fluidRow(
        column(6, fileInput("itemParams", HTML("Upload Item Parameters <br> (a, b, c in CSV)"))),
        column(6, fileInput("responseData", HTML("Upload Response Data <br> (PID + scored response in 1/0)")))
      ),

      # generate sample data
      div(actionButton("showModal", "Generate Sample Data", 
                       style = "background-color: green; color: white; font-weight: bold;margin-bottom: 20px;")),

      # IRT models and scoring method
      fluidRow(
        column(6, selectInput("irtModel", "IRT Model:", c("Rasch", "2PL", "3PL"))),
        column(6, selectInput("scoringMethod", "Scoring Method:", c("MLE", "EAP", "MAP")))
      ),

      # Number of quadrature points and theta range
      fluidRow(
        column(6, numericInput("quadPoints", "Number of Quadrature Points:", value = 49)),
        column(6, numericInput("thetaInterval", "Theta Interval:", value = 0.1))
      ),
      fluidRow(
        # Theta Range on the left
        column(6, sliderInput("thetaRange", HTML("&theta; Range:"), min = -5, max = 5, value = c(-4, 4))),
        # SEM, TIF, Reliability on the right, each in its own row
        column(6, fluidRow(column(12, tags$div(checkboxInput("computeSEM", "SEM"),
            style = "color: black; font-weight: bold;"))),
          fluidRow(column(12, tags$div(checkboxInput("computeTIF", "Test Information Function"),
            style = "color: black; font-weight: bold;"))),
          fluidRow(column(12, tags$div(checkboxInput("computeReliability", "Test Reliability"),
            style = "color: black; font-weight: bold;")))
        )
      ),
      # Prior mean and variance
      fluidRow(
        column(6, numericInput("priorMean", "Mean Prior for EAP and MAP:", value = 0)),
        column(6, numericInput("priorVariance", "Variance Prior for EAP and MAP:", value = 1))
      ),
      # Min, cutscore, and max in theta
      fluidRow(
        column(4, numericInput("minTheta", HTML("Min in &theta;:"), value = -4)),
        column(4, numericInput("cutTheta", HTML("Cut in &theta;:"), value = 1)),
        column(4, numericInput("maxTheta", HTML("Max in &theta;:"), value = 4))
      ),
      # Min, cutscore, and max in scale score
      fluidRow(
        column(4, numericInput("minScale", "Min in Scale Score:", value = 0)),
        column(4, numericInput("cutScale", "Cut in Scale Score:", value = 180)),
        column(4, numericInput("maxScale", "Max in Scale Score:", value = 300))
      ),
      # Existing fluidRow for the download button
      fluidRow(
        # Action button for computing scale coefficients
        column(4, align = "center", actionButton("computeScaleCoef", HTML("Compute Scale<br>Coefficients"),
            style = "background-color: green; color: white; font-weight: bold;")),
        # Action button for converting theta to scale scores
        column(4, align = "center", actionButton("convertTheta", HTML("Convert Theta<br>to Scale Scores"),
            style = "background-color: blue; color: white; font-weight: bold;")),
        # Center-aligned download button with green background and black bold font
        column(4, align = "center", downloadButton("downloadCSV", HTML("Download<br>Output as CSV"),
            style = "background-color: red; color: white; font-weight: bold;"))
      )
  ),

  ####  Main panel ####
  mainPanel(
    # Custom CSS to make the labels bold and larger
    tags$style(HTML(".nav-tabs > li > a {font-weight: bold; font-size: 18px; }")),
  
    # Tabs for Data Inspection, Result Output, and Visualization
    tabsetPanel(
      tabPanel(
        "Data Inspection",
        # Item parameters summary and histogram
        fluidRow(
          column(6, tableOutput("item_params_summary")),
          column(6, plotOutput("item_params_histogram"))
        ),
        dataTableOutput("item_params_table"),
  
        # Person data summary and histogram
        fluidRow(
          column(6, tableOutput("person_data_summary")),
          column(6, plotOutput("person_data_histogram"))
        ),
        dataTableOutput("response_data_table")
      ),
      tabPanel("Result Output"),
      tabPanel("Visualization")
    )
  )
))