## checkbox 

library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Physician Prediction by Population"),
    
    sidebarPanel(
      h3("Inputs"),
      radioButtons('region', 'Region:',
                  c('All regions' = '0',
                    'North East' = '1',
                    'North Coast' = '2',
                    'South' = '3',
                    'West' = '4'), selected = '0'),
      numericInput('population', 'Total Population(per city):', 100000, min = 5000, max = 9999999, step = 1000),
      sliderInput(inputId = "conf_level",
                  label = "Confidence Level - alpha:",
                  min = 0.01,
                  max = 0.1,
                  value = 0.05,
                  step = 0.01),
      
      h3('Output options'),
      checkboxInput('linear_model','Linear Model Regression summary'),
      checkboxInput('anova','ANOVA Analysis'),
      checkboxInput('hypothesis', 'Hypothesis Testing'),
      submitButton('Submit')
    ),
    mainPanel(
      tabsetPanel(id ="theTabs",
        tabPanel("Description",
                 h3("Purpose"),
                 p("This prediction application is for predicting physician numbers in a city by given the city's population."),
                 h3("DataSet"),
                 p("The data set provides selected county demographic information (CDI) for 440 of the most populous counties in the United States."),
                 p("The information generally pertains to the years 1990 and 1992. There are 17 variables in the data set, I only used population and physicians to build this simple linear regression prediction application."),
                 h3("Inputs"),
                 tags$ul(
                   tags$li("Region - You can select region to predict based on whole dataset or use selected region data"), 
                   tags$li("Total Population - The prediction based on Total Population field, you can enter your value to predict the physician number"), 
                   tags$li("confidence level - Use this slider to set alpha value"),
                   tags$li("Output options - let you select more features to display")
                 ),
                 h3("Outputs"),
                 p("Right hand side main panel contains 3 analysis tab panels."),
                 tags$ol(
                   tags$li("Summary - Prediction result and Hypothesis testing result"), 
                   tags$li("DataSet Analysis - dataset exploratory analysis and linear regression model summary"), 
                   tags$li("Regression Analysis - Plots and ANOVA Table")
                 ),
                 h3("Action"),
                 p("After fill up the input form click on Submit button to see the result.")
                 ),
        
        tabPanel("Summary",
                 h3('Results of prediction'),
                 h4('The total population You entered'),
                 verbatimTextOutput(outputId = 'population'),
                 h4('Which resulted in a prediction of number of physicians '),
                 verbatimTextOutput("prediction"),
                 h4('The confidence interval given confidence level: '),
                 verbatimTextOutput("conf_interval"),
                 conditionalPanel(
                   condition = "input.hypothesis == true",
                   h4('Hypothesis Testing'),
                   verbatimTextOutput("test_desc"),
                   verbatimTextOutput("test_result")
                 ),
                 value = "summary"),
        
        tabPanel("DataSet Analysis",
                 plotOutput("x_hist"),
                 plotOutput("y_scatter"),
                 conditionalPanel(
                   condition = "input.linear_model == true",
                   h4('Linear Regression Model Summary'),
                   verbatimTextOutput("sse"),
                   verbatimTextOutput("coefficients"),
                   verbatimTextOutput("r_squared")
                 ),
                 value = "input"),
        
        tabPanel("Regression Analysis",
                 plotOutput('residual_plot'),
                 conditionalPanel(
                   condition = "input.anova == true",
                   h4('ANOVA Table'),
                   dataTableOutput('anova_table')
                 ),
                 value = "analysis")
        )
    )
  )
)