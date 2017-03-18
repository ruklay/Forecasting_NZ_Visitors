library(shiny)

shinyUI(fluidPage(
  #useShinyjs(),
  
  titlePanel("NZ Visitors Model"),
  
  sidebarLayout(
    sidebarPanel(
      h2("1. Create a Model"),
      
      selectInput("Mt", 
                  label = "1) Select the Moving Average Month:",
                  choices = c(6,12,24),
                  selected = 6),
      
      sliderInput("years", 
                  label = "2) Select range of years to train:",
                  min = 1922, max = 2015, value = c(1922, 2015)),
      
      radioButtons("d", label = "3) Select Model Complexity",
                   choices = list("Simple Linear Regression" = 1, "Quadratic Regression" = 2),
                   selected = 1),
      
      actionButton("action", label = "Load Model"),
      
      #actionButton("clear", label = "Clear"),
      
      br(),br(),br(),
      
      h2("2. Model Components"),
      
      selectInput("type", 
                  label = "Select Model Components:",
                  choices = c("Actual Data (Yt)",
                              "Trend (Mt)",
                              "Seasonality (Qt)",
                              "Random Noise (Et)",
                              "De-seasonalization (Ct) vs Regression",
                              "Actual vs Prediction"),
                  selected = "Actual Data (Yt)"),
      
      br(),br(),br(),
      
      h2("3. Prediction"),
      
      selectInput("predictedYear", 
                  label = "Select the year to predict:",
                  choices = c(2040:2000),
                  selected = 2016),
      
      actionButton("predict", label = "Load Prediction")
      
    ),
    
    
    mainPanel(
      h1("Model Section", align="center"),
      plotOutput("outModel"),
      htmlOutput("htmlError"),
      HTML("<hr style='border-width: 10px;'><br>"),
      h1("Prediction Section", align="center"),
      dataTableOutput("predictedYear"),
      plotOutput("outPrediction")
    )
  )
))