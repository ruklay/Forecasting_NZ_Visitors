library(shiny)

source("visitors.R")

shinyServer(function(input, output) {
  
  d <- reactiveValues(data = 1)
  
  v <- eventReactive(input$action, {
    visitors(as.numeric(input$Mt), as.numeric(input$years[1]), as.numeric(input$years[2]), input$d)
  })
  
  observeEvent(input$action, {
    d$data <- input$d
  })
  
  k <- eventReactive(input$predict, {
    k <- v()
  })
  
  output$outModel <-renderPlot({
    
    if(input$type=="De-seasonalization (Ct) vs Regression") {
      plotCtRegress(v(), d$data)
    } else if (input$type == "Actual vs Prediction") {
      plotActualPrediction(v())
    } else if (input$type == "Trend (Mt)") {
      plotMt(v())
    } else if (input$type == "Seasonality (Qt)") {
      plotQt(v())
    } else if (input$type == "Random Noise (Et)") {
      plotEt(v())
    } else if (input$type == "Actual Data (Yt)") {
      plotYt(v())
    }
    
    output$htmlError <-renderUI({ 
      e <- findError(v())
      
      Header <- paste("<b>Model Evaluation (Error values)</b>")
      MSE <- paste("MSE = ", formatC(e$MSE, format="f", big.mark=','))
      SDE <- paste("SDE = ", formatC(e$SDE, format="f", big.mark=','))
      MAPE <- paste("MAPE = ", formatC(e$MAPE, format="f", big.mark=','))
      HTML(paste(Header, MSE, SDE, MAPE, sep = '<br/>'))
    })
    
  })
  
  output$predictedYear = renderDataTable({
    t <- predictionYt(k(),input$predictedYear, d$data)
  },
  option=list(
    pageLength=12,
    paging=FALSE ,
    searching=FALSE 
  ),
  
  output$outPrediction <- renderPlot ({
    plotPrediction(predictionYt(k(),input$predictedYear, d$data), input$predictedYear)
  })
  )
  
  observeEvent(input$clear, {
    #hide("Out01")
  })
  
})