# title: "Stern MSBA Stock Forecast Assignment"
# author: "Team 9 - Christian Endter, Li Yun Sim, Ueng Yu Cheng, Robin Collis"
# date: "September 2019"

# Note: 
# No attempt is made to identify and 'jump' non-trading days in the forecasts made herein and shown 
# in the chart and table (i.e. every day is assumed to be a trading day). The app looks for forecasting models saved as Rdata files once a stock is selected.
# Refer to Stock_Forecasting_G9_v01.RMD for the estimation of the models

# Ensure required packages are installed, then load them
packages_list <- c("shiny", "xts", "quantmod","tidyverse","tidyquant","plyr", "rowr", "dplyr","e1071",
                   "randomForest", "shinyjs")
new_packages <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

for(package in packages_list){
  library(package, character.only = TRUE)
}

# We put some functions into a separate file so as to have less clutter in this one
source("Utility Functions.R")

# Stocks shown in the app
stocks <- c("GE", "GOLD", "K","GM","IBM")

# UI / client-side
ui <- fluidPage(
  
   shinyjs::useShinyjs(),

   titlePanel("Forecasting of stock's Closing Price"),
   
   sidebarLayout(
     # The inputs
      sidebarPanel(
         selectInput("stocks", h4("Select Stock"), choices=stocks,selected = ""),
         dateRangeInput("dateRange",h4("Data range to display"), 
                        start="2019-03-02", end=Sys.Date(),
                        min="1990-01-01", max=Sys.Date()),
         htmlOutput("model_selection"),
         #selectInput("model", h4("Model to use"), choices=list("SVM"), selected="SVM"),
         sliderInput("days_to_forecast",
                     h4("Days to forecast"),
                     min = 0,
                     max = 5,
                     value = 0)
      ),

      # The plot
      mainPanel(
         plotOutput("stockPlot"),
         div(tableOutput("forecastValues"), style = "font-size:90%")
      )
   )
   
)


# Server logic
server <- function(input, output, session) {
  
   # Populate the models list, depending on which stock symbol has been selected
   output$model_selection <- renderUI({
     model_choices <- retrieve_models()
     model_choices <- model_choices[model_choices$Symbol == input$stocks, "Available models"]
     if (nrow(model_choices) == 0) {
       # Workaround as otherwise does not display anything if list is empty
       model_choices <- add_row(model_choices, `Available models` = "None Available")
       model_choices <- dplyr::rename(model_choices,"None Available" = `Available models`)
       disable("days_to_forecast")
     } else {
       enable("days_to_forecast")
       model_choices <- add_row(model_choices, `Available models` = "Select Model", .before=1)
     }
     # Display the list
     selectInput("model", h4("Model to use"), choices=model_choices, multiple=FALSE)
   })

   output$stockPlot <- renderPlot({ 
     
     # Get the period to be displayed
     date_from <- as.Date(input$dateRange[1])
     date_to <- as.Date(input$dateRange[2])
     
     # Enable the slider (will be disabled if no model is/was selected)
     enable("days_to_forecast")
     
     # Very crude error avoidance, give chart at least four days to work with / Need to see if we also need to check input consistency from widget?
     if (date_to - date_from <= 4) date_from <- date_to - 5
     period_display <- paste0(date_from,"/", date_to + input$days_to_forecast)
     period_prediction <- paste0(date_from-30,"/",date_to)
   
     # Get the data if object not in environment and can't be loaded from file (note - adds 30 days to the start for forecast)
     check_load_create(input$stocks, start=date_from-30, end=date_to) # TO DO change later to work with period
     
     # Symbol now exists in global environment, assign it to chart data (necessary to subset as may have more data beyond chosen end date)
     chart_data <- eval(as.name(input$stocks))[period_prediction]
     
     # Fix names (not strictly necessary)
     names(chart_data) <- c("Open","High","Low","Close","Volume","Adjusted")

     # Check if a model has been selected, if not, show the chart without a forecast
     # If a model has been selected, load the model, do the prediction and show chart with forecast
     if ((!is.null(input$model)) && (input$model != "None Available") && (input$model != "Select Model")){
       
       # Load the model, construct the prediction and forecast series 
       # (note: prepare_prediction_inputs function has all relevant data prep work)
       # filename must be: SYMBOL_TYPE_MODEL.Rdata
       # model object itself: TYPE_MODEL
       model_name <- paste0(input$model, "_MODEL")
       load(paste0(input$stocks,"_", model_name, ".Rdata"))
       prediction_data <- construct_forecast_series(eval(as.name(model_name)), chart_data, date_to+1, input$days_to_forecast)
     } else { 
       reset("days_to_forecast")
       disable("days_to_forecast")
     }

     # If we're forecasting, get the additional series and overlay them
     if (exists("prediction_data")) {
       
       # Read the the number of days to forecast and extend the chart
       chart_data <- extend_xts_range(chart_data,input$days_to_forecast)
       # Add the predicted values as column to data object (dates should align)
       chart_data$Predicted <- prediction_data$Predicted
       chart_data$Forecast <- prediction_data$Forecast

     }

     # Plot chart as appropriate
     if (exists("prediction_data")) {
       
       # Show the basic chart, then add the prediction and forecast
       print(chart_Series(as.xts(chart_data)[period_display],
                          name=paste0(input$stocks, " (with ", input$days_to_forecast, " day forecast)")))
       if (input$days_to_forecast >= 1) print(add_TA(as.xts(chart_data)$Forecast[period_display], on=1, col="green", type="b"))
       print(add_TA(as.xts(chart_data)$Predicted[period_display], on=1, col="black", type ="b"))
       
       # Show the actual values in the table under the chart
       show("forecastValues")
       output$forecastValues <- renderTable({
         x <- fortify.zoo(tail(chart_data, n=6))
         x$Index <- as.character(x$Index)
         colnames(x)[1] <- "Date"
         x$Volume <- format(x$Volume, big.mark=",", scientific=FALSE)
         if (input$days_to_forecast ==0) x$Forecast[nrow(x)] <- NA
         return(x)
                    }, digits = 2, width="98%")
     } else {
       
       hide("forecastValues")
       # Show the basic chart
       print(chart_Series(as.xts(chart_data)[period_display], minor.ticks=TRUE,
                          name=paste0(input$stocks, " (no forecast)")))
     }
     
   })
   

}

# Run the application 
shinyApp(ui = ui, server = server)






