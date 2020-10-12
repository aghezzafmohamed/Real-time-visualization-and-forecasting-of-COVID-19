library(shinydashboard)
library(plotly)
library(ggfortify)
library(forecast)
source("Covid19_stream.R")

df_Cases <- Scripting_Cases("https://services3.arcgis.com/hjUMsSJ87zgoicvl/arcgis/rest/services/Covid_19/FeatureServer/5/query?where=1%3D1&outFields=*&outSR=4326&f=json")

number <- c(tail(df_Cases$Cas_confirmés, 1), tail(df_Cases$Décédés , 1), tail(df_Cases$Negative_tests, 1), tail(df_Cases$Tested, 1), tail(df_Cases$Retablis , 1))

Case <- c("Cas confirmés ", "Décédés ", "Negative tests", "Tested", "Retablis")
matrix <- data.frame(Case, number)

numberOfRows <- nrow(df_Cases)
bound <- as.integer(numberOfRows *0.8)
train <- df_Cases[1:bound ,]
test <- df_Cases[(bound+1):numberOfRows ,]

train <- subset(train, select=c('Date','Cas_confirmés'))

training <- ts(train$Cas_confirmés, start = c(decimal_date(as.Date("2020-03-02"))), frequency = 365)
tbats_model <- tbats(training)
tbats_forecast <- forecast(tbats_model, h=length(test$Cas_confirmés))
df_tbats <- fortify(tbats_forecast, ts.connect = TRUE)
df_tbats[166,]["Fitted"] <- NA
df_tbats$Index <- date_decimal(df_tbats$Index)
df_tbats$Tbats <- rowSums(df_tbats[, c("Fitted","Point Forecast")], na.rm=T)

sarima_forecast <- auto.arima(training, trace=TRUE, test="kpss", ic="bic")
sarima_forecast <- forecast(sarima_forecast, h=length(test$Cas_confirmés))
df_sarima <- fortify(sarima_forecast, ts.connect = TRUE)
df_sarima$Index <- date_decimal(df_sarima$Index)
df_sarima[166,]["Fitted"] <- NA
df_sarima$Sarima <- rowSums(df_sarima[, c("Fitted","Point Forecast")], na.rm=T)


Date <- c(df_tbats$Index)
Sarima <- c(df_sarima["Sarima"])
Tbats <- c(df_tbats["Tbats"])
Actuel <- c(df_Cases["Cas_confirmés"][1:nrow(df_sarima["Sarima"]),])
df_data <- data.frame(Date, Sarima, Tbats, Actuel)

function(input, output) {
  output$Cas_confirmés <- renderValueBox({
    valueBox(
      tail(df_Cases$Cas_confirmés, 1), "Cases", icon = icon("virus"),
      color = "yellow"
    )
  })
  
  output$Décédés  <- renderValueBox({
    valueBox(
      tail(df_Cases$Décédés , 1), "Deaths", icon = icon("frown"),
      color = "red"
    )
  })
  
  output$Retablis <- renderValueBox({
    valueBox(
      tail(df_Cases$Retablis, 1), "Recovered", icon = icon("user"),
      color = "blue"
    )
  })
  output$mytable = DT::renderDataTable({
    DT::datatable(df_Cases, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  
  output$download <- downloadHandler(
    filename = function(){"Covid19_maroc.csv"}, 
    content = function(fname){
      write.csv(df_Cases, fname)
    }
  )
  
  
  output$plot_totale <- renderPlotly({
    ggplotly(ggplot(data=matrix, aes(x=Case, y=number, fill=Case)) +
               theme_minimal() +
               geom_bar(stat="identity") +
               scale_y_continuous(labels = suffix_formatter_0) + ylab("") + 
               xlab("") +
               theme(legend.position="none"))
  })

  
  
  currentCases <- reactive({
    if (input$selectedvariable == "Cases"){ selected <- df_Cases$Cas_confirmés} 
    else if (input$selectedvariable == "Deaths "){ selected <-df_Cases$Décédés  }
    else if (input$selectedvariable == "Negative tests"){ selected <- df_Cases$Negative_tests }
    else if (input$selectedvariable == "Tested"){ selected <- df_Cases$Tested }
    else if (input$selectedvariable == "Recovered"){ selected <- df_Cases$Retablis }
  })
  output$plot_cases <- renderPlotly({
    plot_ly(data =  df_Cases, x = as.Date(df_Cases$Date, format = "%d/%m/%Y"),  y = currentCases(), type = "scatter", mode = 'lines')
  })
  
  
  output$plot_forecasting <- renderPlotly({ p()
  })
  
  p <- eventReactive(input$Plot, { plot_ly(data =  df_data, x = as.Date(df_data$Date, format = "%d/%m/%Y"),  y = df_data$Sarima, name = 'Sarima', type = "scatter", mode = 'lines') %>% add_trace(y = df_data$Tbats, name = 'Tbats', mode = 'lines') %>% add_trace(y = df_data$Cas_confirmés, name = 'actuel', mode = 'lines') 
})
}