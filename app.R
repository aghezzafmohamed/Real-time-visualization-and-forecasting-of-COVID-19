library(ggfortify)
library(numberFormattR)
library(forecast)
library(httr)
library(jsonlite)
library(tibble)
library(lubridate)
library(shinydashboard)
library(plotly)


Scripting_Cases <- function (LocationId) {
  request <- paste0(LocationId)
  text <- content(GET(request), as = "text", encoding = "UTF-8")
  tryCatch(data <- fromJSON(text), error=function(e) NULL)
  df <- as_data_frame(data$features$attributes)
  df$Date <- format(df$Date, scientific=FALSE)
  df$Date <- format(as_datetime(as.numeric(substr(df$Date, 1, nchar(df$Date)-3))), "%d/%m/%Y")
  df = subset(df, select = -c(OBJECTID,GlobalID) )
  df[]<-lapply(df,replace_na_with_previous)
  
  return(df)
}

replace_na_with_previous<-function (vector) {
  if (is.na(vector[1])) 
    vector[1] <- na.omit(vector)[1]
  for (i in 1:length(vector)) {
    if ((i - 1) > 0) {
      if (is.na(vector[i])) 
        vector[i] <- vector[i - 1]
    }
  }
  return(vector)
}

df_Cases <- Scripting_Cases("https://services3.arcgis.com/hjUMsSJ87zgoicvl/arcgis/rest/services/Covid_19/FeatureServer/5/query?where=1%3D1&outFields=*&outSR=4326&f=json")
df_Cases <- df_Cases[-c(nrow(df_Cases)),]
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
df_tbats[165,]["Fitted"] <- NA
df_tbats$Index <- date_decimal(df_tbats$Index)
df_tbats$Tbats <- rowSums(df_tbats[, c("Fitted","Point Forecast")], na.rm=T)

sarima_forecast <- auto.arima(training, trace=TRUE, test="kpss", ic="bic")
sarima_forecast <- forecast(sarima_forecast, h=length(test$Cas_confirmés))
df_sarima <- fortify(sarima_forecast, ts.connect = TRUE)
df_sarima$Index <- date_decimal(df_sarima$Index)
df_sarima[165,]["Fitted"] <- NA
df_sarima$Sarima <- rowSums(df_sarima[, c("Fitted","Point Forecast")], na.rm=T)


Date <- c(df_tbats$Index)
Sarima <- c(df_sarima["Sarima"])
Tbats <- c(df_tbats["Tbats"])
Actuel <- c(df_Cases["Cas_confirmés"][1:nrow(df_sarima["Sarima"]),])
df_data <- data.frame(Date, Sarima, Tbats, Actuel)


header <- dashboardHeader(title = "Covid19 Morocco", tags$li(a(href = 'https://www.linkedin.com/in/mohamed-aghezzaf/', icon("linkedin"), title = "LinkedIn"), class = "dropdown"),  tags$li(a(href = 'https://github.com/aghezzafmohamed', icon("github"), title = "My Github account"), class = "dropdown"))  
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Forecasting", tabName = "forecasting", icon = icon("fas fa-chart-line")),
    menuItem("À propos", tabName = "about", icon = icon("fad fa-info"))
  )
)
body <- dashboardBody(
  tabItems(tabItem(tabName = "dashboard",
                   frow1 <- fluidRow(
                     valueBoxOutput("Cas_confirmés"),
                     valueBoxOutput("Décédés"),
                     valueBoxOutput("Retablis")
                   ),
                   
                   frow2 <- fluidRow( 
                     box(
                       title = "Incidences Chart",
                       status = "primary",
                       solidHeader = TRUE,
                       
                       collapsible = TRUE ,
                       plotlyOutput("plot_totale", height = "350")
                     ),
                     box(
                       title = "Daily cases",
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       
                       selectInput(inputId = "selectedvariable",
                                   label = "Select a variable", 
                                   choices = c("Cases", "Deaths ", "Negative tests", "Tested", "Recovered")),
                       plotlyOutput("plot_cases", height = "270")
                     ) 
                   ),
                   
                   frow3 <- fluidRow( 
                     box(
                       title = "Data table",
                       status = "primary",
                       solidHeader = F, 
                       collapsible = T,
                       width = 12,
                       downloadButton("download","CSV"),
                       column( 12,align="center", DT::dataTableOutput("mytable", height = "300px"))
                     ) 
                   )
  ),
  
  tabItem(tabName = "forecasting",
          fluidRow(
            sidebarPanel(
              tags$hr(),
              selectInput(inputId = "selectedvariable",
                          label = "Select a variable", 
                          choices = c("Cases", "Deaths ", "Recovered")),
              checkboxGroupInput("variable", "Module to show:",
                                 c("SARIMA" = "SARIMA",
                                   "TBATS" = "TBATS"), selected = c("SARIMA" = "SARIMA",
                                                                    "TBATS" = "TBATS")),
              sliderInput(inputId="i_forecast_n","Forecast periods",value=30,min=2,max=120,step=1),
              actionButton("Plot", "Start forecasting!", icon("paper-plane"), 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              width=3),tags$hr(),
            
            
            column(width=9,
                   
                   tabPanel("Plot",
                            plotlyOutput("plot_forecasting")
                   )
            ))
  ),
  
  tabItem(tabName = "about",
          titlePanel("Real-time visualization and forecasting of COVID-19"),
          sidebarLayout(
            sidebarPanel(img(src = "mohamed.png", height = 160, width = 160), h3("Aghezzaf mohamed"), width = 4),
            mainPanel(
              br(),
              p("Application for real-time visualization and forecasting of COVID-19, data on the number of patients diagnosed, the number of patients who died and the number of patients cured. These figures are based on information from the Geomatic API, Morocco.", style = "font-family: 'times'; font-size : 16pt"),
              br(),
              strong("Last update"), strong(tail(df_Cases$Date, 1)),
              br(),
              a(href = 'https://services3.arcgis.com/hjUMsSJ87zgoicvl/arcgis/rest/services/Covid_19/FeatureServer/5/query?where=1%3D1&outFields=*&outSR=4326&f=json', icon("link"), title = "My Github account"),
              strong("API Link"),
              br(),
              br(),
              a(href = 'https://github.com/aghezzafmohamed', icon("github"), title = "My Github account"),
              strong("My Github account"),
              br(),
              a(href = 'https://www.linkedin.com/in/mohamed-aghezzaf/', icon("linkedin"), title = "LinkedIn"),
              strong("My LinkedIn account"),
              br(),
            )
          )
  )
  ))

ui <- dashboardPage(title = "Covid19 Maroc", header, sidebar, body)


server <- function(input, output) {
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



#Run app
shinyApp(ui, server)
