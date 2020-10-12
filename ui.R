library(plotly)
library(numberFormattR)
library(astsa)


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
              strong("Last update"),strong(tail(df_Cases$Date, 1)),
              br(),
              a(href = 'https://github.com/aghezzafmohamed', icon("link"), title = "My Github account"),
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
