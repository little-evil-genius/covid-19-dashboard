library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)
library(scales)

data <- read.csv(file="C:/Users/gimmi/OneDrive/Universität/5. Semester_WS23/Datenbanken und Datenanalyse/WHO-COVID-19-global-data.csv", 
                 sep =",", dec = ",", header = TRUE)

data_cases <- data %>%
  select(-Country_code, -New_cases, -New_deaths, 
         -Cumulative_deaths) %>%
  mutate(Date_reported = as.Date(Date_reported),
         Year = lubridate::year(Date_reported)) %>%
  group_by(Country, Year) %>%
  summarise(Total_Cumulative_Cases = max(Cumulative_cases))

data_deaths <- data %>%
  select(-Country_code, -WHO_region, -New_cases, -New_deaths, 
         -Cumulative_cases) %>%
  mutate(Date_reported = as.Date(Date_reported),
         Year = lubridate::year(Date_reported)) %>%
  group_by(Country, Year) %>%
  summarise(Total_Cumulative_deaths = max(Cumulative_deaths))

ui <- dashboardPage(
  dashboardHeader(title = 'Globale COVID-19-Pandemie Datenzusammentragung'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Deutschland", tabName = "deutschland"),
      menuItem("Europa", tabName = "europa"),
      menuItem("Welt", tabName = "welt")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "deutschland",
              h2("Informationen zur globalen COVID-19-Pandemie innerhalb Deutschlands nach Daten der WHO, inklusive Fallzahlen, Todesfälle und Impfungen.")
      ),
      
      tabItem(tabName = "europa",
              h2("Informationen zu den Fallzahlen, Todesfällen und Impfungen in Europa im Rahmen der COVID-19-Pandemie gemäß den Berichten der WHO.")
      ),
      
      tabItem(tabName = "welt",
              h2("Informationen über die weltweite COVID-19-Pandemie, einschließlich Fallzahlen und Todesfälle sowie Impfungen nach den Berichten der WHO."),
              fluidRow(
                column(6,
                       selectInput("selected_year", "Jahr (2020-2023):", choices = unique(data_cases$Year))
                )
              ),
              fluidRow(
                column(12,
                       tabsetPanel(
                         tabPanel("Kumulierte Fallzahlen nach Land", plotlyOutput("world_map_cases")),
                         tabPanel("Kumulierte Todesfälle nach Land", plotlyOutput("world_map_deaths"))
                       )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data_cases <- reactive({
    req(input$selected_year)
    subset_data <- data_cases[data_cases$Year == input$selected_year, ]
    return(subset_data)
  })
  
  filtered_data_deaths <- reactive({
    req(input$selected_year)
    subset_data <- data_deaths[data_deaths$Year == input$selected_year, ]
    return(subset_data)
  })
  
  output$world_map_cases <- renderPlotly({
    fig <- plot_ly(
      data = filtered_data_cases(),
      type = 'choropleth',
      locations = ~Country,
      locationmode = "country names",
      z = ~Total_Cumulative_Cases,
      color = "hot",
      colorscale = "Plasma",
      text = ~paste(Country, "<br>Total Cases: ", scales::label_number(big.mark = ".")(Total_Cumulative_Cases)),
      hoverinfo = "text"
    ) %>%
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'mercator', center = list(lon = 0)),
          lataxis = list(range = c(-60, 90)),  
          lonaxis = list(range = c(-180, 180))  
        ),
        width = 1280,
        height = 450,
        margin = list(l = 50),
        colorbar = list(title = NULL)  # Hier wird die Überschrift der Legende entfernt
      )
    
    return(fig)
  })
  
  output$world_map_deaths <- renderPlotly({
    fig <- plot_ly(
      data = filtered_data_deaths(),
      type = 'choropleth',
      locations = ~Country,
      locationmode = "country names",
      z = ~Total_Cumulative_deaths,
      color = "hot",
      colorscale = "Plasma",
      text = ~paste(Country, "<br>Total Cases: ", scales::label_number(big.mark = ".")(Total_Cumulative_deaths)),
      hoverinfo = "text"
    ) %>%
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'mercator', center = list(lon = 0)),
          lataxis = list(range = c(-60, 90)),  
          lonaxis = list(range = c(-180, 180))  
        ),
        width = 1280,
        height = 450,
        margin = list(l = 50),
        colorbar = list(title = NULL)  
      )
    
    return(fig)
  })
}

shinyApp(ui = ui, server = server)
