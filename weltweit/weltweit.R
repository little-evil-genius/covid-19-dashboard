library(dplyr)
library(lubridate)
library(plotly)
library(shiny)
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

ui <- fluidPage(
  titlePanel("COVID-19 Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_year", "Jahr (2020-2023):", choices = unique(data_cases$Year))
    ),
    mainPanel(
      fluidRow(
        column(width = 12,
               tabsetPanel(
                 tabPanel("Kumulierte Fallzahlen nach Land", plotlyOutput("world_map_cases")),
                 tabPanel("Kumulierte Todesfälle nach Land", plotlyOutput("world_map_deaths")),
                 tabPanel("Prozentuale Verteilung nach Kontinent", plotlyOutput("continent_pie_chart"))
               )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data_cases <- reactive({
    subset_data <- data_cases[data_cases$Year == input$selected_year, ]
    return(subset_data)
  })
  
  filtered_data_deaths <- reactive({
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
      color = ~Total_Cumulative_Cases,
      colorscale = "Plasma",
      text = ~paste(Country, "<br>Total Cases: ", scales::comma(Total_Cumulative_Cases)),
      hoverinfo = "text"
    ) %>%
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'mercator')
        ),
        width = 1000,
        height = 500,
        margin = list(l = 50)  
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
      color = ~Total_Cumulative_deaths,
      colorscale = "Plasma",
      text = ~paste(Country, "<br>Total Cases: ", scales::comma(Total_Cumulative_deaths)),
      hoverinfo = "text"
    ) %>%
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'mercator')
        ),
        width = 1000,
        height = 500,
        margin = list(l = 50)
      )
    
    return(fig)
  })
  
  output$continent_pie_chart <- renderPlotly({
    continent_data <- data %>%
      filter(Year == input$selected_year) %>%
      group_by(Continent = WHO_region) %>%
      summarise(Total_Cumulative_Cases = sum(Cumulative_cases))
    
    fig <- plot_ly(
      data = continent_data,
      labels = ~Continent,
      values = ~Total_Cumulative_Cases,
      type = 'pie',
      textinfo = 'percent+label',
      insidetextorientation = 'radial',
      hoverinfo = 'label+percent'
    ) %>%
      layout(
        title = "Prozentuale Verteilung der Fallzahlen nach Kontinent",
        showlegend = TRUE
      )
    
    return(fig)
  })
}

shinyApp(ui = ui, server = server)
