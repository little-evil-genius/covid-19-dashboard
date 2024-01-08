library(dplyr)
library(lubridate)
library(plotly)
library(shiny)
library(scales)

data <- read.csv(file="C:/Users/gimmi/OneDrive/Universität/5. Semester_WS23/Datenbanken und Datenanalyse/WHO-COVID-19-global-data.csv", 
                 sep =",", dec = ",", header = TRUE)

data_cases <- data %>%
  select(-Country_code, -WHO_region, -New_cases, -New_deaths, 
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
      tabsetPanel(
        tabPanel("Kumulierte Fallzahlen nach Land", plotlyOutput("world_map_cases")),
        tabPanel("Kumulierte Todesfälle nach Land", plotlyOutput("world_map_deaths"))
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
    )
    
    fig <- fig %>% layout(
      #title = paste("Kumulierte Fälle nach Land im Jahr", input$selected_year),
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'mercator')
      )
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
    )
    
    fig <- fig %>% layout(
      #title = paste("Kumulierte Todesfälle nach Land im Jahr", input$selected_year),
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'mercator')
      )
    )
    
    return(fig)
  })
}

shinyApp(ui = ui, server = server)

