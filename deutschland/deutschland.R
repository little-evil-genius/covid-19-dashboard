library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(sf)
library(dplyr)
library(plotly)
library(scales)

# die CSV-Dateien einlesen
daten <- read_csv('faelle-todesfaelle-deutschland-monat-jahr.csv')
impfungen <- read_csv('impfen-bundeslaender.csv')
infektionsdaten <- read_csv('gesamt-zahlen-bundeslaender.csv')

# Geodaten für Deutschland laden
deutschland_geodaten <- st_read("germany_map.geojson")

# Berichtsdatum in ein Datum und extrahiere Jahr und Monat als separate Spalten
daten$Berichtsdatum <- as.Date(paste0(daten$Berichtsdatum, "-01"))
daten$Jahr <- format(daten$Berichtsdatum, "%Y")
daten$Monat <- format(daten$Berichtsdatum, "%m")

# Monate richtig an ordnen
monatsnamen <- c("Jan.", "Feb.", "März", "Apr.", "Mai", "Jun.",
                 "Jul.", "Aug.", "Sept.", "Okt.", "Nov.", "Dez.")
daten$MonatName <- factor(monatsnamen[as.integer(daten$Monat)], levels = monatsnamen)

# 'Impfquote' und 'Impfungen' müssen numerisch sein
impfungen$Impfquote <- as.numeric(as.character(impfungen$Impfquote))
impfungen$Impfungen <- as.numeric(as.character(impfungen$Impfungen))

# Formatierung der Zahlen für den Tooltip
format_number_for_tooltip <- function(number) {
  if (number >= 1e6) {
    return(paste0(format(round(number / 1e6, 1), nsmall = 1), " Mio."))
  } else if (number >= 1e3) {
    return(paste0(format(round(number / 1e3), nsmall = 0), " Tsd."))
  } else {
    return(as.character(number))
  }
}
# Formatierung der Zahlen für den Tooltip (Karte)
format_number_for_tooltip_map <- function(numbers) {
  formatted_numbers <- ifelse(numbers >= 1e6, paste0(format(round(numbers / 1e6, 1), nsmall = 1), " Mio."),
                              ifelse(numbers >= 1e3, paste0(format(round(numbers / 1e3), nsmall = 0), " Tsd."), as.character(numbers)))
  return(formatted_numbers)
}

# Merge der Infektionsdaten mit den Geodaten
daten_merged <- deutschland_geodaten %>%
  left_join(infektionsdaten, by = c("gen" = "Bundesland"))


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Deutschland", tabName = "deutschland"),
    menuItem("Europa", tabName = "europa"),
    menuItem("Welt", tabName = "welt")
  )
)

body <- dashboardBody(
  tabItems(
    # Deutschland Tab-Inhalt
    tabItem(tabName = "deutschland",
            h2("Informationen zur COVID-19-Impfung in Deutschland"),
            fluidRow(
              column(6,
                     selectInput("dropdown_select", "Wähle Daten aus:", choices = c("Infektionen", "Todesfälle")),
                     plotlyOutput("mapOutput")
              ),
              column(6,
                     selectInput("auswahl", "Wähle Daten aus:", choices = c("Impfquote", "Impfungen")),
                     plotlyOutput("impfChart")
              )
            ),
            fluidRow(
              column(12,
                     selectInput("jahrInput", "Jahr wählen:", choices = unique(daten$Jahr))
              )
            ),
            fluidRow(
              column(6,
                     plotlyOutput("faellePlot")
              ),
              column(6,
                     plotlyOutput("todesfaellePlot")
              )
            )
    ),
    # Europa Tab-Inhalt
    tabItem(tabName = "europa",
            h2("Informationen zu den Fallzahlen, Todesfällen und Impfungen in Europa im Rahmen der COVID-19-Pandemie gemäß den Berichten der WHO.")
    ),
    # Welt Tab-Inhalt
    tabItem(tabName = "welt",
            h2("Informationen über die weltweite COVID-19-Pandemie, einschließlich Fallzahlen und Todesfälle sowie Impfungen nach den Berichten der WHO.")
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = 'Globale COVID-19-Pandemie Datenzusammentragung'),
  sidebar,
  body
)

server <- function(input, output) {
  
  # Erstelle der interaktiven Karte für Todes- und Fälle
  output$mapOutput <- renderPlotly({
    
    selected_data <- switch(input$dropdown_select,
                            "Infektionen" = daten_merged$Infektionen,
                            "Todesfälle" = daten_merged$Todesfälle)
    
    daten_merged$tooltip_text <- paste("Bundesland:", daten_merged$gen, "<br>", input$dropdown_select, ":", format_number_for_tooltip_map(selected_data))
    
    p <- ggplot(data = daten_merged) +
      geom_sf(aes(fill = selected_data, text = tooltip_text), color = "white") +
      scale_fill_gradient(low = "green", high = "red", na.value = NA, name = input$dropdown_select,
                          labels = scales::label_number(big.mark = ".")) +  
      labs(fill = NULL) +  
      theme_minimal() +
      theme(panel.grid = element_blank(),  
            axis.text.x = element_blank(),  
            axis.text.y = element_blank(),
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
    
    # ggplot-Objekt zu Plotly 
    plotly_obj <- ggplotly(p, tooltip = c("text", "fill")) %>% 
      layout(legend = list(title = "", tickformat = "$,.0f"), 
             showlegend = TRUE) %>%  
      config(displayModeBar = FALSE)
    
    plotly_obj
  })
  
  # gefilterte Daten für das gewählte Jahr
  gefilterteDaten <- reactive({
    daten %>%
      filter(Jahr == input$jahrInput) %>%
      mutate(Faelle_tooltip = sapply(Faelle_gesamt, format_number_for_tooltip),
             Todesfaelle_tooltip = sapply(Todesfaelle_gesamt, format_number_for_tooltip))
  })
  
  # Erstelle das interaktive Säulendiagramm für Fälle
  output$faellePlot <- renderPlotly({
    p <- ggplot(data = gefilterteDaten(), aes(y = MonatName, x = Faelle_gesamt, text = Faelle_tooltip)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme(axis.text.y = element_text(angle = 0)) +
      labs(y = "", x = "Anzahl der Fälle") +
      scale_x_continuous(labels = scales::label_number(big.mark = ".")) +
      ggtitle(paste("COVID-19 Fälle im Jahr", input$jahrInput)) +
      coord_flip() +
      theme_minimal() + 
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
    
    ggplotly(p, tooltip = c("text")) %>% config(displayModeBar = FALSE)
  })
  
  # Erstelle das interaktive Säulendiagramm für Todesfälle
  output$todesfaellePlot <- renderPlotly({
    p <- ggplot(data = gefilterteDaten(), aes(y = MonatName, x = Todesfaelle_gesamt, text = Todesfaelle_tooltip)) +
      geom_bar(stat = "identity", fill = "red") +
      theme(axis.text.y = element_text(angle = 0)) +
      labs(y = "", x = "Anzahl der Todesfälle") +
      scale_x_continuous(labels = scales::label_number(big.mark = ".")) +
      ggtitle(paste("COVID-19 Todesfälle im Jahr", input$jahrInput)) +
      coord_flip() +
      theme_minimal() + 
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
    
    ggplotly(p, tooltip = c("text")) %>% config(displayModeBar=FALSE)
  })
  
  # Erstelle das Balkendiagramm für Impfdaten
  output$impfChart <- renderPlotly({
    
    datenZuZeigen <- if (input$auswahl == "Impfquote") {
      impfungen$Impfquote
    } else {
      impfungen$Impfungen
    }
    
    # neue Spalte im DataFrame für den Tooltip-Text
    impfungen$tooltip_text <- if (input$auswahl == "Impfquote") {
      paste(impfungen$Bundesland, ': ', impfungen$Impfquote, '%', sep = '')
    } else {
      paste(impfungen$Bundesland, ': ', sapply(impfungen$Impfungen, format_number_for_tooltip), sep = '')
    }
    
    # Erstelle das Balkendiagramm mit Tooltip-Text
    p <- ggplot(impfungen, aes(x = reorder(Bundesland, -datenZuZeigen), y = datenZuZeigen,
                               text = tooltip_text)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() + # Um die Balken horizontal anzuzeigen
      labs(x = "", y = input$auswahl) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
    
    # Anpassung der Achsenbeschriftung
    if (input$auswahl == "Impfungen") {
      p <- p + expand_limits(y = c(500000, NA)) +
        scale_y_continuous(labels = scales::label_number(big.mark = "."))
    } else {
      p <- p + scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1))
    }
    
    # ggplot-Objekt zu Plotly 
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
}

shinyApp(ui=ui, server=server)
