library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(sf)
library(dplyr)
library(plotly)
library(scales)

# Lese die CSV-Dateien ein
daten <- read_csv('faelle-todesfaelle-deutschland-monat-jahr.csv')
impfungen <- read_csv('impfen-bundeslaender.csv')
infektionsdaten <- read.csv("gesamt-zahlen-bundeslaender.csv", stringsAsFactors = FALSE)

# Laden der Geodaten für Deutschland
deutschland_geodaten <- st_read("germany_map.geojson")

# Konvertiere das Berichtsdatum in ein Datum und extrahiere Jahr und Monat als separate Spalten
daten$Berichtsdatum <- as.Date(paste0(daten$Berichtsdatum, "-01"))
daten$Jahr <- format(daten$Berichtsdatum, "%Y")
daten$Monat <- format(daten$Berichtsdatum, "%m")

# Ordne die Monate richtig an
monatsnamen <- c("Januar", "Februar", "März", "April", "Mai", "Juni",
                 "Juli", "August", "September", "Oktober", "November", "Dezember")
daten$MonatName <- factor(monatsnamen[as.integer(daten$Monat)], levels = monatsnamen)

# Stelle sicher, dass 'Impfquote' und 'Impfungen' als numerisch eingelesen werden
impfungen$Impfquote <- as.numeric(as.character(impfungen$Impfquote))
impfungen$Impfungen <- as.numeric(as.character(impfungen$Impfungen))

# Benutzerdefinierte Funktion zur Formatierung der Zahlen für den Tooltip
format_number_for_tooltip <- function(number) {
  if (number >= 1e6) {
    return(paste0(format(round(number / 1e6, 1), nsmall = 1), " Mio."))
  } else if (number >= 1e3) {
    return(paste0(format(round(number / 1e3), nsmall = 0), " Tsd."))
  } else {
    return(as.character(number))
  }
}

# Merge der Infektionsdaten mit den Geodaten
daten_merged <- deutschland_geodaten %>%
  left_join(infektionsdaten, by = c("NAME_1" = "Bundesland"))

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
                     plotlyOutput("germany_map")
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
  
  
  output$mapOutput <- renderPlot({
    ggplot(data = daten_merged) +
      geom_sf(aes(fill = Impfektionen), color = "white") +
      scale_fill_gradient(low = "green", high = "red", na.value = NA) +
      labs(fill = "Infektionen") +
      theme_minimal()
  })
  
  # Erstelle das Balkendiagramm für Impfdaten
  output$impfChart <- renderPlotly({
    # Entscheide basierend auf dem Dropdown-Menü, welche Daten angezeigt werden sollen
    datenZuZeigen <- if (input$auswahl == "Impfquote") {
      impfungen$Impfquote
    } else {
      impfungen$Impfungen
    }
    
    # Erstelle eine neue Spalte im DataFrame für den Tooltip-Text
    impfungen$tooltip_text <- if (input$auswahl == "Impfquote") {
      paste(impfungen$Bundesland, ': ', impfungen$Impfquote, '%', sep = '')
    } else {
      paste(impfungen$Bundesland, ': ', sapply(impfungen$Impfungen, format_number_for_tooltip), sep = '')
    }
    
    # Erstelle das Balkendiagramm mit angepasstem Tooltip-Text
    p <- ggplot(impfungen, aes(x = reorder(Bundesland, -datenZuZeigen), y = datenZuZeigen,
                               text = tooltip_text)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() + # Um die Balken horizontal anzuzeigen
      labs(x = "", y = input$auswahl) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "transparent", colour = NA))
    
    # Anpassung der Achsenbeschriftung und Erweiterung der y-Achse bei Bedarf
    if (input$auswahl == "Impfungen") {
      p <- p + expand_limits(y = c(500000, NA)) +
        scale_y_continuous(labels = scales::comma) # Verwende Kommas als Tausendertrennzeichen
    } else {
      p <- p + scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) # Zeige Prozentwerte für Impfquote ohne Umrechnung in Dezimalzahl
    }
    
    # Konvertiere ggplot-Objekt zu Plotly und stelle sicher, dass das angepasste Tooltip verwendet wird
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Reaktiver Ausdruck für gefilterte Daten basierend auf dem gewählten Jahr
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
      scale_x_continuous(labels = scales::comma) +
      ggtitle(paste("COVID-19 Fälle im Jahr", input$jahrInput)) +
      coord_flip()
    
    ggplotly(p, tooltip = c("text")) %>% config(displayModeBar = FALSE)
  })
  
  # Erstelle das interaktive Säulendiagramm für Todesfälle
  output$todesfaellePlot <- renderPlotly({
    p <- ggplot(data = gefilterteDaten(), aes(y = MonatName, x = Todesfaelle_gesamt, text = Todesfaelle_tooltip)) +
      geom_bar(stat = "identity", fill = "red") +
      theme(axis.text.y = element_text(angle = 0)) +
      labs(y = "", x = "Anzahl der Todesfälle") +
      scale_x_continuous(labels = scales::comma) +
      ggtitle(paste("COVID-19 Todesfälle im Jahr", input$jahrInput)) +
      coord_flip()
    
    ggplotly(p, tooltip = c("text")) %>% config(displayModeBar=FALSE)
  })
}

shinyApp(ui=ui, server=server)
