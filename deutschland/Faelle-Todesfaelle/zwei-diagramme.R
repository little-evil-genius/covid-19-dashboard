library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

# Lese die CSV-Datei ein (ersetze 'deine_daten.csv' mit dem Pfad zu deiner CSV-Datei)
daten <- read_csv('faelle-todesfaelle-deutschland-monat-jahr.csv')

# Konvertiere das Berichtsdatum in ein Datum und extrahiere Jahr und Monat als separate Spalten
daten$Berichtsdatum <- as.Date(paste0(daten$Berichtsdatum, "-01"))
daten$Jahr <- format(daten$Berichtsdatum, "%Y")
daten$Monat <- format(daten$Berichtsdatum, "%m")

# Ordne die Monate richtig an
monatsnamen <- c("Januar", "Februar", "März", "April", "Mai", "Juni",
                 "Juli", "August", "September", "Oktober", "November", "Dezember")
daten$MonatName <- factor(monatsnamen[as.integer(daten$Monat)], levels = monatsnamen)

# UI
ui <- fluidPage(
  titlePanel("COVID-19 Statistiken"),

  sidebarLayout(
    sidebarPanel(
      selectInput("jahrInput", "Jahr wählen:", choices = unique(daten$Jahr))
    ),

    mainPanel(
      plotOutput("faellePlot"),
      plotOutput("todesfaellePlot")
    )
  )
)

# Server
server <- function(input, output) {

  # Reaktiver Ausdruck für gefilterte Daten basierend auf dem gewählten Jahr
  gefilterteDaten <- reactive({
    daten %>% filter(Jahr == input$jahrInput)
  })

  # Erstelle das Säulendiagramm für Fälle
  output$faellePlot <- renderPlot({
    ggplot(data = gefilterteDaten(), aes(y = MonatName, x = Faelle_gesamt)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme(axis.text.y = element_text(angle = 0)) +
      labs(y = "", x = "Anzahl der Fälle") +
      scale_x_continuous(labels = scales::comma) +
      ggtitle(paste("COVID-19 Fälle im Jahr", input$jahrInput)) +
      coord_flip()
  })

  # Erstelle das Säulendiagramm für Todesfälle
  output$todesfaellePlot <- renderPlot({
    ggplot(data = gefilterteDaten(), aes(y = MonatName, x = Todesfaelle_gesamt)) +
      geom_bar(stat = "identity", fill = "red") +
      theme(axis.text.y = element_text(angle = 0)) +
      labs(y = "", x = "Anzahl der Todesfälle") +
      scale_x_continuous(labels = scales::comma) +
      ggtitle(paste("COVID-19 Todesfälle im Jahr", input$jahrInput)) +
      coord_flip()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
