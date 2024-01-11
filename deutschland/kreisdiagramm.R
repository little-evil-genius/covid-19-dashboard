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

# Berechne den Prozentsatz der Todesfälle pro Monat
daten <- daten %>%
  mutate(TodesfallProzent = ifelse(Faelle_gesamt > 0, (Todesfaelle_gesamt / Faelle_gesamt) * 100, 0))

# UI
ui <- fluidPage(
  titlePanel("COVID-19 Statistiken"),

  sidebarLayout(
    sidebarPanel(
      selectInput("jahrInput", "Jahr wählen:", choices = unique(daten$Jahr))
    ),

    mainPanel(
      plotOutput("pieChart")
    )
  )
)

# Server
server <- function(input, output) {

  # Reaktiver Ausdruck für gefilterte Daten basierend auf dem gewählten Jahr
  gefilterteDaten <- reactive({
    daten %>% filter(Jahr == input$jahrInput)
  })

  # Erstelle das Kreisdiagramm für den Prozentsatz der Todesfälle pro Monat
  output$pieChart <- renderPlot({
    gefilterteDatenPie <- gefilterteDaten()

    ggplot(gefilterteDatenPie, aes(x = "", y = TodesfallProzent, fill = MonatName)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      theme_void() +
      labs(fill = "Monat") +
      ggtitle(paste("Prozentualer Anteil der COVID-19 Todesfälle im Jahr", input$jahrInput))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
