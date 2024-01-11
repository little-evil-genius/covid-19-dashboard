library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(dplyr)
library(plotly)

# Lese die CSV-Dateien ein
daten <- read_csv('faelle-todesfaelle-deutschland-monat-jahr.csv')
impfungen <- read_csv('impfungen-bundeslaender.csv')
bevoelkerung <- read_csv('bevoelkerung.csv')

# Konvertiere das Berichtsdatum in ein Datum und extrahiere Jahr und Monat als separate Spalten
daten$Berichtsdatum <- as.Date(paste0(daten$Berichtsdatum, "-01"))
daten$Jahr <- format(daten$Berichtsdatum, "%Y")
daten$Monat <- format(daten$Berichtsdatum, "%m")

# Ordne die Monate richtig an
monatsnamen <- c("Januar", "Februar", "März", "April", "Mai", "Juni",
                 "Juli", "August", "September", "Oktober", "November", "Dezember")
daten$MonatName <- factor(monatsnamen[as.integer(daten$Monat)], levels = monatsnamen)

# Verbinde die Impfungsdaten mit den Bevölkerungsdaten
impfungen_bev <- left_join(impfungen, bevoelkerung, by = "Bundesland")

# Berechne die Impfrate pro 100 Einwohner
impfungen_bev <- impfungen_bev %>%
  mutate(Impfrate = (Impfungen / Bevoelkerung) * 100)

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
            h2("Informationen zur globalen COVID-19-Pandemie innerhalb Deutschlands nach Daten der WHO, inklusive Fallzahlen, Todesfälle und Impfungen."),
            fluidRow(
              column(12,
                     plotlyOutput("impfungenPlot")
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
  
  # Reaktiver Ausdruck für gefilterte Daten basierend auf dem gewählten Jahr
  gefilterteDaten <- reactive({
    daten %>% filter(Jahr == input$jahrInput)
  })
  
  # Erstelle das interaktive Säulendiagramm für Fälle
  output$faellePlot <- renderPlotly({
    p <- ggplot(data = gefilterteDaten(), aes(y = MonatName, x = Faelle_gesamt)) +
      geom_bar(stat = "identity", fill = "blue") +
      theme(axis.text.y = element_text(angle = 0)) +
      labs(y = "", x = "Anzahl der Fälle") +
      scale_x_continuous(labels = scales::comma) +
      ggtitle(paste("COVID-19 Fälle im Jahr", input$jahrInput)) +
      coord_flip()
    
    ggplotly(p, tooltip = c("x")) %>% config(displayModeBar = FALSE)
  })
  
  # Erstelle das interaktive Säulendiagramm für Todesfälle
  output$todesfaellePlot <- renderPlotly({
    p <- ggplot(data = gefilterteDaten(), aes(y = MonatName, x = Todesfaelle_gesamt)) +
      geom_bar(stat = "identity", fill = "red") +
      theme(axis.text.y = element_text(angle = 0)) +
      labs(y = "", x = "Anzahl der Todesfälle") +
      scale_x_continuous(labels = scales::comma) +
      ggtitle(paste("COVID-19 Todesfälle im Jahr", input$jahrInput)) +
      coord_flip()
    
    ggplotly(p, tooltip = c("x")) %>% config(displayModeBar=FALSE)
  })
  
  # Erstelle das Balkendiagramm für Impfrate pro Bundesland
  output$impfungenPlot <- renderPlotly({
    p <- ggplot(data=impfungen_bev, aes(x=reorder(Bundesland,-Impfrate), y=Impfrate)) +
      geom_bar(stat="identity") + 
      coord_flip() + 
      labs(x="", y="Impfrate pro 100 Einwohner") + 
      theme_minimal() + 
      ggtitle("Impfrate nach Bundesland")
    
    ggplotly(p) %>% config(displayModeBar=FALSE)
  })
}

shinyApp(ui=ui, server=server)
