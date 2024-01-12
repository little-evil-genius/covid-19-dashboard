library(shiny)
library(plotly)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("superhero"),
  title = tags$script(src = "https://kit.fontawesome.com/17e0029cd2.js"),
  navbarPage(
    title = tags$div(
      tags$i(class = "fa-solid fa-virus-covid"),"Covid-19 Datensatz"),
    
    

#### Weltkarte ####  

    tabPanel(div(icon("globe"),"Weltkarte"),
             sidebarPanel(
               "Section 1",
               tabPanel("Subsection 1.1", "Content for Subsection 1.2"),
               tabPanel("Subsection 1.2", "Content for Subsection 1.2")
             ),
             sidebarPanel(
               "Section 2",
               tabPanel("Subsection 2.1", "Content for Subsection 2.1"),
               tabPanel("Subsection 2.2", "Content for Subsection 2.2")
             ),
              mainPanel("Main content for Tab 1")  # Hauptinhalt für Tab 1
    ),
    
#### Europakarte ####  

    tabPanel("Europa",
navlistPanel(
  "Menüleiste Europa",
  
  tabPanel("Willkommen",
           h2("Corona-Dashboard", align = "center"),
           h4("Überblick"),
           "Willkommen zum Corona-Dashboard! Dieses interaktive Dashboard bietet eine 
           visuelle Darstellung von COVID-19-Daten für europäische Länder. Hier sind einige 
           wichtige Informationen und Funktionen:"
            ),
  
  tabPanel("Corona Fälle Gesamt", 
           h3("Europakarte der COVID-19-Fälle:"),
'Wähle ein Jahr (2020, 2021, 2022 oder Gesamt) und erkunde eine Weltkarte mit 
kumulierten COVID-19-Fällen für jedes Land.
fahre mit der Maus über die Länder, um detaillierte Informationen anzuzeigen.',
           selectInput('Jahr',
                       '',
                       choices = c('2020' = '2020',
                                   '2021' = '2021',
                                   '2022' = '2022',
                                   'Gesamt' = 'Gesamt')
                                  ),
####Eu_map_plot####           
                     plotlyOutput("Eu_map_plot"),

           h3('Statistikdaten:'),
'Erhalte Zusammenfassungen und Strukturanalysen der COVID-19-Daten für Europa.
Nutze Tabellen, um detaillierte Datensätze anzuzeigen.
Hauptkomponentenanalyse (PCA):

Erforsche die Anwendung der PCA auf ausgewählte COVID-19-Variablen.
Erhalte Einblicke in die Dimensionalität der Daten und Muster.',
                     tabsetPanel(
                       tabPanel('Summary', verbatimTextOutput('sum')),
                       tabPanel('Structure', verbatimTextOutput('str')),
                       tabPanel('pca', verbatimTextOutput('pca'))
                       )
           ),

#### Tab Todesfälle pro 100.000 Deutschland#####

  tabPanel("Todesfälle pro 100.000", 
           "Content for Subsection 2.2",
           plotlyOutput("Eu_map_thousend")
           )
)
    ),
    

#### Deutschland #### 

  tabPanel("Deutschland",
           navlistPanel(
             "Section 1",
             tabPanel("Subsection 1.1", "Content for Subsection 1.2"),
             tabPanel("Subsection 1.2", "Content for Subsection 1.2")
           ),
           navlistPanel(
             "Section 2",
             tabPanel("Subsection 2.1", "Content for Subsection 2.2"),
             tabPanel("Subsection 2.2", "Content for Subsection 2.2")
           ),
           mainPanel("Main content for Tab 1")  # Hauptinhalt für Tab 1
  )
),
)


