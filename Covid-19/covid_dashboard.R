library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(readr)
library(sf)
library(dplyr)
library(plotly)
library(scales)
library(lubridate)
library(DT)
library(shinyauthr)

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("Max", "Erika"),
  password = sapply(c("AdminPasswort", "StandardPasswort"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("Max Musterman", "Erika Musterman")
)

# die CSV-Dateien einlesen
daten <- read_csv('faelle-todesfaelle-deutschland-monat-jahr.csv')
impfungen <- read_csv('impfen-bundeslaender.csv')
infektionsdaten <- read_csv('gesamt-zahlen-bundeslaender.csv')
data <- read_csv('WHO-COVID-19-global-data.csv')
COVID_full_df <- read_csv('WHO-COVID-19-global-data.csv')

# einlesen der conversion Datei für iso 3 code
alpha2_3 <- read_csv('countries_codes_and_coordinates.csv')

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

# Covid Datensatz als Eu Datensatz reduziert
eu <- COVID_full_df%>%
  filter(WHO_region == "EURO")

# Merging der beiden Daten
ergebnis <- merge(COVID_full_df, alpha2_3, 
                  by.x = "Country_code", 
                  by.y = "Alpha-2 code", 
                  all.x = TRUE)


# Umbenennung der Spalte 10 in "land"
colnames(ergebnis)[10] <- "land"

### properties für plot
graph_properties <- list(
  scope = 'europe',
  landcolor = toRGB("white"),
  color = toRGB("white"),
  bgcolor = 'rgba(0,0,0,0)'
)

font = list(
  family = "DM Sans",
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = font
)


# Datenverarbeitung
neuer_datensatz <- ergebnis %>%
  filter(WHO_region == "EURO") %>%
  group_by(land, Country.y, Datum = format(Date_reported, "%Y-%m")) %>%
  summarise(Gesamt_pro_Monat = max(Cumulative_cases, na.rm = TRUE)) %>%
  mutate(hover = paste0(Country.y, "\n",
                        format(round(Gesamt_pro_Monat,2),
                               decimal.mark = ",", big.mark = ".")))

neuer_datensatz$Datum <- as.Date(paste0(neuer_datensatz$Datum, "-01"))


population <- read_csv('Countries-Europe.csv')

test <- merge(neuer_datensatz, population, 
              by.x = "land", 
              by.y = "ISO alpha 3", 
              all.x = TRUE)

test$tausend <- test$Gesamt_pro_Monat / test$population *100000

daten_fuer_pca <- eu[, c('New_cases', 'Cumulative_cases')]

pca <- prcomp(daten_fuer_pca, scale = TRUE)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Willkommen", tabName = "willkommen"),
    menuItem("Deutschland", tabName = "deutschland"),
    menuItem("Europa", tabName = "europa"),
    menuItem("Welt", tabName = "welt")
  )
)

body <- dashboardBody(
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  shinyauthr::loginUI(id = "login"),
  uiOutput("body")
)

ui <- dashboardPage(
  dashboardHeader(title = 'COVID-19 Dashboard'),
  sidebar,
  body
)

server <- function(input, output) {
credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
    )
    
    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )

  output$body <- renderUI({
  req(credentials()$user_auth)
  tags$head(
    tags$style(HTML("
    .custom-welcome-text h1,
    .custom-welcome-text h3,
    .custom-welcome-text p {
      font-weight: bold; 
    }
  "))
  )
  tabItems(
    # Willkommen Tab-Inhalt
    tabItem(tabName = "willkommen",
            div(class = "custom-welcome-text",
                h1("Willkommen beim COVID-19 Dashboard", align = "center"),
                h2("Herzlich willkommen auf unserem COVID-19 Dashboard. Diese Plattform bietet einen umfassenden Überblick über die aktuellen COVID-19-Zahlen und warum es wichtig ist, sich mit diesen Daten auseinanderzusetzen. Hier ist eine kurze Einführung:"),
                h3("Die Bedeutung der Daten"),
                p("Dieses Dashboard ermöglicht es Ihnen, die aktuellen Fallzahlen, Impfstatistiken und andere relevante Informationen im Zusammenhang mit COVID-19 zu verfolgen. Die Daten werden sorgfältig aus verschiedenen vertrauenswürdigen Quellen gesammelt und bieten eine zuverlässige Grundlage für Entscheidungen und Handlungen."),
                h3("Warum ist es wichtig?"),
                tags$ul(
                  tags$li("Informierte Entscheidungen treffen: Durch das Verfolgen der aktuellen COVID-19-Zahlen können Sie informierte Entscheidungen für sich selbst, Ihre Familie und Ihre Gemeinschaft treffen. Dies ist besonders wichtig, da die Situation sich ständig ändert."),
                  tags$li("Prävention und Schutz: Das Dashboard bietet Einblicke in die Verbreitung des Virus in verschiedenen Regionen. Diese Informationen können dazu beitragen, präventive Maßnahmen zu verstehen und zu ergreifen, um sich selbst und andere zu schützen."),
                  tags$li("Impffortschritt überwachen: Die Impfstatistiken auf dem Dashboard geben Aufschluss darüber, wie weit die Impfkampagnen fortgeschritten sind. Dies ist entscheidend, um die Herdenimmunität zu erreichen und die Ausbreitung des Virus zu verlangsamen."),
                  tags$li("Globales Verständnis fördern: COVID-19 betrifft die ganze Welt. Durch das Verstehen globaler Trends und Auswirkungen können wir besser zusammenarbeiten, um die Pandemie einzudämmen und gemeinsam Lösungen zu finden.")
                ),
                h3("Wie das Dashboard Ihnen hilft"),
                tags$ul(
                  tags$li("Aktuelle Daten: Wir aktualisieren regelmäßig die Daten, um Ihnen stets die neuesten Informationen zur Verfügung zu stellen."),
                  tags$li("Interaktive Visualisierungen: Grafiken und Karten erleichtern das Verständnis komplexer Zusammenhänge."),
                  tags$li("Zusammenarbeit fördern: Das Dashboard dient als Plattform für den Austausch von Informationen und Erfahrungen, um eine stärkere Gemeinschaftsantwort zu fördern.")
                ),
                h3("Unsere Quellen"),
                h4("Die Daten für dieses Dashboard werden aus folgenden Quellen bezogen:"),
                tags$ul(
                  tags$li("Weltgesundheitsorganisation (WHO)"),
                  tags$li("Robert Koch-Institut (RKI)"),
                ),
                
                p("Wir hoffen, dass dieses Dashboard nicht nur als Informationsquelle dient, sondern auch dazu beiträgt, das Bewusstsein für die Bedeutung gemeinsamer Anstrengungen zur Bewältigung der COVID-19-Pandemie zu stärken.")
            )
    ),
    # Deutschland Tab-Inhalt
    tabItem(tabName = "deutschland",
            h2("Informationen über die COVID-19-Pandemie in Deutschland, einschließlich Fallzahlen und Todesfälle sowie Impfungen nach den Berichten des RKI."),
            fluidRow(
              column(6,
                     selectInput("dropdown_select", "Wähle Daten aus:", choices = c("Infektionen", "Todesfälle")),
                     'Wähle den gewünschten Input und erkunde eine Deutschlandkarte von den gesamt Zahlen der COVID-19 (Todes)Fällen für jedes Bundesland. Fahre mit der Maus über die Länder, um detaillierte Informationen anzuzeigen.',
                     plotlyOutput("mapOutput")
              ),
              column(6,
                     selectInput("auswahl", "Wähle Daten aus:", choices = c("Impfquote", "Impfungen")),
                     'Wähle entweder die Impfqoute oder Impfungen (komplette Zahlen) und lass dir ein Balkendiagramm für die COVID-19 Impfungen pro Bundesland anzeigen. Fahre mit der Maus über die Balken, um detaillierte Informationen anzuzeigen.',
                     plotlyOutput("impfChart")
              )
            ),
            fluidRow(
              column(12,
                     selectInput("jahrInput", "Jahr wählen:", choices = unique(daten$Jahr)),
                     'Wähle ein Jahr (2020, 2021, 2022 oder 2023) und lass dir jeweils ein Säulendiagramm für die COVID-19 (Todes)Fälle in Deutschland ausgeben. Fahre mit der Maus über die Säulen, um detaillierte Informationen anzuzeigen.'
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
            h2("Informationen über die europaweiten COVID-19-Pandemie nach den Berichten der WHO."),
            fluidRow(
              column(12,
                     'Wähle ein Jahr (2020, 2021, 2022 oder Gesamt) und erkunde eine Europakarte mit kumulierten COVID-19-Fällen für jedes Land. Fahre mit der Maus über die Länder, um detaillierte Informationen anzuzeigen.',
                     selectInput('Jahr', '', choices = c('2020' = '2020', '2021' = '2021', '2022' = '2022', 'Gesamt' = 'Gesamt')),
                     plotlyOutput("Eu_map_plot"),
                     h3('Statistikdaten:'),
                     'Erhalte Zusammenfassungen und Strukturanalysen der COVID-19-Daten für Europa. Nutze Tabellen, um detaillierte Datensätze anzuzeigen. Hauptkomponentenanalyse (PCA): Erforsche die Anwendung der PCA auf ausgewählte COVID-19-Variablen. Erhalte Einblicke in die Dimensionalität der Daten und Muster.',
                     tabsetPanel(
                       tabPanel('Summary', verbatimTextOutput('sum')),
                       tabPanel('Structure', verbatimTextOutput('str')),
                       tabPanel('pca', verbatimTextOutput('pca'))
                     )
              )
            )
    ),
    
    # Welt Tab-Inhalt
    tabItem(tabName = "welt",
            h2("Informationen über die weltweite COVID-19-Pandemie, einschließlich Fallzahlen und Todesfälle nach den Berichten der WHO."),
            fluidRow(
              column(6,
                     'Wähle ein Jahr (2020, 2021, 2022 oder Gesamt) und erkunde eine Weltkarte mit kumulierten COVID-19-Fällen für jedes Land. Fahre mit der Maus über die Länder, um detaillierte Informationen anzuzeigen.',
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
})
  
  # Erstelle der interaktiven Karte für Todes- und Fälle
  output$mapOutput <- renderPlotly({
  req(credentials()$user_auth)
    
    selected_data <- switch(input$dropdown_select,
                            "Infektionen" = daten_merged$Infektionen,
                            "Todesfälle" = daten_merged$Todesfälle)
    
    daten_merged$tooltip_text <- paste("Bundesland:", daten_merged$gen, "<br>", input$dropdown_select, ":", format_number_for_tooltip_map(selected_data))
    
    p <- ggplot(data = daten_merged) +
      geom_sf(aes(fill = selected_data, text = tooltip_text), color = "#ffffff") +
      scale_fill_gradient(low = "#f5b588", high = "#b61220", na.value = NA, name = input$dropdown_select,
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
  req(credentials()$user_auth)
    p <- ggplot(data = gefilterteDaten(), aes(y = MonatName, x = Faelle_gesamt, text = Faelle_tooltip)) +
      geom_bar(stat = "identity", fill = "#f0c8a9") +
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
  req(credentials()$user_auth)
    p <- ggplot(data = gefilterteDaten(), aes(y = MonatName, x = Todesfaelle_gesamt, text = Todesfaelle_tooltip)) +
      geom_bar(stat = "identity", fill = "#b44646") +
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
    req(credentials()$user_auth)
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
      geom_bar(stat = "identity", fill = "#f0c8a9") +
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
  req(credentials()$user_auth)
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
          bgcolor = 'rgba(0,0,0,0)',
          lataxis = list(range = c(-60, 90)),  
          lonaxis = list(range = c(-180, 180))  
        ),
        width = 1280,
        height = 450,
        margin = list(l = 50),
        colorbar = list(title = NULL),  # Hier wird die Überschrift der Legende entfernt
        plot_bgcolor= 'transparent',
        paper_bgcolor= 'transparent'
      )
    
    return(fig)
  })
  
  output$world_map_deaths <- renderPlotly({
  req(credentials()$user_auth)
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
          bgcolor = 'rgba(0,0,0,0)',
          lataxis = list(range = c(-60, 90)),  
          lonaxis = list(range = c(-180, 180))  
        ),
        width = 1280,
        height = 450,
        margin = list(l = 50),
        colorbar = list(title = NULL),
        plot_bgcolor= 'transparent',
        paper_bgcolor= 'transparent'
      )
    
    return(fig)
  })
  
  #### Stats daten ####
  
  output$sum <- renderPrint({
    summary(eu[, c('New_cases','Cumulative_cases','New_deaths','Cumulative_deaths')])
  })
  
  output$pca <- renderPrint({
    summary(pca)
  })
  
  output$str <- renderPrint({
    str(eu)
  })
  
  output$data <- renderTable({
    eu[colnames(eu)]
  })  
  
  
  
  output$Eu_map_plot <- renderPlotly({
  req(credentials()$user_auth)
    # Bedingte Verzweigung basierend auf der Auswahl des Jahres
    if (input$Jahr == '2020') {
      # Code für das Jahr 2020
      Eu_map <- plot_geo(neuer_datensatz %>% filter(year(Datum) == 2020),
                         locationmode = "ISO-3",
                         frame = ~Datum) %>%
        add_trace(locations = ~land,
                  z = ~Gesamt_pro_Monat,
                  zmin = min(neuer_datensatz$Gesamt_pro_Monat),
                  zmax = max(neuer_datensatz$Gesamt_pro_Monat),
                  color = 'hot',
                  colorscale = ~Gesamt_pro_Monat,
                  text = ~hover,
                  hoverinfo = 'text') %>%
        layout(geo = graph_properties,
               title = "Corona Fälle Gesamt 2020",
               font = list(family = "DM Sans"),
               plot_bgcolor= 'transparent',
               paper_bgcolor= 'transparent'
               ) %>%
        config(displayModeBar = FALSE) %>%
        style(hoverlabel = label)
    }
    
    else if (input$Jahr == '2021') {
      # Code für das Jahr 2021
      Eu_map <- plot_geo(neuer_datensatz %>% filter(year(Datum) == 2021),
                         locationmode = "ISO-3",
                         frame = ~Datum) %>%
        add_trace(locations = ~land,
                  z = ~Gesamt_pro_Monat,
                  zmin = 0,
                  zmax = max(neuer_datensatz$Gesamt_pro_Monat),
                  color = 'hot',
                  colorscale = ~Gesamt_pro_Monat,
                  text = ~hover,
                  hoverinfo = 'text') %>%
        layout(geo = graph_properties,
               title = "Corona Fälle Gesamt 2021",
               font = list(family = "DM Sans"),
               plot_bgcolor= 'transparent',
               paper_bgcolor= 'transparent'
               ) %>%
        config(displayModeBar = FALSE) %>%
        style(hoverlabel = label)
    }
    
    else if (input$Jahr == '2022') {
      # Code für das Jahr 2022
      Eu_map <- plot_geo(neuer_datensatz %>% filter(year(Datum) == 2022),
                         locationmode = "ISO-3",
                         frame = ~Datum) %>%
        add_trace(locations = ~land,
                  z = ~Gesamt_pro_Monat,
                  zmin = 0,
                  zmax = max(neuer_datensatz$Gesamt_pro_Monat),
                  color = 'hot',
                  colorscale = ~Gesamt_pro_Monat,
                  text = ~hover,
                  hoverinfo = 'text') %>%
        layout(geo = graph_properties,
               title = "Corona Fälle Gesamt 2022",
               font = list(family = "DM Sans"),
               plot_bgcolor= 'transparent',
               paper_bgcolor= 'transparent'
               ) %>%
        config(displayModeBar = FALSE) %>%
        style(hoverlabel = label)
    } else {
      # Code für die Gesamtansicht
      Eu_map <- plot_geo(neuer_datensatz,
                         locationmode = "ISO-3",
                         frame = ~Datum) %>%
        add_trace(locations = ~land,
                  z = ~Gesamt_pro_Monat,
                  zmin = 0,
                  zmax = max(neuer_datensatz$Gesamt_pro_Monat),
                  color = 'hot',
                  colorscale = ~Gesamt_pro_Monat,
                  text = ~hover,
                  hoverinfo = 'text') %>%
        layout(geo = graph_properties,
               title = "Corona Fälle Gesamt",
               font = list(family = "DM Sans"),
               plot_bgcolor= 'transparent',
               paper_bgcolor= 'transparent'
               ) %>%
        config(displayModeBar = FALSE) %>%
        style(hoverlabel = label)
    }
    
    return(Eu_map)
  })
}

shinyApp(ui=ui, server=server)
