library(shiny)
library(readr)
library(dplyr)
library(scales)
library(plotly)

library(lubridate)


# einlesen der Daten
COVID_full_df <- read_csv("/Users/alex/Desktop/ShinyDashboard/eu/WHO-COVID-19-global-data.csv")

# einlesen der conversion Datei für iso 3 code
alpha2_3 <- read_csv('/Users/alex/Desktop/ShinyDashboard/eu/countries_codes_and_coordinates.csv')

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
  color = toRGB("white")
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


shinyServer(function(input, output) {
  output$Eu_map_plot <- renderPlotly({
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
                  reversescale = F,
                  hoverinfo = 'text') %>%
        layout(geo = graph_properties,
               title = "Corona Fälle Gesamt 2020",
               font = list(family = "DM Sans")) %>%
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
                  reversescale = F,
                  hoverinfo = 'text') %>%
        layout(geo = graph_properties,
               title = "Corona Fälle Gesamt 2021",
               font = list(family = "DM Sans")) %>%
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
                  reversescale = F,
                  hoverinfo = 'text') %>%
        layout(geo = graph_properties,
               title = "Corona Fälle Gesamt 2022",
               font = list(family = "DM Sans")) %>%
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
                  color = input$color,
                  colorscale = ~Gesamt_pro_Monat,
                  text = ~hover,
                  reversescale = F,
                  hoverinfo = 'text') %>%
        layout(geo = graph_properties,
               title = "Corona Fälle Gesamt",
               font = list(family = "DM Sans")) %>%
        config(displayModeBar = FALSE) %>%
        style(hoverlabel = label)
    }

    return(Eu_map)

    #### Stats daten ####
    output$sum <- renderPrint({
      summary(eu[, c('New_cases','Cumulative_cases','New_deaths','Cumulative_deaths')])
    })

    output$str <- renderPrint({
      str(eu)
    })

    output$data <- renderTable({
      eu[colm()]
    })

  })
})


population <- read_csv('/Users/alex/Desktop/ShinyDashboard/eu/Countries-Europe.csv')

test <- merge(neuer_datensatz, population, 
              by.x = "land", 
              by.y = "ISO alpha 3", 
              all.x = TRUE)

test$Ergebnis <- test$Gesamt_pro_Monat / test$population *100000

#### ####


