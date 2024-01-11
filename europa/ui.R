library(shiny)
library(plotly)

ui <- fluidPage(
  title = tags$script(src = "https://kit.fontawesome.com/17e0029cd2.js"),
  navbarPage(
    title = tags$div(
      tags$i(class = "fa-solid fa-virus-covid"),"Covid-19 Datensatz"),
    
    #### Weltkarte ####  
    tabPanel(div(icon("globe"),"Weltkarte"),
             navlistPanel(
               "Section 1",
               tabPanel("Subsection 1.1", "Content for Subsection 1.2"),
               tabPanel("Subsection 1.2", "Content for Subsection 1.2")
             ),
             navlistPanel(
               "Section 2",
               tabPanel("Subsection 2.1", "Content for Subsection 2.1"),
               tabPanel("Subsection 2.2", "Content for Subsection 2.2")
             ),
             # mainPanel("Main content for Tab 1")  # Hauptinhalt für Tab 1
    ),
    
    
    
    #### Europa ####  
    tabPanel("Europa",
             navlistPanel(
               "Section 1",
               tabPanel("Subsection 1.1", 
                        selectInput('Jahr', 
                                    'Wähle ein Jahr', 
                                    choices = c('2020' = '2020',
                                                '2021' = '2021',
                                                '2022' = '2022',
                                                'Gesamt' = 'Gesamt')
               ),
               plotlyOutput("Eu_map_plot"),
               tabsetPanel(type = 'tab',
                           tabPanel('Plot', plotOutput('myhist')),
                           tabPanel('Summary', verbatimTextOutput('sum')),
                           tabPanel('Structure', verbatimTextOutput('str')),
                           tabPanel('Data', tableOutput('data'))
               )
             ),
             
             tabPanel("Subsection 1.2", 
                      selectInput('var1', 
                                  'Select the X variable', 
                                  choices = c('Sepal.Length' = 1,
                                              'Sepal.Width' = 2,
                                              'Petal.Length' = 3,
                                              'Petal.Width' = 4)),
                      br(),
                      selectInput('var2', 
                                  'Select the Y variable', 
                                  choices = c('Sepal.Length' = 1,
                                              'Sepal.Width' = 2,
                                              'Petal.Length' = 3,
                                              'Petal.Width' = 4)),
                      br(),
                      radioButtons('var3', 
                                   'Select the file type', 
                                   choices = list('png','pdf'))
             )
    ),
    navlistPanel(
      "Section 2",
      tabPanel("Subsection 2.1", "Content for Subsection 2.1"),
      tabPanel("Subsection 2.2", "Content for Subsection 2.2")
    ),
    # mainPanel("Main content for Tab 1")  # Hauptinhalt für Tab 1
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
             tabPanel("Subsection 2.1", "Content for Subsection 2.1"),
             tabPanel("Subsection 2.2", "Content for Subsection 2.2")
           ),
           mainPanel("Main content for Tab 1")  # Hauptinhalt für Tab 1
  )
),
)


