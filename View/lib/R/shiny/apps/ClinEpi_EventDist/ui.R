## ui.R
require(plotly)

shinyUI(
  fluidPage(
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              "#distribution{height:90vh !important;}"
   ),
   titlePanel("Observation Distributions for Selected Participants"),
   fluidRow(
     div(
       id = "userInput",
       column(3,
              div(
                id = "plotTypeUI",
                selectInput(inputId = "plotChoice",
                            label = "Choose plot to generate:",
                            choices = list('Single Variable' = 'singleVar', 'Multi Variable' = 'groups'),
                            selected = "singleVar")
              ),
              div(
                id = "groupsUI",
                uiOutput("choose_groups")
              )
       ),
       column(4, offset = 1,
              div(
                id = "xUI",
                uiOutput("choose_xaxis")
              ),
              div(
                id = "rangeUI",
                uiOutput("choose_range")
              )
       ),
       column(4,
              div(
                id = "facetUI",
                uiOutput("choose_facet")
              )
      )
    )
   ), 
   hr(),
   div(
     id = "plot_area",
     plotlyOutput("distribution")
   )      
    
  )
)
