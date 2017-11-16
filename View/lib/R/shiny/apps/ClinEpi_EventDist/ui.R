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
   tabsetPanel(
     tabPanel("Plot", fluid = TRUE,
       fluidPage(
         h5(),
         fluidRow(
           column(3,
                  selectInput(inputId = "plotChoice",
                              label = "Choose plot to generate:",
                              choices = list('Single Variable' = 'singleVar'), 'Multi Variable' = 'groups'),
                              selected = "singleVar"),
                  uiOutput("choose_groups")
           ),
           column(4, offset = 1,
                  uiOutput("choose_xaxis"),
                  uiOutput("choose_range")
           ),
           column(4,
                  uiOutput("choose_facet")
           )
         ), 
         hr(),
         plotlyOutput("distribution")
      )
    ),
    tabPanel("Notes", fluid = TRUE,
      fluidPage(
        h4("Coming soon...")
      ) 
    )
  )
)

