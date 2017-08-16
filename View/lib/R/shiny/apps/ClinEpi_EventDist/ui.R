## ui.R
#require(rCharts)
#require(ggplot2)
require(shinyjs)
#options(RCHART_LIB = 'polycharts')

shinyUI(
  fluidPage(
   useShinyjs(),
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              ".container-fluid {  max-width: 1260px; }",
              ".container-fluid {  max-height: 1260px; }"
   ),
   titlePanel("Events Distribution for Selected Participants"),
   sidebarLayout(
       sidebarPanel(
         div(
           id = "userInput",
           div(
             id = "plotTypeUI",
             selectInput(inputId = "plotChoice",
                         label = "Choose plot to generate:",
                         choices = list('Single Variable' = 'singleVar', 'Groups' = 'groups'),
                         selected = "singleVar")
           ),
           div(
             id = "xUI",
             uiOutput("choose_xaxis")
           ),
           #hidden(
           div(
             id = "facetUI",
             uiOutput("choose_facet")
           ),
           div(
             id = "groupsUI",
             uiOutput("choose_groups")
           )
           # )
         )
         # div(
         #     id = "plot_loading",
         #     h5("Loading plot...")
         # ),
         # hidden(
       ),
       mainPanel(
         div(
           id = "plot_area",
             plotOutput("distribution",  width = '100%')
            #style = 'width:950px;',
            #style = 'height:950px;'
         )
         #  )
       )
   )
    
  )
)
