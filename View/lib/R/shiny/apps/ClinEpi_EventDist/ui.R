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
              "#distribution{height:75vh !important;}"
   ),
   tags$head(tags$script('var dimension = [0, 0];
                         $(document).on("shiny:connected", function(e) {
                         dimension[0] = window.innerWidth;
                         dimension[1] = window.innerHeight;
                         Shiny.onInputChange("dimension", dimension);
                         });
                         $(window).resize(function(e) {
                         dimension[0] = window.innerWidth;
                         dimension[1] = window.innerHeight;
                         Shiny.onInputChange("dimension", dimension);
                         });'
   )),
   titlePanel("Observations Distribution for Selected Participants"),
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
                #h5("Groups:"),
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
     htmlOutput("distribution")
   )      
    
  )
)
