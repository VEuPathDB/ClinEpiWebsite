## ui.R
require(plotly)

shinyUI(
  fluidPage(
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              "#distribution{height:90vh !important;}"
   ),
   uiOutput("title"),
   tabsetPanel(
     tabPanel("Plot", fluid = TRUE,
       fluidPage(
         h5(),
         fluidRow(
           column(12, 
                  align = "center",
                  timelineUI("timeline")
           )
         ),
         fluidRow(
           column(3,
                  selectInput(inputId = "plotChoice",
                              label = "Choose plot to generate:",
                              choices = list('Single Variable' = 'singleVar', 'Multi Variable' = 'groups'),
                              selected = "singleVar"),
                  uiOutput("choose_groups")
           ),
           column(4, offset = 1,
                  column(12,
                         fluidRow(
                           uiOutput("choose_xaxis")
                         )
                  ),
                  column(12,
                         fluidRow(
                           customGroupsUI("group", colWidth = 12)
                         )
                  )
                  #uiOutput("choose_range")
           ),
           column(4,
                  column(12,
                         fluidRow(
                           uiOutput("facet_type")
                         )
                  ),
                  column(12,
                         fluidRow(
                           customGroupsUI("facet", colWidth = 12)
                         )
                  )
           )
         ),
         fluidRow(
                   column(12, align = "center",
                          actionButton("btn", "Plot!",
                                       style='padding:8px; font-size:115%; color:white; background-color:#366dc4',
                                       width = '15%')
                   )
         ), 
         hr(),
         plotlyOutput("distribution")
      )
    ),
    tabPanel("Notes", fluid = TRUE,
      fluidPage(
        h4("Tutorial"),
        h5("some stuff"),
        h4("Notes"),
        h5("some more stuff"),
        h4("References"),
        h5("the rest of the stuff")
      ) 
    )
   )
  )
 )

