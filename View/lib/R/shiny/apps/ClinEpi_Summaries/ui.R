## ui.R
require(plotly)

shinyUI(
  fluidPage(
    tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$head(tags$style(
      HTML(".js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: transparent}")
    )),
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
                   column(4,
                          column(12,
                                 fluidRow(
                                   uiOutput("choose_yaxis")
                                 ),
                                 fluidRow(
                                   uiOutput("yaxis_stp1")
                                 ),
                                 fluidRow(
                                   uiOutput("yaxis_stp2")
                                 )
                          )
                   ),
                   column(4,
                          column(12,
                                 fluidRow(
                                   uiOutput("groups_type")
                                 )
                          ),
                          column(12,
                                  fluidRow(
                                    customGroupsUI("group", colWidth = 12)
                                  )
                          )
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
                 div(
                   id = "plot_area",
                   DT::dataTableOutput("table"),
                   hr(),
                   plotlyOutput("plot", width = '100%', height = '1000px')
                   
                 ) 
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

