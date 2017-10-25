## ui.R
require(plotly)

shinyUI(
  fluidPage(
    tags$head(tags$style(
      HTML(".js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: transparent}")
    )),
    titlePanel("Change in Observations Over Time"),
    div(
      id = "userInput",
      fluidRow(
        div(
          id = "timeframeInput",
          column(12, 
                 align = "center",
                 div(
                   id = "timeframeUI",
                   uiOutput("choose_timeframe")
                 )
          )
        )
      ),
      fluidRow(
        column(4,
               div(
                 id = "attributeUI",
                 uiOutput("choose_yaxis")
               )
        ),
        column(4,
               div(
                 id = "outcomeUI",
                 uiOutput("groups_type")
               )
        ),
        column(4,
               div(
                 id = "outcomeUI",
                 uiOutput("facet_type")
               )
        )
      ),
      fluidRow(
        column(4,
               uiOutput("yaxis_stp1")
               ),
        column(4,
               uiOutput("choose_groups")
        ),
        column(4,
               uiOutput("choose_facet")
        )
      ),
      fluidRow(
        column(4,
               uiOutput("yaxis_stp2")),
        column(4,
               uiOutput("groups_stp1")
        ),
        column(4,
               uiOutput("facet_stp1")
        )
      ),
      fluidRow(
        column(4),
        column(4,
               uiOutput("groups_stp2")
        ),
        column(4,
               uiOutput("facet_stp2")
        )
      ),
      fluidRow(
        column(4),
        column(4,
               uiOutput("groups_stp3")
        ),
        column(4,
               uiOutput("facet_stp3")
        )
      ),
      fluidRow(
        column(4),
        column(4,
               uiOutput("groups_stp4")
        ),
        column(4,
               uiOutput("facet_stp4")
        )
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
)
