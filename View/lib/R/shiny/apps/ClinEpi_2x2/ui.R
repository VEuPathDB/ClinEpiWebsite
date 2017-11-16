## ui.R
require(plotly)

shinyUI(
  fluidPage(
    tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
    ),
    tags$head(tags$style(
      HTML("input[type='search']:disabled {visibility:hidden}"),
      HTML(".js-irs .irs-bar-edge, .js-irs .irs-bar {background: transparent}")
      #HTML(".js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: blue}")
    )),
    titlePanel("Contingency Tables for Selected Participants"),
    tabsetPanel(
      tabPanel("Plot", fluid = TRUE,
               fluidPage(
                 h5(),
                 fluidRow(
                   column(12, 
                            align = "center",
                            uiOutput("choose_timeframe")
                   )
                 ),
                 fluidRow(
                   column(6,
                     uiOutput("choose_attribute")
                   ),
                   column(6,
                     uiOutput("choose_outcome")
                   )
                 ),
                 fluidRow(
                   column(6,
                     uiOutput("attr_stp1")
                   ),
                   column(6,
                     uiOutput("out_stp1")
                   )
                 ),
                 fluidRow(
                   column(6,
                     uiOutput("attr_stp2")
                   ),
                   column(6,
                     uiOutput("out_stp2")
                   )
                 ), 
                 fluidRow(
                   column(6,
                     uiOutput("attr_stp3")
                   ),
                   column(6,
                     uiOutput("out_stp3")
                   )
                 ),
                 fluidRow(
                   column(6,
                     uiOutput("attr_stp4")
                   ),
                   column(6,
                     uiOutput("out_stp4")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(12,
                          DT::dataTableOutput("statsTable"),
                          DT::dataTableOutput("table"),
                          helpText("*Pearson's Chi-Square test applied: small sample sizes result in unreliable p-values.")
                   )
                 ),
                 hr(),
                 plotlyOutput("plot")
               ) 
      ),
      tabPanel("Notes", fluid = TRUE,
               fluidPage(
                 h4("Coming soon...")
               )
      )
    )
  )
)

