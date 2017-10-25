## ui.R
require(plotly)

shinyUI(
  fluidPage(
   tags$head(tags$style(
      HTML("input[type='search']:disabled {visibility:hidden}"),
      HTML(".js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: transparent}"),
      HTML(".js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: transparent}")
    )),
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              "#2x2{height:90vh !important;}"
   ),
   titlePanel("Contingency Tables for Selected Participants"),
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
         column(6,
                div(
                  id = "attributeUI",
                  uiOutput("choose_attribute")
                )
         ),
         column(6,
                div(
                  id = "outcomeUI",
                  uiOutput("choose_outcome")
                )
         )
     ),
     fluidRow(
       column(6,
              div(
                id = "attr_stp1",
                uiOutput("attr_stp1")
              )
       ),
       column(6,
              div(
                id = "out_stp1",
                uiOutput("out_stp1")
              )
       )
     ),
     fluidRow(
       column(6,
              div(
                id = "attr_stp2",
                uiOutput("attr_stp2")
              )
       ),
       column(6,
              div(
                id = "out_stp2",
                uiOutput("out_stp2")
              )
       )
     )
   ), 
   fluidRow(
     column(6,
            div(
              id = "attr_stp3",
              uiOutput("attr_stp3")
            )
     ),
     column(6,
            div(
              id = "out_stp3",
              uiOutput("out_stp3")
            )
     )
   ),
   fluidRow(
     column(6,
            div(
              id = "attr_stp4",
              uiOutput("attr_stp4")
            )
     ),
     column(6,
            div(
              id = "out_stp4",
              uiOutput("out_stp4")
            )           
     )
   ),
   hr(),
   div(
     id = "plot_area",
     fluidRow(
       column(12,
              DT::dataTableOutput("table"),
      # ),
      # column(6,
              DT::dataTableOutput("statsTable"),
              helpText("*Pearson's Chi-Square test applied: small sample sizes result in unreliable p-values.")      
       )
     ),
     hr(),
     plotlyOutput("plot")
   )            
  )
)
