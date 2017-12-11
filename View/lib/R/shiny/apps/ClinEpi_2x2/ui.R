## ui.R

shinyUI(
  fluidPage(
    tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              "table, th, td { border: 1px solid black; padding: 15px;}"
    ),
    tags$head(tags$style(
      HTML("input[type='search']:disabled {visibility:hidden}"),
      HTML(".js-irs .irs-bar-edge, .js-irs .irs-bar {background: transparent}")
    )),
    uiOutput("title"),
    tabsetPanel(
                plot,
                help
    )
  )
)

