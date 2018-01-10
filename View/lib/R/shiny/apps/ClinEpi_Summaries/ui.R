## ui.R

shinyUI(
  fluidPage(
   theme = "cerulean.css",
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              ".dataTable {border: none;}"
   ),
   tags$head(tags$style(
     HTML(".js-irs-0 .irs-line-left, .js-irs-0 .irs-line-right {background: transparent}"
   )),
   uiOutput("title"),
   tabsetPanel(
     plot,
     help
   )
  )
 )

