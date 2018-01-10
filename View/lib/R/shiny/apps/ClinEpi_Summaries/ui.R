## ui.R

shinyUI(
  fluidPage(
   theme = "cerulean.css",
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
   ),
   tags$head(tags$style(
      HTML(".js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: transparent}")
   )),
   uiOutput("title"),
   tabsetPanel(
     plot,
     help
   )
  )
 )

