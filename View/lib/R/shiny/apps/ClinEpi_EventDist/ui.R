## ui.R

shinyUI(
  fluidPage(
   theme = "cerulean.css",
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              "#distribution{height:90vh !important;}"
   ),
   uiOutput("title"),
   tabsetPanel(
     plot,
     help
   )
  )
 )

