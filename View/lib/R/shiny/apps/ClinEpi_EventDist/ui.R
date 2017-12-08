## ui.R

shinyUI(
  fluidPage(
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

