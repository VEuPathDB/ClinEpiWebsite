## ui.R

shinyUI(
  fluidPage(
   theme = "cerulean.css",
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              "#distribution{height:90vh !important;}"
   ),
   tags$head(tags$style(
     HTML(".js-irs-0 .irs-line-left {background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);}"),
     HTML(".js-irs-0 .irs-line-right {background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);}"),
     HTML(".js-irs-0 .irs-line-mid {background: linear-gradient(to bottom, #DDD -50%, #FFF 150%); border: 0px;}"),
     HTML(".js-irs-0 .irs-bar {background: #428bca;
                               border-top: 1px solid #CCC;
                               border-bottom: 1px solid #CCC;}"),
     HTML(".js-irs-0 .irs-bar-edge {background: inherit; border: inherit;}") 
   )),
   uiOutput("title"),
   tabsetPanel(
     plot,
     help
   )
  )
 )

