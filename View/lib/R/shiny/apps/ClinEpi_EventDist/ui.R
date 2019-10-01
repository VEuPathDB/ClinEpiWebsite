## ui.R

shinyUI(
  fluidPage(
   useShinyjs(),
   extendShinyjs(text="shinyjs.virtualBodyClick = function(){$(\"body\").trigger(\"click\")}", functions=c("virtualBodyClick")),
   theme = "cerulean.css",
   
   tags$script(inactivity),
   tags$style(type="text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }",
              "#distribution{height:auto !important;}",
              ".dataTable {border: none;}"
   ),
   tags$style(type = 'text/css', ".btn-default{width: 100%;}"),
   tags$style(type = 'text/css', ".btn .caret{position: relative; color: black; border-top: 5px solid; border-left: 5px solid transparent; border-right: 5px solid transparent;}"),
   tags$style(type = 'text/css', ".caret{position: absolute; top: 50%; margin-top: 8px; float: right}"),
   tags$style(".treeContainer { height: 400px; overflow-y: scroll; }"),
   tags$style(".treeContainer { width: 350px; overflow-x: scroll; }"),
   tags$style(type = "text/css", ".jstree-default .jstree-search { color: #428bca; }"),
   tags$style(type = 'text/css', ".btn, .btn:hover {background: #ffffff; text-align:left; border-color: #cccccc; padding: 6px 12px;}"),
   tags$head(includeScript("../../functions/jstree.js")),
   tags$head(tags$style(
     HTML("div#ss-connect-dialog {width: 60em; bottom: 15em; left: 0;}"),
     HTML("div.modal-backdrop {display:none;}"),
     HTML("div#shiny-disconnected-overlay {display:none;}"),
     HTML("div#ss-overlay {z-index: 1;}"),
     HTML("a#ss-reload-link {visibility:hidden;}"),
     HTML("a#ss-reload-link::after {visibility:visible;content: 'Click to Reload ONLY if there was a Timeout. Otherwise please contact us with your selections.';}"),
     HTML(".js-irs-0 .irs-line-left {background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);}"),
     HTML(".js-irs-0 .irs-line-right {background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);}"),
     HTML(".js-irs-0 .irs-line-mid {background: linear-gradient(to bottom, #DDD -50%, #FFF 150%); border: 0px;}"),
     HTML(".js-irs-0 .irs-bar {background: #428bca;
                               border-top: 1px solid #CCC;
                               border-bottom: 1px solid #CCC;}"),
     HTML(".js-irs-0 .irs-bar-edge {background: inherit; border: inherit;}"), 
     HTML(".js-irs-1 .irs-line-left {background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);}"),
     HTML(".js-irs-1 .irs-line-right {background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);}"),
     HTML(".js-irs-1 .irs-line-mid {background: linear-gradient(to bottom, #DDD -50%, #FFF 150%); border: 0px;}"),
     HTML(".js-irs-1 .irs-bar {background: #428bca;
                               border-top: 1px solid #CCC;
                               border-bottom: 1px solid #CCC;}"),
     HTML(".js-irs-1 .irs-bar-edge {background: inherit; border: inherit;}")
   )),
   tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
     dashboardPage(
     dashboardHeader(title = textOutput("title")),
     sidebar,
     body
   )
  )
 )

