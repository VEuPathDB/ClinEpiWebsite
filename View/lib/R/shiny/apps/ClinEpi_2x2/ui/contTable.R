contTable <- tabItem(tabName = "contTable", 
         fluidRow(
           box(width = 12, status = "primary", title = "Contingency Tables",
               withSpinner(uiOutput("table"), type=5)
           )
         )
        )
