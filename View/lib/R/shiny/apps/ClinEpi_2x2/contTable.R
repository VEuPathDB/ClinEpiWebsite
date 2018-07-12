contTable <- tabItem(tabName = "contTable", 
         fluidRow(
           box(width = 12, status = "primary", title = "Contingency Tables",
               uiOutput("table")
           )
         )
        )