plotGrid <- tabItem(tabName = "plotGrid", 
                      fluidRow(
                        box(width = 12, status = "primary", title = "Plot",
                            plotlyOutput("plot") 
                        )
                      )
)