plotGrid <- tabItem(tabName = "plotGrid", 
                      fluidRow(
                        box(width = 12, status = "primary", title = "Plot",
                            withSpinner(plotlyOutput("plot",  height="auto", width="auto", inline=T), type=5) 
                        )
                      )
)
