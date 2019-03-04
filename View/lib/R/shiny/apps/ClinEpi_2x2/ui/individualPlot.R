individualPlot <- tabItem(tabName = "individualPlot", 
                      fluidRow(
                        box(width = 12, status = "primary", title = "Plot",
                            fluidRow(
                              column(6,
                                     uiOutput("individualPlot_stp1")
                                     ),
                              column(6,
                                     uiOutput("individualPlot_stp2")
                              )
                            ),
                            plotlyOutput("individual_plot",  height="auto", width="auto", inline=T) 
                        )
                      )
)