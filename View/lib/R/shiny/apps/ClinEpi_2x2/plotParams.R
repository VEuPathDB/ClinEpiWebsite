plotParams <- tabItem(tabName = "plotParams", 
                fluidRow(
                  box(width = 12, status = "primary", title = "Timeline(s)",
                      timelineUI("timeline")
                  )
                ),
                fluidRow(
                  box(width = 6, status = "primary", title = "Unit of Analysis",
                      uiOutput("prtcpntViewSwitch")
                  )
                )  ,
                fluidRow(
                  box(width = 6, status = "primary", title = "Variable 1",
                      customGroupsUI("attr", colWidth = 12)
                  ),
                  box(width = 6, status = "primary", title = "Variable 2",
                      customGroupsUI("out", colWidth = 12)
                  )
                ),
                fluidRow(
                  box(width = 6, status = "primary", title = "Facet Plot (1)",
                      uiOutput("facet_type"),
                      customGroupsUI("facet", colWidth = 12)
                  ),
                  box(width = 6, status = "primary", title = "Facet Plot (2)",
                      uiOutput("facet2_type"),
                      customGroupsUI("facet2", colWidth = 12)
                  )
                )
)