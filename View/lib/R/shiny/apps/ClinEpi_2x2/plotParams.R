plotParams <- tabItem(tabName = "plotParams", 
                fluidRow(
                      timelineUI("timeline")
                ),
                fluidRow(
                      uiOutput("prtcpntViewSwitch")
                ),
                fluidRow(
                  box(width = 6, status = "primary", title = "Variable 1",
                      customGroupsUI("attr", colWidth = 12)
                  ),
                  box(width = 6, status = "primary", title = "Variable 2",
                      customGroupsUI("out", colWidth = 12)
                  )
                ),
                fluidRow(
                  box(width = 6, status = "primary", title = "Stratify Plot (1)",
                      uiOutput("facet_type"),
                      customGroupsUI("facet", colWidth = 12)
                  ),
                  box(width = 6, status = "primary", title = "Stratify Plot (2)",
                      uiOutput("facet2_type"),
                      customGroupsUI("facet2", colWidth = 12)
                  )
                )
)
