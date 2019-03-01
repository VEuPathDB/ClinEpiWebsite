plotParams <- tabItem(tabName = "plotParams", 
                fluidRow(
                  timelineUI("timeline")
                ),
                fluidRow(
                  uiOutput("prtcpntViewSwitch"),
                  uiOutput("xaxisBox")
                ),
                fluidRow(
                  box(width = 6, status = "primary", title = "Y-Axis",
                      uiOutput("choose_yaxis"),
                      uiOutput("yaxis_stp1"),
                      uiOutput("yaxis_stp2"),
                      uiOutput("yaxis_stp3")
                  ),
                  uiOutput("groupBox")
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
