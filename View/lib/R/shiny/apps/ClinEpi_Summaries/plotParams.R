plotParams <- tabItem(tabName = "plotParams", 
                fluidRow(
                  box(width = 12, status = "primary", title = "Timeline(s)",
                      timelineUI("timeline")
                  )
                ),
                fluidRow(
                  box(width = 6, status = "primary", title = "Unit of Analysis",
                      uiOutput("prtcpntViewSwitch")
                  ),
                  box(width = 6, status = "primary", title = "X-Axis",
                      uiOutput("xaxis_var"),
                      uiOutput("xaxis_stp2")
                  )
                ),
                fluidRow(
                  box(width = 6, status = "primary", title = "Y-Axis",
                      uiOutput("choose_yaxis"),
                      uiOutput("yaxis_stp1"),
                      uiOutput("yaxis_stp2"),
                      uiOutput("yaxis_stp3")
                 ),
                 box(width = 6, status = "primary", title = "Facet Line",
                     uiOutput("groups_type"),
                     customGroupsUI("group", colWidth = 12)
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