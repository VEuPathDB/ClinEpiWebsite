plot <-      tabPanel("Plot", fluid = TRUE,
               fluidPage(
                 h5(),
                 fluidRow(
                   column(12, 
                          align = "center",
                          timelineUI("timeline")
                   )
                 ),
                 fluidRow(
                   column(4,
                          column(12,
                                 fluidRow(
                                   uiOutput("xaxis_var")
                                 ),
                                 fluidRow(
                                   uiOutput("choose_yaxis")
                                 ),
                                 fluidRow(
                                   uiOutput("yaxis_stp1")
                                 ),
                                 fluidRow(
                                   uiOutput("yaxis_stp2")
                                 )
                          ) 
                   ),
                   column(4,
                          column(12,
                                 fluidRow(
                                   uiOutput("groups_type")
                                 )
                          ),
                          column(12,
                                  fluidRow(
                                    customGroupsUI("group", colWidth = 12)
                                 )
                          )
                   ),
                   column(4,
                          column(12,
                                 fluidRow(
                                   uiOutput("facet_type")
                                 )
                          ),
                          column(12,
                                 fluidRow(
                                   customGroupsUI("facet", colWidth = 12)
                                 )
                          )
                   )
                 ),
                 hr(),
                 div(
                   id = "plot_area",
                   DT::dataTableOutput("table"),
                   hr(),
                   plotlyOutput("plot", width = '100%', height = '1000px')
                   
                 ) 
               )
      )
