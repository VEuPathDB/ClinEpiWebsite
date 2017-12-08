plot <- tabPanel("Plot", fluid = TRUE,
          fluidPage(
            h5(),
              fluidRow(
                column(12, 
                       align = "center",
                       timelineUI("timeline")
                )
              ),
              fluidRow(
                column(6, align = "center",
                       customGroupsUI("attr", colWidth = 12)
                ),
                column(6, align = "center",
                       customGroupsUI("out", colWidth = 12)
                )
              ),
              fluidRow(
                column(12, align = "center",
                       actionButton("btn", "Plot!",
                                    style='padding:8px; font-size:115%; color:white; background-color:#366dc4',
                                    width = '15%'
                       )
                )
              ),
              hr(),
              fluidRow(
                column(12,
                       DT::dataTableOutput("statsTable"),
                       DT::dataTableOutput("table"),
                       helpText("*Pearson's Chi-Square test applied: small sample sizes result in unreliable p-values.")
                )
              ),
              hr(),
              plotlyOutput("plot")
          ) 
        )
