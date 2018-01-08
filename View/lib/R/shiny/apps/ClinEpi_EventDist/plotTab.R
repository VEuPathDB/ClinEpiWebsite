plot <-  tabPanel("Plot", fluid = TRUE,
           fluidPage(
             h5(),
             fluidRow(
               column(12, 
                      align = "center",
                      timelineUI("timeline")
               )
             ),
             fluidRow(
               column(3,
                      selectInput(inputId = "plotChoice",
                                  label = "Choose plot to generate:",
                                  choices = list('Single Variable' = 'singleVar', 'Multi Variable' = 'groups'),
                                  selected = "singleVar"),
                      uiOutput("choose_groups")
               ),
               column(4, offset = 1,
                      column(12,
                             fluidRow(
                               uiOutput("choose_xaxis")
                             )
                      ),
                      column(12,
                             fluidRow(
                               customGroupsUI("group", colWidth = 12)
                             )
                      )
                      #uiOutput("choose_range")
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
             plotlyOutput("distribution")
          )
        )
