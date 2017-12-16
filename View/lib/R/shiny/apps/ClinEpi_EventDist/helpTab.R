help <- tabPanel("Help", fluid = TRUE,
          fluidPage(
            h4("Overview"),
            h5(
              p("This Shiny app allows you to look at the distribution of any variable in the dataset. There are several drop-down boxes at the top. The one in the top left applies only to longitudinal datasets and allows you to choose to view the distribution of multiple variables over time. The others allow you to set the X axis or facet the plot by any variable of interest. While faceting the plot, you can choose to make your own groups based on a selected variable or to use all possible groups for that variable. If the dataset is longitudinal, you will also see a timeline across the top of the application, allowing you to subset by time periods of interest. Whenever you click the 'Plot!' button, you will see the plot appear. If you update your selections in the menus above the 'Plot' button, you will need to click it again."),
              p("After clicking 'Plot' a figure will appear with one or more blue histograms. The dashed line(s) in the plot represent the mean for the selected value across the entire dataset. In this plot numeric values will be automatically binned along the X axis and will be automatically broken into 4 groups when faceting with the 'All possible' option. If you hover over any of the color shaded areas a tooltip will appear telling you the X axis value. For each of the bars in the histogram this will be the count, or number of participants in that bin. For the dashed line the tooltip will display the mean across the entire dataset."),
              p("It is also worth noting the panel that appears in the top right of the plot when you hover over any portion of it. This menu will allow you to pan and zoom the graph, to download the graph as an image and to change how tooltips are displayed among other things.") 
            ),
            br(),
            h4("Notes"),
            h5(
              p("It is important to note that this analysis tool uses the entire set of participants for the current dataset. In order to see how the subset of participants you returned in the participant query compare to the others in the dataset, you should select 'User Defined Group' when choosing a variable. The participants returned in the participant query belong to the group 'Selected' while all others belong to the group 'Not Selected'.")
            ),
            br(),
            h4("Version Information"),
            h5(
              p("Shiny Server: 1.5.6.875", br(),
                "R: 3.4.1 (2017-06-30)", br(),
                "Packages:", br(),
                tags$ul(
                  tags$li("data.table: 1.10.4-2"),
                  tags$li("DT: 0.2"),
                  tags$li("plotly: 4.7.1"),
                  tags$li("shiny: 1.0.5"),
                  tags$li("viridisLite: 0.2.0")
                )
              )
            )
          ) 
        )
