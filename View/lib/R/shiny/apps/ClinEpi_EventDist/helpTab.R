help <- tabItem(tabName = "help",
            h4("Overview"),
            #h5(
              p("This Shiny app allows you to look at the distribution of any variable in the dataset. There is a sidebar which allows you to choose between the various tabs, and a menu button that allows you to toggle the sidebar. On the first tab there are several boxes which allow you to set various plot parmeters, such as the x axis or facets for the plot. While faceting the plot, you can choose to make your own groups based on a selected variable or to use all possible groups for that variable. If the dataset is longitudinal (on a continuous variable), you will also see at least one timeline across the top of the application, allowing you to filter by time periods and/or age groups of interest. In the various other tabs, you will see a table and the plot(s) appear. The table will indicate the number of participants belonging to each facet, and if the X axis variable you selected is numeric, will also provide some summary information on that facet. If you update your selections in the menus the table and plot will both update automatically."),
              p("The vertical dashed line(s) in the plot represent the mean for the selected value across the entire dataset for numeric X axis selections. In this plot numeric values will be automatically binned along the X axis and will be automatically broken into 4 groups when faceting with the 'All possible' option. If you hover over any of the color shaded areas a tooltip will appear telling you the X axis value. For each of the bars in the histogram this will be the count, or number of participants or observations in that bin. For the dashed line the tooltip will display the mean across the entire dataset, which can be compared to the mean for the individual facets in the table above."),
              p("It is also worth noting the panel that appears in the top right of the plot when you hover over any portion of it. This menu will allow you to pan and zoom the graph, to download the graph as an image and to change how tooltips are displayed among other things."),
            #),
            br(),
            h4("Notes"),
            #h5(
              p("It is important to note that while this analysis tool can be reached through either the participant or observation query, it always uses the entire set of participants for the current dataset. If you used the participant query to reach the application, then in order to see how the subset of participants you returned in the participant query compare to the others in the dataset, you should select 'Participant Search Results' when choosing a variable. The participants returned in the participant query belong to the group 'Selected' while all others belong to the group 'Not Selected'. If, however, you were working in the observations query, then you should similarly select 'Observation Search Results'. You can type 'Search Results' into the drop-down menu when selecting a variable to see variables which were passed through as results from the query. These are often the same as those columns which appear in the table under the 'Results' tab (left-most tab, before your open analyses)."),
             p("When selecting observations to 'Make your own' facets by, you will be prompted to select whether you would like to select participants with either any or all of their associated observations matching the selected value. In the first case, any participant which has at least one observation among the group of selected observations will be considered part of your custom facet. Only those participants without a single observation among the group of selected observations will appear in the second facet. Alternatively, if you choose to match participants based on all observations (which appears as the term 'always' on screen), then the first facet will contain only those participants where every observation is within your group of selected observations."),
             p("If you came to the analysis tool through the observation query then you will additionally be able to toggle the 'Unit of Analysis' plot parameter between 'Participant View' and 'Observation View'. Selecting 'Participant View' will aggregate data points based on which participant they relate to and will cause the app to work the same as it would if you had instead gone through the participant query."),
             p("It is possible to select observational data in the X axis and Facet drop-down menus. In this case, it should be remembered that each participant has a number of observations and so will have contributed multiple data points to the histrogram."),
            #),
            br(),
            h4("Version Information"),
            #h5(
              p("Shiny Server: 1.5.6.875", br(),
                "R: 3.5.0 (2018-04-23)", br(),
                "Packages:", br(),
                tags$ul(
                  tags$li("data.table: 1.11.4"),
                  tags$li("DT: 0.4"),
                  tags$li("ggplot2: 2.2.2.1"),
                  tags$li("httr: 1.3.1"),
                  tags$li("plotly: 4.7.1"),
                  tags$li("shiny: 1.1.0"),
                  tags$li("shinydashboard: 0.7.0"),
                  tags$li("shinyjs: 1.0"),
                  tags$li("shinyTree: 0.2.2"),
                  tags$li("tidyr: 0.8.1"),
                  tags$li("viridisLite: 0.3.0")
                )
          ) 
        )
