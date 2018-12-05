help <- tabItem(tabName = "help", 
            h4("Overview"),
            #h5(
              p("This Shiny app allows you to look at the association between any two variables that are found in the dataset. There is a sidebar which allows you to choose between the various tabs, and a menu button that allows you to toggle the sidebar. On the first tab there are several boxes which allow you to set various plot parmeters. Notice here that there are drop-down menus that allow you to choose what variables you are comparing. There are also tabs for displaying various tables and bar graphs. Every time you change your selection in the drop-down menus, the two tables and plot will automatically update. Variable 1 defines the rows in the second table and Variable 2 defines the columns. If the dataset is longitudinal, you will also see at least one timeline across the top of the application, allowing you to filter by time periods and/or age groups of interest."),
              p("In the 'Summary Stats' tab, you will find some basic statistics including a p-value, Odds Ratio and Relative Risk. For the Odds Ratio and Relative Risk, the 95% confidence interval is also included."),
              p("In the 'Contingency Tables' tab, you can see the numbers within the table indicate the number of participants that meet the defined criteria. For example, the value in the top left corner of this table represents the number of participants for which both of your selected variables are positive/true. Meanwhile, the value in the bottom right corner represents the number of participants negative for both variables."),
              p("The figures in green and blue on either the 'Plot Grid' or 'Individual Plots' tab are a visual representation of the proportion of participants per column of the second table described here. The blue box on the left hand side corresponds to the the top left corner of the table, and the green box on the right corresponds to the bottom right corner of the table. If you hover over any of the color shaded areas a tooltip will appear telling you the attributes that you have specified and what proportion of participants meet this group for the given column."),
              p("It is also worth noting the panel that appears in the top right of the plot when you hover over any portion of it. This menu will allow you to pan and zoom the graph, to download the graph as an image and to change how tooltips are displayed among other things."),
            #),
            br(),
            h4("Notes"),
            #h5(
              p("It is important to note that while this analysis tool can be reached through either the participant or observation query, it always uses the entire set of participants for the current dataset. If you used the participant query to reach the application, then in order to see how the subset of participants you returned in the participant query compare to the others in the dataset, you should select 'Participant Search Results' when choosing a variable. The participants returned in the participant query belong to the group 'Selected' while all others belong to the group 'Not Selected'. If, however, you were working in the observations query, then you should similarly select 'Observation Search Results'. You can type 'Search Results' into the drop-down menu when selecting a variable to see variables which were passed through as results from the query. These are often the same as those columns which appear in the table under the 'Results' tab (left-most tab, before your open analyses)."),
             p("When using observations to define your variables, you will be prompted to select whether you would like to select participants with either any or all of their associated observations matching the selected value. In the first case, any participant which has at least one observation among the group of selected observations will be considered part of the first (or positive) group. Only those participants without a single observation among the group of selected observations will appear in the second group. Alternatively, if you choose to match participants based on all observations (which appears as the term 'always' on screen), then the first group will contain only those participants where every observation is within your group of selected observations."),
             p("It is possible to select observational data in the X axis and Facet drop-down menus. In this case, it should be remembered that each participant has a number of observations and so will have contributed multiple data points to the histrogram."),
              p("The p-value is calculated using Pearson's Chi Square test. As a result, this statistic may be unreliable for small sample sizes. This value represents the probability that the two selected variables are randomly vs. significantly associated. Typically, we consider a p-value below 0.05 to indicate confidence that the two variables are significantly associated."),
              p("The Odds Ratio measures the magnitude and direction of the association of the two selected variables. A value above 1 indicates that the two variables are positively associated, or that the presence of Variable 1 increases the odds of also seeing Variable 2. It is calculated as the ratio of the odds of seeing Variable 1 given Variable 2."),
              p("The Relative Risk represents the likelihood of Variable 1 occuring given Variable 2. So a value of 20 would indicate that we are 20 times more likely to see Variable 1 occur in the group positive for Variable 2. It is calculated as the ratio of the probability that Variable 1 will be seen in the group positive for Variable 2 to the probability that Variable 1 will be seen in the group negative for Variable 2. "), 
              br(),
              p("So given a table such as the following:"),
              tags$table(
                tags$tr(
                  tags$th(""),
                  tags$th("Outcome +"),
                  tags$th("Outcome -")
                 ),
                 tags$tr(
                   tags$td("Exposure +"),
                   tags$td("A"),
                   tags$td("B")                               
                 ),
                 tags$tr(
                   tags$td("Exposure -"),
                   tags$td("C"),
                   tags$td("D")
		 )
               ),
               br(),
               p("The equation to compute the Odds Ratio would be:"),
               p(withMathJax("$$OR=\\frac{A/C}{B/D}=AD/BC$$")),
               p("While the equation to compute the Relative Risk would be:"),
               p(withMathJax("$$RR=\\frac{A/(A+B)}{C/(C+D)}$$")),
            # ),
             br(),
             h4("Version Information"),
             #h5(
               p("Shiny Server: 1.5.6.875", br(),
                 "R: 3.5.1 (2018-07-02)", br(),
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
