help <- tabPanel("Help", fluid = TRUE,
          fluidPage(
            h4("Overview"),
            #h5(
              p("This Shiny app allows you to look at the association between any two variables that are found in the dataset. Notice that there are drop-down menus that allow you to choose what variables you are comparing. Below that, after a moment you will also see that two tables and a bar graph appear. Every time you change your selection in the drop-down menus, the two tables and plot will automatically update. Variable 1 defines the rows in the second table and Variable 2 defines the columns. If the dataset is longitudinal, you will also see a timeline across the top of the application, allowing you to subset by time periods of interest."),
              p("In the first table below, you will find some basic statistics including a p-value, Odds Ratio and Relative Risk. For the Odds Ratio and Relative Risk, the 95% confidence interval is also included."),
              p(" In the second table below the drop-down boxes to select your variables, you can see the numbers within the table indicate the number of participants that meet the defined criteria. For example, the value in the top left corner of this table represents the number of participants for which both of your selected variables are positive/true. Meanwhile, the value in the bottom right corner represents the number of participants negative for both variables."),
              p("The figure in green and blue below is a visual representation of the proportion of participants per column of the second table. The blue box on the left hand side corresponds to the the top left corner of the table, and the green box on the right corresponds to the bottom right corner of the table. If you hover over any of the color shaded areas a tooltip will appear telling you the attributes that you have specified and what proportion of participants meet this group for the given column."),
              p("It is also worth noting the panel that appears in the top right of the plot when you hover over any portion of it. This menu will allow you to pan and zoom the graph, to download the graph as an image and to change how tooltips are displayed among other things."),
            #),
            br(),
            h4("Notes"),
            #h5(
              p("It is important to note that while this analysis tool can be reached through either the participant or observation query, it always uses the entire set of participants for the current dataset. If you used the participant query to reach the application, then in order to see how the subset of participants you returned in the participant query compare to the others in the dataset, you should select 'User Defined Participants' when choosing a variable. The participants returned in the participant query belong to the group 'Selected' while all others belong to the group 'Not Selected'. If, however, you were working in the observations query, then you should similarly select 'User Defined Observations'. It is important to note in this case that any participant which has at least one observation among the group of returned observations will be in the 'Selected' group. Only those participants without a single observation among the group of observations returned from the query will appear in the 'Not Selected' group."),
              p("The p-value is calculated using Pearson's Chi Square test. As a result, this statistic may be unreliable for small sample sizes. This value represents the probability that the two selected variables are randomly vs. significantly associated. Typically, we consider a p-value below 0.05 to indicate confidence that the two variables are significantly associated."),
              p("The Odds Ratio measures the magnitude and direction of the association of the two selected variables. A value above 1 indicates that the two variables are positively associated, or that the presence of Variable 1 increases the odds of also seeing Variable 2. It is calculated as the ratio of the odds of seeing Variable 1 given Variable 2."),
              p("The Relative Risk represents the likelihood of Variable 1 occuring given Variable 2. So a value of 20 would indicate that we are 20 times more likely to see Variable 1 occur in the group positive for Variable 2. It is calculated as the ratio of the probability that Variable 1 will be seen in the group positive for Variable 2 to the probability that Variable 1 will be seen in the group negative for Variable 2. "), 
              br(),
              p("So given a table such as the following:"),
              tags$table(
                tags$tr(
                  tags$th(""),
                  tags$th("Var 2 +"),
                  tags$th("Var 2 -")
                 ),
                 tags$tr(
                   tags$td("Var 1 +"),
                   tags$td("A"),
                   tags$td("B")                               
                 ),
                 tags$tr(
                   tags$td("Var 1 -"),
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
             #)
           )
       )
