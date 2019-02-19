body <- dashboardBody(
          tabItems(
            # First tab content
            plotParams,
            summaryStats,
            plotGrid,
            individualPlot,
            # Second tab content
            help
          )
        )