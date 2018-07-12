body <- dashboardBody(
          tabItems(
            # First tab content
            plotParams,
            summaryStats,
            contTable,
            plotGrid,
            individualPlot,
            # Second tab content
            help
          )
        )