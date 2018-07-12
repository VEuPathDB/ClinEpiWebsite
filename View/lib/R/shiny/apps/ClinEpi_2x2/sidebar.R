sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Plot Parameters", tabName = "plotParams", icon = icon("check-square")),
    menuItem("Summary Statistics", tabName = "summaryStats", icon = icon("table")),
    menuItem("Contingency Tables", tabName = "contTable", icon = icon("table")),
    menuItem("Plot Grid", tabName = "plotGrid", icon = icon("th")),
    menuItem("Individual Plots", tabName = "individualPlot", icon = icon("bar-chart")),
    menuItem("Help", tabName = "help", icon = icon("question-circle"))
  )
)