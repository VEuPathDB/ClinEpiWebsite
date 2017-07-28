## ui.R
require(rCharts)
#options(RCHART_LIB = 'polycharts')

shinyUI(pageWithSidebar(
  headerPanel("Visualizing participant information"),
  
  sidebarPanel(
    selectInput(inputId = "x",
                label = "Plot",
                choices = c('AvgHemoglobin', 'AvgWeight','AvgAnopheles','Avgageatvisit',
                            'GeoMeanParasiteDensity','MatchingVisitsYear','matchingvisits','YearsofObservation'),
                selected = "Avgageatvisit"),
    selectInput(inputId = "y",
                label = "Against",
                choices = c('Avgageatvisit','AvgHemoglobin','AvgWeight','AvgAnopheles',
                            'GeoMeanParasiteDensity','MatchingVisitsYear','matchingvisits','YearsofObservation'),
                selected = "MatchingVisitsYear"),
    selectInput(inputId = "facet",
                label = "Facets",
                choices = c('SubcountyinUganda','Dwellingtype','Sex','G6PDGenotype','AthalassemiaGenotype','HbSGenotype'),
                selected = "SubcountyinUganda"),
    selectInput(inputId = "color",
                label = "Color",
                choices = c('SubcountyinUganda','Dwellingtype','Sex','G6PDGenotype','AthalassemiaGenotype','HbSGenotype'),
                selected = "HbSGenotype"),
    selectInput(inputId = "ptype",
                label = "Plot Type",
                choices = c('point', 'bar'),
                selected = "point")
    #dateRangeInput("dateR","Period", start = min.d, end = max.d, min = min.d, max =max.d)
  ),
  mainPanel(
    showOutput("myChart", "polycharts")
  )
))
