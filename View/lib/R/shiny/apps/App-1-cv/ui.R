## ui.R
require(rCharts)
shinyUI(pageWithSidebar(
  headerPanel("Clinical Visit data Visualizations"),
  
  sidebarPanel(
    selectInput(inputId = "x",
                label = "Plot",
#                choices = c('ageattimeofvisit','visitdate','hemoglobin','asexualparasitedensity'),
                choices = c('ageattimeofvisit','visitdate','hemoglobin'),
                selected = "ageattimeofvisit"),
    selectInput(inputId = "y",
                label = "Against",
                choices = c('hemoglobin','ageattimeofvisit'),
#                choices = c('hemoglobin','asexualparasitedensity','ageattimeofvisit'),
                            selected = "hemoglobin"),
    selectInput(inputId = "color",
                label = "Color",
                choices = c('malariadiagnosis','subcountyinuganda', 'asexualparasitespresent', 
                            'visittype'),
                selected = "malariadiagnosis"),
#    selectInput(inputId = "group",
#                label = "Group",
#                choices = c('subcountyinuganda', 'malariadiagnosis', 'asexualparasitespresent', 
#                            'visittype'),
#                selected = ""),
#    selectInput(inputId = "fill",
#                label = "Fill",
#                choices = c( 'asexualparasitespresent', 'subcountyinuganda', 'malariadiagnosis',
#                            'visittype'),
#                selected = ""),
    selectInput(inputId = "facet",
                label = "Facet",
                choices = c('subcountyinuganda', 'visittype', 'asexualparasitespresent'),
                selected = "subcountyinuganda"),
    selectInput(inputId = "type",
                label = "Graph Type",
                choices = c('point', 'bar'),
                selected = "point")
  ),
  mainPanel(
    showOutput("myChart", "polycharts")
  )
))
