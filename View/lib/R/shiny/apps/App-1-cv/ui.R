## ui.R
require(rCharts)
shinyUI(pageWithSidebar(
  headerPanel("Clinical Visit data Visualizations"),
  
  sidebarPanel(
    selectInput(inputId = "x",
                label = "Plot",
#                choices = c('ageattimeofvisit','dateofvisit','hemoglobin','asexualparasitedensity'),
                choices = list('Age at time of visit' = 'ageattimeofvisit','Date of visit' = 'dateofvisit','Hemoglobin' = 'hemoglobin'),
                selected = "ageattimeofvisit"),
    selectInput(inputId = "y",
                label = "Against",
                choices = list('Hemoglobin' = 'hemoglobin','Age at time of visit' = 'ageattimeofvisit'),
#                choices = c('hemoglobin','asexualparasitedensity','ageattimeofvisit'),
                selected = "hemoglobin"),
    selectInput(inputId = "color",
                label = "Color",
                choices = list('Malaria diagnosis' = 'malariadiagnosisandparasitestatus','Subcounty in Uganda' = 'subcountyinuganda','Asexual parasite present' = 'asexualparasitespresent', 'Visit type' = 'visittype'),
                selected = "malariadiagnosisandparasitestatus"),
#    selectInput(inputId = "group",
#                label = "Group",
#                choices = c('subcountyinuganda', 'malariadiagnosisandparasitestatus', 'asexualparasitespresent', 
#                            'visittype'),
#                selected = ""),
#    selectInput(inputId = "fill",
#                label = "Fill",
#                choices = c( 'asexualparasitespresent', 'subcountyinuganda', 'malariadiagnosisandparasitestatus',
#                            'visittype'),
#                selected = ""),
    selectInput(inputId = "facet",
                label = "Facet",
                choices = list('Subcounty in Uganda' = 'subcountyinuganda','Visit type' = 'visittype','Asexual parasite present' = 'asexualparasitespresent'),
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
