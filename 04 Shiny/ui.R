#ui.R 

library(shiny)
require(shinydashboard)
require(leaflet)

# Define UI for application that plots random distributions 
dashboardPage(
  # Application title
  dashboardHeader(title = "Portuguese Bank Marketing Data", titleWidth = 350),
  dashboardSidebar(width = 350,
    sidebarMenu(
      menuItem("ScatterPlot", tabName = "scatterplot", icon = icon("line-chart")),
      menuItem("Barchart", tabName = "barchart", icon = icon("bar-chart")),
      menuItem("CrossTab", tabName = "crosstab", icon = icon("table")),
      menuItem("Blending", tabName = "blending", icon = icon("link"))
    )
  ),
  dashboardBody(
    tabItems(
      #first tab content
      tabItem(tabName = "scatterplot",
              h4("Scatterplot Input: "),
              selectInput("OutcomeSelectionFilter",
                          "Filter Outcome:", 
                          choices = list("Yes" = 1, "No" = 2, "All" = 3), selected = 3),
              h4("Scatterplot: "),
              plotOutput("scatterPlot")
              ),
      tabItem(tabName = "barchart",
              h4("Barchart Input:"),
              radioButtons("ReferenceLine", 
                           "Reference Lines:",
                           choices = list("Average" = 1, "Minimum" = 2,
                                          "Maximum" = 3, "Sum" = 4), selected = 1),
              h4("Barchart: "),
              plotOutput("barPlot")
              ),
      tabItem(tabName = "crosstab",
              h4("Crosstab Inputs: "),
              sliderInput("KPI1", 
                          "KPI_Low_Max_value:", 
                          min = 0,
                          max = .13, 
                          value = .1),
              sliderInput("KPI2", 
                          "KPI_Medium_Max_value:", 
                          min = .13,
                          max = .18, 
                          value = .15),
              h4("Crosstab: "),
              plotOutput("crosstabPlot")
              ),
      tabItem(tabName = "blending",
              h4("Blended Data Barchart: "),
              plotOutput("blendedPlot"))
    )
  )
)

