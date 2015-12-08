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
      menuItem("CrossTab", tabName = "crosstab", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      #first tab content
      tabItem(tabName = "scatterplot",
              h4("Scatterplot Input: "),
              selectInput("OutcomeSelectionFilter",
                          "Filter Outcome:", 
                          choices = list("Lower Tail" = 1, "Upper Tail" = 2, "All" = 3), selected = 3),
              sliderInput("LowerTail", 
                          "Lower_Tail_Percent:", 
                          min = 2,
                          max = 40, 
                          value = 20),
              sliderInput("UpperTail", 
                          "Upper_Tail_Percent:", 
                          min = 60,
                          max = 99, 
                          value = 80),
              h4("Scatterplot: "),
              plotOutput("scatterPlot")
              ),
      tabItem(tabName = "barchart",
              h4("Barchart Input:"),
              radioButtons("BarchartSorting", 
                           "Sort By:",
                           choices = list("Average Salary" = 1, "Number of Campaigns" = 2), selected = 1),
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
              )
    )
  )
)

