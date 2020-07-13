library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Boxplot"),
  
  sidebarPanel(
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 500),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500),
    checkboxInput("jitter", "Jitter", value = FALSE),
    checkboxInput("notch", "Notch", value = FALSE),
    checkboxInput("addmean", "Add mean", value = FALSE),
    checkboxInput("log", "Log transformation", value = FALSE),
    textInput("xlab", "x axis label", "Group"),
    textInput("ylab", "y axis label", "Value")
  ),
  
  mainPanel(
    uiOutput("reacOut")
  )
  
))