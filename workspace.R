library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(ggplot2)

# http://127.0.0.1:5402/admin/w/0364f962da893f82fef2b9549204c830/ds/594ed2cd-7212-40e4-971d-3834e1d82807
############################################
#### This part should not be included in ui.R and server.R scripts
# http://127.0.0.1:5402/admin/w/0364f962da893f82fef2b95492005f45/ds/f3b5ff7d-6c8a-4146-9412-296262090545
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "f3b5ff7d-6c8a-4146-9412-296262090545",
                   workflowId = "0364f962da893f82fef2b95492005f45")
  
  # ctx <- tercenCtx(stepId = "594ed2cd-7212-40e4-971d-3834e1d82807",
  #                  workflowId = "0364f962da893f82fef2b9549204c830")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  
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

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  output$reacOut <- renderUI({
    plotOutput(
      "main.plot",
      height = input$plotHeight,
      width = input$plotWidth
    )
  }) 
  
  output$main.plot <- renderPlot({
    
    values <- dataInput()
    
    df <- values$data
    # in.col <- values$colors
    
    input.par <- list(
      notch = input$notch,
      jitter = input$jitter,
      add.mean = input$addmean,
      log = input$log,
      xlab = input$xlab,
      ylab = input$ylab
    )
    
    if(input.par$log) df$.y <- log1p(df$.y)
    
    fill.col <- NULL
    # if(length(unique(in.col)) > 1) fill.col <- as.factor(in.col)
    
    theme_set(theme_minimal())
    
    plt <- ggplot(df, aes( x= .xLevels, y = .y, fill = fill.col)) + 
      geom_boxplot() 
    
    if (length(values$ctx$colors)>0){
      plt = plt + geom_point(aes(col=colorsValues)) + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      plt = plt + geom_point(aes()) + theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    plt = plt + labs(x = input.par$xlab, y = input.par$ylab, fill = "Legend") +
      facet_grid( as.formula(paste0(paste(values$ctx$rnames, collapse = " + "),
                                    "~",
                                    paste(values$ctx$cnames, collapse = " + "))),
                  scales = "free")
    
    plt
    
  })
  
})

getValues <- function(session){
  
  ctx <- getCtx(session)
  
  values <- list()
  
  values$ctx = ctx
  
  values$data <- ctx$select(list(".y", ".ri", ".ci", ".xLevels")) 
  
  if (length(values$ctx$colors)>0){
    colorsValues =  do.call("paste", ctx$select(ctx$colors))
    values$data = values$data %>% mutate(colorsValues=colorsValues)
  } 
  
  values$data = values$data %>%
    mutate(ci = as.character(.ci)) %>%
    mutate(ri = as.character(.ri))
  
  column = ctx$cselect() %>% mutate(.ci= (seq_len(nrow(.))-1) )
  row = ctx$rselect() %>% mutate(.ri= (seq_len(nrow(.))-1) )
  
  values$data = values$data %>% 
    left_join(column, by=".ci") %>%
    left_join(row, by=".ri")
  
  return(values)
}




runApp(shinyApp(ui, server))  
