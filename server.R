library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(ggplot2)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
  
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
    in.col <- values$colors
    
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
    if(length(unique(in.col)) > 1) fill.col <- as.factor(in.col)
    
    theme_set(theme_minimal())
    
    if(!input.par$jitter) {
      plt <- ggplot(df, aes(x = as.factor(rnames), y = .y, fill = fill.col)) + 
        geom_boxplot(notch = input.par$notch) + labs(x = input.par$xlab, y = input.par$ylab, fill = "Legend")
    } else {
      plt <- ggplot(df, aes(x = as.factor(rnames), y = .y)) + 
        geom_boxplot(notch = input.par$notch) + labs(x = input.par$xlab, y = input.par$ylab)
    }
    
    if(input.par$add.mean) plt <- plt + stat_summary(fun.y = mean, geom = "point",
                                                     shape = 18, size = 2.5, color = "#FC4E07")
    
    if(input.par$jitter) plt <- plt + geom_jitter(aes(color = fill.col), position = position_jitter(0.2), size = 0.7)
    
    if(!is.null(df$cnames)) plt <- plt + facet_wrap(~ cnames)
    
    plt
    
  })
  
})

getValues <- function(session){
  
  ctx <- getCtx(session)
  
  values <- list()
  
  values$data <- ctx %>% select(.y, .ri, .ci) %>%
    group_by(.ri)
  
  values$colors <- NA
  if(length(ctx$colors)) values$colors <- ctx$select(ctx$colors[[1]])[[1]]
  
  values$rnames <- ctx$rselect()[[1]]
  names(values$rnames) <- seq_along(values$rnames) - 1
  values$data$rnames <- values$rnames[as.character(values$data$.ri)]
  
  values$cnames <- ctx$cselect()[[1]]
  names(values$cnames) <- seq_along(values$cnames) - 1
  values$data$cnames <- values$cnames[as.character(values$data$.ci)]
  
  return(values)
}