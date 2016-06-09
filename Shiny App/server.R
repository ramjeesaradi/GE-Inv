library(shiny)

shinyServer(function(input,output){
  
  filein <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
  updMod <- reactive({
    if(input$part=="" | is.null(featrs)){return(NULL)}
    featrs <<- as.character(input$featrs)
    target <<- as.character(input$target)
    part <<- as.character(input$part)
    lr.model <<- getLM(df[df$Part.Number == part,],featrs,target)
  })
  
  output$InvDailyValue <- renderPlot({
    df <<-  filein()
    getDailyInv(df)
  })
  
  output$abcPlot <- renderPlot({
    input$file1
    parts <<- getABCpie(df)
    partsA <<- parts$partsA
    partsB <<- parts$partsB
    partsC <<- parts$partsC
    output$Aparts <- renderTable({
      as.data.frame(partsA)
    })
  })
  
  output$featsel <- renderUI({
    selectInput("featrs",label = "Select Factors",choices = names(df),multiple = T)
  })
  
  output$predicted <- renderUI({
    selectInput("target",label = "Target",choices = names(df))
  })
  output$partsel <- renderUI({
    
    selectInput("part",label = "Select Part"
                ,choices = as.character(get(paste(c("parts",as.character(input$partCat)),collapse = ""))))
  })
  output$graph1 <- renderPlot({
    getgraph1(df,partsA)
  })
  output$graph2 <- renderPlot({
    getgraph2(df,partsA)
  })
  output$graph3 <- renderPlot({
    getgraph3(df,partsA)
  })
  output$PartAvailIssuegraph <- renderPlot({
    getPartAvailIssuegraph(df,input$part)
  })
  
  output$PartIssuegraph <- renderPlot({
    getPartIssuegraph(df,input$part)
  })
  
  output$PartAvailgraph <- renderPlot({
    getPartAvailgraph(df,input$part)
  })
  
  output$modeldesc <- renderText({
    updMod()
    output$Coefficients <- renderPlot({
      getlrCoeffgraph(lr.model)
    })
    paste(c("Regressing",target,"of", part, "on", paste(featrs,collapse = ", ")),collapse = " ")
  })
  
  # output$Coefficients <- renderPlot({
  #   getlrCoeffgraph(lr.model)
  # })
  
  
  output$InvLvl <- renderText({
    Vec <- Vec()
    # if(is.null(lr.model)){return(0)}
    return(predict(lr.model,Vec))
  })
  
})
