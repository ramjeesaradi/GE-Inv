source("/Users/Admin/Documents/workspace/GE Inv/Shiny App/helpers.R")
setwd("/Users/Admin/Documents/workspace/GE Inv/Shiny App/")
source("abc.R")
source("graphs.R")
source("model.R")
source("exprmnts.R")

shinyUI(fluidPage(
  titlePanel(title=h1("Inventory Analysis", align="center")),
  mainPanel(type="tab",width = 12,
            tabsetPanel(
              abctab
              ,exprmnts
              ,graphstab
              ,modeltab
            ))
  
))