setwd("/Users/Admin/Documents/workspace/GE Inv/")
library(ABCanalysis)
library(caret)
itemCost <- read.csv("abc-data.csv")

abc <- ABCanalysis(itemCost$Cost,PlotIt = T)
plot((1:nrow(itemCost)), sapply(1:nrow(itemCost)
                                      , function (x) sum(itemCost[order(itemCost$Cost,decreasing = T)[1:x],"Cost"])))
