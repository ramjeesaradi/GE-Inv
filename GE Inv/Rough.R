setwd("/Users/Admin/Documents/workspace/GE Inv/")

invD <- read.csv("Inventory-data.csv")

c = 3
m = 1
y <- invD$Issue[invD$Part.Number == "GN-301"]
# y <- c + m*x + sample(-100000:100000/8100000, length(x),replace = T)
x <- (y-c)/m + sample(-100000:100000/81000, length(y),replace = T)
plot(x,y)

y <- invD$Issue[invD$Part.Number == "GN-301"]
# y <- c + m*x + sample(-100000:100000/8100000, length(x),replace = T)
x1 <- (y-c)/m
View(x1)
corrupt1 <- rbinom(length(x1),1,0.8) # choose an average of 10% to corrupt at random
corrupt1 <- as.logical(corrupt1)
noise1 <- rnorm(sum(corrupt1),1000,200) # generate the noise to add
x1[corrupt1] <- x1[corrupt1] + noise1
plot(x1,y)


str <- "output$InvLvl <- renderText(input$featr)"
codestrs <- sapply(featrs, function (x) gsub("featr", x, str))

str <- "input$featr"
codestrs <- sapply(featrs, function (x) gsub("featr", x, str))
paste(codestrs,collapse = ", ")

getgraph4(df,"GN-301")
