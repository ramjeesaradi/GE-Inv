library(ABCanalysis)
library(ggplot2)
library(caret)
library(plyr)
library(reshape2)

lr.model <- NULL
df <- NULL
part <- NULL
partsA <- c("")
partsB <- c("")
partsC <- c("")
featrs <- c("dev.EDD", "partial.Delivery", "price.Rise","discount","A1", "A2", "A3", "P.A","F.A" )
target <- "Available.Quantity"

synthFeat <- function(df,featrs){
  df[, featrs] <- rep(0,nrow(df))
  
  
  df$intrvls <- cut(df$Available.Quantity,breaks = length(featrs),labels = F)
  
  for( i in 1:length(featrs)){
    df[df$intrvls == i, featrs[i]] <- 1
  }
  
  
  df$dev.EDD <- df$dev.EDD * sample(-21:21, nrow(df),T)
  df$F.A <- sample(60:140/100, nrow(df),T)
  df$P.A <- sample(20:100/100, nrow(df),T)
  
  write.csv(df,"/Users/Admin/Documents/workspace/GE Inv/SynthData.csv",row.names = F)
}

getLM <- function(invD, variables,tar) {
  lm.model <- lm(paste(c(tar, paste(featrs,collapse  = "+")), collapse = "~"), invD)
  return(lm.model)
}

getABC <- function(invD){
  parts <- list()
  invDAggr <-  aggregate(Available.Quantity*Unit.Cost ~Part.Number,invD,sum)
  abc <- ABCanalysis(invDAggr$`Available.Quantity * Unit.Cost`,PlotIt = T)
  parts$partsA <- as.character(invDAggr$Part.Number[abc$Aind])
  parts$partsB <- as.character(invDAggr$Part.Number[abc$Bind])
  parts$partsC <- as.character(invDAggr$Part.Number[abc$Cind])
  return(parts)
}

getgraph1 <- function(invD,parts){
  invD <- invD[invD$Part.Number %in% parts,]
  invDAggr <-  aggregate(cbind(Unit.Cost,Issue) ~ Part.Number,invD, mean)
  p <- {ggplot(invDAggr) +
    theme(axis.text.x = element_text(angle = 270)) +
    geom_bar(aes(factor(Part.Number), Unit.Cost),stat = "identity",fill = "blue",position = position_dodge()) +
    # geom_bar(aes(factor(Part.Number), Issue),stat = "identity", fill = "yellow") +
    xlab("Part Number")+
    ylab("Cost")
    }
  return(p)
}

getgraph2 <- function(invD,parts){
  invD <- invD[invD$Part.Number %in% parts,]
  invDAggr <-  aggregate(Issue ~Part.Number,invD,sum)
  p <- {ggplot(invDAggr, aes(factor(Part.Number), Issue) ,xlab ="Inventory ID" ,ylab = "Avg Price") +
      theme(axis.text.x = element_text(angle = 270)) +
      geom_bar(stat = "identity", fill = "green")}
  return(p)
}

getgraph3 <- function(invD,parts){
  invD <- invD[invD$Part.Number %in% parts,]
  invDAggr <-  invD
  p <- qplot(invDAggr$Part.Number,invDAggr$Available.Quantity,xlab ="Inventory ID" ,ylab = "Distribution",geom = "boxplot")
  p + theme(axis.text.x = element_text(angle = 270))
}

getPartAvailIssuegraph <- function(invD,parts){
  invD <- invD[invD$Part.Number %in% parts,]
  invD$Date <- as.Date(invD$Date,"%d-%m-%Y")
  invD <- melt(invD,c("Part.Number","Date"), c("Issue","Available.Quantity"))
  invDAggr <-  invD
  p <- {ggplot(invD) +
  scale_x_date(date_breaks = "2 weeks") +
  geom_line(aes(Date, value, group = variable,colour= variable), size = 1.5) + 
  # geom_line(aes(Date, Issue),colour="blue", size = 1.5) +
  #geom_smooth(span = 0.3, method = "loess") +
  xlab("Day") +
  ylab("Quantity") +
  theme(legend.position = "right")
      }
  return(p)
}

getPartAvailgraph <- function(invD,parts){
  invD <- invD[invD$Part.Number %in% parts,]
  invD$Date <- as.Date(invD$Date,"%d-%m-%Y")
  invDAggr <-  invD
  meanV <- mean(invDAggr$Available.Quantity)
  sd <- sqrt(var(invDAggr$`Available.Quantity`))
  sd <- sd*c(1,2,3)
  yintercept = c(meanV,meanV+sd, meanV-sd)
  p <- {ggplot(invD) +
      scale_x_date(date_breaks = "2 weeks") +
      geom_line(aes(Date, Available.Quantity),colour="green", size = 1.5) + 
      geom_hline(yintercept = yintercept) +
      # geom_line(aes(Date, Issue),colour="blue", size = 1.5) +
      #geom_smooth(span = 0.3, method = "loess") +
      xlab("Day") +
      ylab("Quantity")
  }
  return(p)
}

getPartIssuegraph <- function(invD,parts){
  invD <- invD[invD$Part.Number %in% parts,]
  invD$Date <- as.Date(invD$Date,"%d-%m-%Y")
  invDAggr <-  invD
  meanV <- mean(invDAggr$`Issue`)
  sd <- sqrt(var(invDAggr$`Issue`))
  sd <- sd*c(1,2,3)
  yintercept = c(meanV,meanV+sd, meanV-sd)
  p <- {ggplot(invD) +
      scale_x_date(date_breaks = "2 weeks") +
      # geom_line(aes(Date, Available.Quantity),colour="green", size = 1.5) + 
      geom_line(aes(Date, Issue),colour="blue", size = 1.5) +
      geom_hline(yintercept = yintercept) +
      #geom_smooth(span = 0.3, method = "loess") +
      xlab("Day") +
      ylab("Quantity") +
      theme(legend.position = "right")
  }
  return(p)
}

getABCpie <- function(invD){
  parts <- list()
  invDAggr <-  aggregate(Available.Quantity*Unit.Cost ~Part.Number,invD,sum)
  abc <- ABCanalysis(invDAggr$`Available.Quantity * Unit.Cost`)
  invDAggr$label <- ""
  invDAggr[abc$Aind,"label"] <- "Type 1"
  invDAggr[abc$Bind,"label"] <- "Type 2"
  invDAggr[abc$Cind,"label"] <- "Type 3"
  invDpie <- aggregate(`Available.Quantity * Unit.Cost`~label,invDAggr,sum)
  invDpie <- invDpie[order(invDpie$label),]
  pie(invDpie$`Available.Quantity * Unit.Cost`,invDpie$label)
  parts$partsA <- as.character(invDAggr$Part.Number[abc$Aind])
  parts$partsB <- as.character(invDAggr$Part.Number[abc$Bind])
  parts$partsC <- as.character(invDAggr$Part.Number[abc$Cind])
  return(parts)
}

getDailyInv <- function(invD){
  invDAggr <-  aggregate(Available.Quantity * Unit.Cost ~ Date,invD,sum)
  meanV <- mean(invDAggr$`Available.Quantity * Unit.Cost`)
  sd <- sqrt(var(invDAggr$`Available.Quantity * Unit.Cost`))
  sd <- sd*c(1,2,3)
  yintercept = c(meanV,meanV+sd, meanV-sd)
  # invDAggr$DailyValue <- invDAggr$`Available.Quantity * Unit.Cost`
  invDAggr$Date <- as.Date(invDAggr$Date,"%d-%m-%Y")
  p <- {ggplot(invDAggr, aes(Date, `Available.Quantity * Unit.Cost`)) + 
      scale_x_date(date_breaks = "2 weeks") + 
      geom_line() +
      # geom_hline(yintercept = yintercept,show.legend = TRUE) +
      geom_smooth(span = 0.3, method = "loess") + 
      xlab("") + 
      ylab("Available Quantity") +
      theme(panel.background = element_rect(fill = yintercept[order(yintercept)],colour = yintercept[order(yintercept)]) )}
  return(p)
}

getlrCoeffgraph <- function(lr.model){
  coeffs <- as.data.frame(lr.model$coefficients)
  coeffs$factors <- row.names(coeffs)
  coeffs$Coefficients <- coeffs$`lr.model$coefficients`
  p <- ggplot(coeffs,aes(factors,Coefficients, fill = Coefficients, colour = Coefficients)) + geom_bar(stat = "identity")
  return(p)
}