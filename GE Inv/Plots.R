setwd("/Users/Admin/Documents/workspace/GE Inv/")
library(ABCanalysis)
library(caret)

invD <- read.csv("Inventory-data.csv")

invDAggr <-  aggregate(Available.Quantity*Unit.Cost ~Part.Number,invD,sum)
abc <- ABCanalysis(invDAggr$`Available.Quantity * Unit.Cost`)
t5parts <- invDAggr[order(invDAggr$`Available.Quantity * Unit.Cost`, decreasing = T)[1:5],"Part.Number"]

# sd <- sqrt(var(invD$Available.Quantity))
# meanQuan <- mean(invD$Available.Quantity)
# minQuan <- min(invD$Available.Quantity)
# modeQuan <- getmode(invD$Available.Quantity)

######################Vol to Item ##############

for(i in t5parts){
  dat <- invD[invD$Part.Number == i,]
  # dat <- dat[order]
  plot(y=dat$Available.Quantity,x=dat$Date)

agrdtaVol <- aggregate(Available.Quantity ~ Part.Number,dat,FUN = sum)
plot(agrdtaVol$Part.Number,agrdtaVol$Available.Quantity)
plot(agrdtaVol$Part.Number,log(agrdtaVol$Available.Quantity))

agrdtaCost <- aggregate(Unit.Cost ~ Part.Number, dat, mean)
plot(agrdtaCost$Part.Number,(agrdtaCost$Unit.Cost))
plot(agrdtaCost$Part.Number,log(agrdtaCost$Unit.Cost))
}
boxplot(Available.Quantity ~ Part.Number, invD[invD$Part.Number %in% t5parts,])
