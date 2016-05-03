invD1 <- read.csv("Inventory-data.csv")
invD1 <- data.frame(Available.Quantity = invD1[invD1$Part.Number == "GN-301",c("Available.Quantity")])
featrs <- c("dev.EDD", "partial.Delivery", "price.Rise","discount","partial.Delivery","A1", "A2", "A3", "P.A","F.A" )
invD1[, featrs] <- rep(0,nrow(invD1))


invD1$intrvls <- cut(invD1$Available.Quantity,breaks = length(featrs),labels = F)

for( i in 1:length(featrs)){
  print(i)
  invD1[invD1$intrvls == i, featrs[i]] <- 1
}


invD1$dev.EDD <- invD1$dev.EDD * sample(-21:21, nrow(invD1),T)
invD1$F.A <- sample(60:140/100, nrow(invD1),T)
invD1$P.A <- sample(20:100/100, nrow(invD1),T)


# invD1 <- invD1[,!(names(invD1) %in% "intrvls")]

lm.model <- lm(paste(c("Available.Quantity", paste(featrs,collapse  = "+")), collapse = "~"), invD1)
featurePlot(invD1[,featrs],invD1$Available.Quantity,plot = "pairs")
