getmode <- function(vec){
  unq <- unique(vec)
  counts <- sapply(unq, function (x) length(vec[vec==x]))
  counts <- cbind(unq,counts)
  counts <- counts[order(counts[,2],decreasing = T),]
  return(counts[,])
}