pearson_r <- function(x) {
  
  cor <- x[1,-1:-nrow(x)]

for(i in 1:ncol(x))
  cor[,i] = sum((x[, 1] - mean(x[, 1]))*(x[, i] - mean(x[, i])))/
  sqrt((sum((x[, 1] - mean(x[, 1]))**2)*(sum((x[, i] - mean(x[1, i]))**2)))) 

colnames(cor) <- colnames(x)

return(cor)

}