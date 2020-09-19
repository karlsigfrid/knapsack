greedy_knapsack <- function(x, W){
  x$v_per_w <- x$v/x$w
  x$index <- 1:nrow(x)
  x <- x[order(x$v_per_w, decreasing = T),]
  ksack <- c()
  for(i in 1:nrow(x)){
    if(x$w[i] <= W)
      ksack <- c(ksack, x$index[i])
      W <- W - x$w[i]
  }
  list(value=sum(x$v[x$index %in% ksack]), elements=ksack)
}



