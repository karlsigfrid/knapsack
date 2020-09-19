dynamic_knapsack <- function(x, W){
  #calculate the max total value
  n <- nrow(x)
  m <- matrix(rep(0, n*(W+1)), nrow=n)
  li <- as.list(rep(NA, n))
  m[1,] <- x$v[1] * ((0:W) %/% x$w[1])
  for (i in 2:n){
    for(w in 0:W){
      if(x$w[i] > w){
        m[i, w+1] <- m[i-1, w+1]
      }else{
        m[i, w+1] <- max(m[i-1, w+1-x$w[i]] + x$v[i] , m[i-1, w+1])
      }
    }
  }
  #Traceback to identify elements
  m <- rbind(0, m)
  i <- W+1
  ksack <- c()
  for(j in (n+1):2){
    if(m[j, i] > m[j-1, i]){
      ksack <- c(ksack, j-1)
      i <- i - x$w[j-1]
    }
  }
  #results
  list(value=m[n+1, W+1], elements=rev(ksack))
}


