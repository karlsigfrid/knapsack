brute_force_knapsack <- function(x, W){
  n <- length(x)
  all_combos <- as.numeric(intToBits(0:(2^n-1)))
  all_combos <-
    sapply(0:(2^n-1), function(q) all_combos[(32*q+1):(32*(q+1))])
  all_combos <- all_combos[1:n,]
}
