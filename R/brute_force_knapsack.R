brute_force_knapsack <- function(x, W){
  n <- nrow(x)
  all_combos <- as.numeric(intToBits(0:(2^n-1)))
  all_combos <-
    sapply(0:(2^n-1), function(q) all_combos[(32*q+1):(32*(q+1))])
  all_combos <- all_combos[1:n,]
  df_sums <- data.frame(index=0:(ncol(all_combos)-1),
                        values = colSums(all_combos * x$v),
                        weights = colSums(all_combos * x$w))
  df_sums <- df_sums[df_sums$weights <= W,]
  df_sums <- df_sums[order(df_sums$values, decreasing = T),]
  list(value = df_sums$values[1],
       elements=(1:n)[as.logical(intToBits(df_sums$index[1])[1:n])])
}

