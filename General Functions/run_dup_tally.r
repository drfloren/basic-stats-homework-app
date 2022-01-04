run_dup_tally <- function(x){ #running duplicate tally
  out <- numeric(length=length(x))
  out[1] <- 1
  for(i in 2:length(x))
    out[i] <- sum(x[i] == x[1:i])
  out
}