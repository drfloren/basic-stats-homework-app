
fr <- function(x, dig=4){
  format(round(x, digits = dig), nsmall=dig)
}

t_ci <- function(data, alpha, dig=3){
  n <- length(data)
  df <- n-1
  mu <- mean(data)
  t <- qt(c(alpha/2, 1-alpha/2), df)
  se <- sd(data)/sqrt(n) #note: this works for t, but need to change var/sd calculation for z...
  
  # Step 1: Pieces
  s1 <- list(mu = fr(mu),
             t = fr(t),
             se = fr(se))
  
  # Step 2a: PM Confidence Interval
  pm <- paste0(fr(mu), " \u00B1 ", fr(max(t)*se)) #\u00B1 or ±
  
  # Step 2b: Evaluated Confidence Interval
  ev <- paste0("(", fr(mu + min(t)*se), ", ", fr(mu + max(t)*se), ")")
  
  list(`Pieces`=s1, 
       `Confidence Interval (plus/minus)` = pm,
       `Confidence Interval (evaluated)` = ev)
}