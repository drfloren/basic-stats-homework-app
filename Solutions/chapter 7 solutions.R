
fr <- function(x, dig=4){
  format(round(x, digits = dig), nsmall=dig)
}

# Text Solutions ----
c7s_t <- function(data, alpha, dig=3){
  n <- length(data)
  df <- n-1
  mu <- mean(data)
  t <- qt(c(alpha/2, 1-alpha/2), df)
  se <- sd(data)/sqrt(n) #note: this works for t, but need to change var/sd calculation for z...
  
  # Step 1: Pieces
  s1 <- list(mu = fr(mu),
             df = df,
             t = fr(t),
             sd = fr(sd(data)),
             se = fr(se))
  
  # Step 2a: PM Confidence Interval
  pm <- paste0(fr(mu), " \u00B1 ", fr(max(t)*se)) #\u00B1 or ±
  
  # Step 2b: Evaluated Confidence Interval
  ev <- paste0("(", fr(mu + min(t)*se), ", ", fr(mu + max(t)*se), ")")
  
  list(`Pieces`=s1, 
       `Confidence Interval (plus/minus)` = pm,
       `Confidence Interval (evaluated)` = ev)
}

c7s_z <- function(data, alpha, sdd, dig=3){
  n <- length(data)
  mu <- mean(data)
  z <- qnorm(c(alpha/2, 1-alpha/2))
  se <- sdd/sqrt(n)
  
  # Step 1: Pieces
  s1 <- list(mu = fr(mu),
             z = fr(z),
             se = fr(se))
  
  # Step 2a: PM Confidence Interval
  pm <- paste0(fr(mu), " \u00B1 ", fr(max(z)*se)) #\u00B1 or ±
  
  # Step 2b: Evaluated Confidence Interval
  ev <- paste0("(", fr(mu + min(z)*se), ", ", fr(mu + max(z)*se), ")")
  
  list(`Pieces`=s1, 
       `Confidence Interval (plus/minus)` = pm,
       `Confidence Interval (evaluated)` = ev)
}

c7s_p <- function(alpha, n, x, dig=3){
  p <- round(x/n,4)
  q <- 1-p
  z <- qnorm(c(alpha/2, 1-alpha/2))
  se <- sqrt(p*q/n)
  
  # Step 1: Pieces
  s1 <- list(p = fr(p),
             z = fr(z),
             se = fr(se))
  
  # Step 2a: PM Confidence Interval
  pm <- paste0(fr(p), " \u00B1 ", fr(max(z)*se)) #\u00B1 or ±
  
  # Step 2b: Evaluated Confidence Interval
  ev <- paste0("(", fr(p + min(z)*se), ", ", fr(p + max(z)*se), ")")
  
  list(`Pieces`=s1, 
       `Confidence Interval (plus/minus)` = pm,
       `Confidence Interval (evaluated)` = ev)
}

c7s_text <- function(prob_type, hdat){
  prob_names <- c("z", "t", "p")

  if(!(prob_type %in% prob_names))
    stop(paste0("Problems must be one of the following: random, ", paste0(prob_names, collapse=", ")))
  pn <- prob_type
  
  if(pn == "z"){
    out <- c7s_z(data = hdat$dat, alpha = hdat$alpha, sdd = hdat$sdd)
  } else if(pn == "t"){
    out <- c7s_t(data = hdat$dat, alpha = hdat$alpha)
  } else if(pn == "p"){
    out <- c7s_p(alpha = hdat$alpha, n = hdat$n, x = hdat$x)
  }
  out
}

# Plot Solutions ----
c7s_tplot <- function(data, alpha, tail_exp=1.2){
  df <- length(data)-1
  
  gt <- paste0("T-Distribution with df=", df, " and Alpha=", alpha, ": Two-Tailed")
  lower_bound <- -4 #qt(alpha/2, df=df)*tail_exp # I think I prefer keeping these static... I think +-4 keeps them big enough so that the graphs won't go off of the bounds...
  upper_bound <- 4 #qt(1-alpha/2, df=df)*tail_exp
  x <- seq(lower_bound, upper_bound, by=.01)
  y <- dt(x, df=df)
  
  #base plot
  plot(x, y, main=gt, type="l", xlab="t", ylab="Density", ylim=c(0,.4))
  
  #shading area
  lower_shade_l <- lower_bound
  upper_shade_l <- qt(alpha/2, df=df)
  shade_x_l <- c(lower_shade_l, seq(lower_shade_l, upper_shade_l, by=.01), upper_shade_l)
  shade_y_l <- c(0,dt(seq(lower_shade_l, upper_shade_l, by=.01), df=df),0)
  
  lower_shade_u <- qt(1-alpha/2, df=df)
  upper_shade_u <- upper_bound
  shade_x_u <- c(lower_shade_u, seq(lower_shade_u, upper_shade_u, by=.01), upper_shade_u)
  shade_y_u <- c(0,dt(seq(lower_shade_u, upper_shade_u, by=.01), df=df),0)
  
  axis(side=1, at=upper_shade_l, labels=paste0("T=", fr(upper_shade_l,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
  axis(side=1, at=lower_shade_u, labels=paste0("T=", fr(lower_shade_u,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
  
  polygon(shade_x_l, shade_y_l, col="skyblue")
  polygon(shade_x_u, shade_y_u, col="skyblue")
}

c7s_zplot <- function(alpha, tail_exp=1.2){
  gt <- paste0("Z-Distribution with Alpha=", alpha,": Two-Tailed")
  
  lower_bound <- -3 #qnorm(alpha/2)*tail_exp
  upper_bound <- 3 #qnorm(1-alpha/2)*tail_exp
  x <- seq(lower_bound, upper_bound, by=.01)
  y <- dnorm(x)
  
  #base plot
  plot(x, y, main=gt, type="l", xlab="t", ylab="Density")
  
  #shading area
  lower_shade_l <- lower_bound
  upper_shade_l <- qnorm(alpha/2)
  shade_x_l <- c(lower_shade_l, seq(lower_shade_l, upper_shade_l, by=.01), upper_shade_l)
  shade_y_l <- c(0,dnorm(seq(lower_shade_l, upper_shade_l, by=.01)),0)
  
  lower_shade_u <- qnorm(1-alpha/2)
  upper_shade_u <- upper_bound
  shade_x_u <- c(lower_shade_u, seq(lower_shade_u, upper_shade_u, by=.01), upper_shade_u)
  shade_y_u <- c(0,dnorm(seq(lower_shade_u, upper_shade_u, by=.01)),0)
  
  axis(side=1, at=upper_shade_l, labels=paste0("Z=", fr(upper_shade_l,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
  axis(side=1, at=lower_shade_u, labels=paste0("Z=", fr(lower_shade_u,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
  
  polygon(shade_x_l, shade_y_l, col="skyblue")
  polygon(shade_x_u, shade_y_u, col="skyblue")
}

c7s_plot <- function(prob_type, hdat){
  prob_names <- c("z", "t", "p")
  
  if(!(prob_type %in% prob_names))
    stop(paste0("Problems must be one of the following: random, ", paste0(prob_names, collapse=", ")))
  pn <- prob_type
  
  if(pn == "z"){
    c7s_zplot(alpha = hdat$alpha)
    
  } else if(pn == "t"){
    c7s_tplot(data=hdat$data, alpha = hdat$alpha)
    
  } else if(pn == "p"){
    c7s_zplot(alpha = hdat$alpha)
  }
}
