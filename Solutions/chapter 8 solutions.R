

# Z Test ----
## Z Test Steps ----
c8s_ztest_steps <- function(data, sigma=NULL, direction, alpha, nh=0, cv_dig=2, ts_dig=4){
  # checking on df if it is a t-test
  if(is.null(sigma))
    stop("Need population standard deviation for the z-test.")
  
  # Step 1: Stating the Hypothesis
  if(direction=="less than"){
    s1 <- paste0("H0: mu = ", nh, "   vs   H1: mu < ", nh)
  } else if(direction=="greater than"){
    s1 <- paste0("H0: mu = ", nh, "   vs   H1: mu > ", nh)
  } else if(direction=="not equal to"){
    s1 <- paste0("H0: mu = ", nh, "   vs   H1: mu != ", nh)
  }
  
  # Step 2: Finding the Critical Value
  if(direction=="less than"){
    cv <- round(qnorm(alpha), dig=cv_dig)
  } else if(direction=="greater than"){
    cv <- round(qnorm(1-alpha), dig=cv_dig)
  } else if(direction=="not equal to"){
    cv <- round(qnorm(c(alpha/2, 1-alpha/2)), dig=cv_dig)
  }
  
  # Step 3: Computing the Test Statistic
  n <- length(data)
  ts <- round((mean(data) - nh)/sqrt(sigma^2/n), dig=ts_dig)
  
  # Step 4: Make the Decision
  if(direction=="less than"){
    if(ts<cv){
      s4 <- paste0("Since ", ts, " is less than ", cv, ", we reject the null hypothesis.")
    } else {
      s4 <- paste0("Since ", ts, " is not less than ", cv, ", we fail to reject the null hypothesis.")
    }
    
  } else if(direction=="greater than"){
    if(ts>cv){
      s4 <- paste0("Since ", ts, " is greater than ", cv, ", we reject the null hypothesis.")
    } else {
      s4 <- paste0("Since ", ts, " is not greater than ", cv, ", we fail to reject the null hypothesis.")
    }
    
  } else if(direction=="not equal to"){
    if(ts>max(cv)){
      s4 <- paste0("Since ", ts, " is greater than ", max(cv), ", we reject the null hypothesis.")
    } else if (ts<min(cv)){
      s4 <- paste0("Since ", ts, " is less than ", min(cv), ", we reject the null hypothesis.")
    } else {
      s4 <- paste0("Since ", ts, " is between ", max(cv), " and ", min(cv), ", we fail to reject the null hypothesis.")
    }
  }
  
  
  list(`Step 1`=s1, 
       `Step 2`=paste0("Critical Value(s): ", paste0(cv, collapse=", ")), 
       `Step 3`=paste0("Test Statistic: z = (x_bar-mu_0)/(sigma/sqrt(n)) = (", round(mean(data),ts_dig), "-", nh,")/(", sigma, "/sqrt(", n,")) = ", ts), 
       `Step 4`=s4, 
       `Step 5`="Translate Step 4 into a context dependent answer to the research question.")
}
# foo <- c8p_ztest(); foo; z_test_steps(foo$hidden_data$data, foo$hidden_data$sigma, foo$hidden_data$direction, foo$hidden_data$alpha, foo$hidden_data$h0)

## Z Test Plot ----
c8s_ztest_plot <- function(data, sigma=NULL, direction, alpha, nh=0, cv_dig=2, ts_dig=4, tail_exp=1.1, type="z"){
  gt <- "Z-Distribution for the Z-Test\n(reject in shaded region)"
  
  # Keep until ptest plot is made, but these shouldn't be together (as they don't require the same input)
  # if(type=="z"){
  #   gt <- "Z-Distribution for the Z-Test\n(reject in shaded region)"
  # } else if (type=="p"){
  #   gt <- "Z-Distribution for the Test of Proportions\n(reject in shaded region)"
  # }
  
  # Computing the Test Statistic
  n <- length(data)
  ts <- round((mean(data) - nh)/sqrt(sigma^2/n), dig=ts_dig)
  
  if(ts < qnorm(alpha/2) | ts > qnorm(1-alpha/2)){
    lower_bound <- -abs(ts)*tail_exp
    upper_bound <- abs(ts)*tail_exp
    x <- seq(lower_bound, upper_bound, by=.01)
    y <- dnorm(x)
  } else{
    lower_bound <- qnorm(alpha/2)*tail_exp
    upper_bound <- qnorm(1-alpha/2)*tail_exp
    x <- seq(lower_bound, upper_bound, by=.01)
    y <- dnorm(x)
  }

  #base plot
  if(abs(ts)<0.3){ #if we get too close to the middle, need to extend the plot upwards so the label appears...
    plot(x, y, main=gt, type="l", xlab="t", ylab="Density", ylim=c(0,max(y)*1.1))
  } else {
    plot(x, y, main=gt, type="l", xlab="t", ylab="Density")
  }
  
  #shading area
  if(direction!="not equal to"){
    if(direction=="greater than"){
      lower_shade <- qnorm(1-alpha)
      upper_shade <- upper_bound
      axis(side=1, at=lower_shade, labels=paste0("CV=", fr(lower_shade,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    } else if (direction=="less than"){
      lower_shade <- lower_bound
      upper_shade <- qnorm(alpha)
      axis(side=1, at=upper_shade, labels=paste0("CV=", fr(upper_shade,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    }
    shade_x <- c(lower_shade, seq(lower_shade, upper_shade, by=.01), upper_shade)
    shade_y <- c(0,dnorm(seq(lower_shade, upper_shade, by=.01)),0)
    polygon(shade_x, shade_y, col="skyblue")
  } else if (direction=="not equal to"){
    lower_shade_l <- lower_bound
    upper_shade_l <- qnorm(alpha/2)
    shade_x_l <- c(lower_shade_l, seq(lower_shade_l, upper_shade_l, by=.01), upper_shade_l)
    shade_y_l <- c(0,dnorm(seq(lower_shade_l, upper_shade_l, by=.01)),0)
    
    lower_shade_u <- qnorm(1-alpha/2)
    upper_shade_u <- upper_bound
    shade_x_u <- c(lower_shade_u, seq(lower_shade_u, upper_shade_u, by=.01), upper_shade_u)
    shade_y_u <- c(0,dnorm(seq(lower_shade_u, upper_shade_u, by=.01)),0)
    
    axis(side=1, at=upper_shade_l, labels=paste0("CV=", fr(upper_shade_l,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    axis(side=1, at=lower_shade_u, labels=paste0("CV=", fr(lower_shade_u,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    
    polygon(shade_x_l, shade_y_l, col="skyblue")
    polygon(shade_x_u, shade_y_u, col="skyblue")
  }
  
  #plotting ts
  ts_col <- "red"
  segments(x0=ts, y=0, x1=ts, y1=dnorm(ts), col=ts_col)
  text(x=ts, y=dnorm(ts), labels = paste0("TS = ", round(ts,2)), col=ts_col, offset = 1, pos=3)
}
# foo <- c8p_ztest(); foo; c8s_ztest_plot(foo$hidden_data$data, foo$hidden_data$sigma, foo$hidden_data$direction, foo$hidden_data$alpha, foo$hidden_data$h0)









# T Test ----
## T Test Steps ----
c8s_ttest_steps <- function(data, direction, alpha, nh=0, cv_dig=2, ts_dig=4){
  n <- length(data)
  df <- n-1
  
  # Step 1: Stating the Hypothesis
  if(direction=="less than"){
    s1 <- paste0("H0: mu = ", nh, "   vs   H1: mu < ", nh)
  } else if(direction=="greater than"){
    s1 <- paste0("H0: mu = ", nh, "   vs   H1: mu > ", nh)
  } else if(direction=="not equal to"){
    s1 <- paste0("H0: mu = ", nh, "   vs   H1: mu != ", nh)
  }
  
  # Step 2: Finding the Critical Value
  if(direction=="less than"){
    cv <- round(qt(alpha, df=df), dig=cv_dig)
  } else if(direction=="greater than"){
    cv <- round(qt(1-alpha, df=df), dig=cv_dig)
  } else if(direction=="not equal to"){
    cv <- round(qt(c(alpha/2, 1-alpha/2), df=df), dig=cv_dig)
  }
  
  # Step 3: Computing the Test Statistic
  ts <- round((mean(data) - nh)/sqrt(var(data)/n), dig=ts_dig)
  
  # Step 4: Make the Decision
  if(direction=="less than"){
    if(ts<cv){
      s4 <- paste0("Since ", ts, " is less than ", cv, ", we reject the null hypothesis.")
    } else {
      s4 <- paste0("Since ", ts, " is not less than ", cv, ", we fail to reject the null hypothesis.")
    }
    
  } else if(direction=="greater than"){
    if(ts>cv){
      s4 <- paste0("Since ", ts, " is greater than ", cv, ", we reject the null hypothesis.")
    } else {
      s4 <- paste0("Since ", ts, " is not greater than ", cv, ", we fail to reject the null hypothesis.")
    }
    
  } else if(direction=="not equal to"){
    if(ts>max(cv)){
      s4 <- paste0("Since ", ts, " is greater than ", max(cv), ", we reject the null hypothesis.")
    } else if (ts<min(cv)){
      s4 <- paste0("Since ", ts, " is less than ", min(cv), ", we reject the null hypothesis.")
    } else {
      s4 <- paste0("Since ", ts, " is between ", max(cv), " and ", min(cv), ", we fail to reject the null hypothesis.")
    }
  }
  
  
  list(`Step 1`=s1, 
       `Step 2`=paste0("Critical Value(s): ", paste0(cv, collapse=", ")), 
       `Step 3`=paste0("Test Statistic: t = (x_bar-mu_0)/(s/sqrt(n)) = (", round(mean(data),ts_dig), "-", nh,")/(", round(sd(data), ts_dig), "/sqrt(", n,")) = ", ts), 
       `Step 4`=s4, 
       `Step 5`="Translate Step 4 into a context dependent answer to the research question.")
}
# foo <- c8p_ttest(); foo; c8s_ttest_steps(foo$hidden_data$data, foo$hidden_data$direction, foo$hidden_data$alpha, foo$hidden_data$h0)

## T Test Plot ----
c8s_ttest_plot <- function(data, direction, alpha, nh=0, cv_dig=2, ts_dig=4, tail_exp=1.1, type="z"){
  gt <- "T-Distribution for the T-Test\n(reject in shaded region)"
  
  # Computing the Test Statistic
  n <- length(data)
  df <- n-1
  ts <- round((mean(data) - nh)/sqrt(var(data)/n), dig=ts_dig)
  
  if(ts < qt(alpha/2, df=df) | ts > qt(1-alpha/2, df=df)){
    lower_bound <- -abs(ts)*tail_exp
    upper_bound <- abs(ts)*tail_exp
    x <- seq(lower_bound, upper_bound, by=.01)
    y <- dt(x, df=df)
  } else{
    lower_bound <- qt(alpha/2, df=df)*tail_exp
    upper_bound <- qt(1-alpha/2, df=df)*tail_exp
    x <- seq(lower_bound, upper_bound, by=.01)
    y <- dt(x, df=df)
  }
  
  #base plot
  if(abs(ts)<0.3){ #if we get too close to the middle, need to extend the plot upwards so the label appears...
    plot(x, y, main=gt, type="l", xlab="t", ylab="Density", ylim=c(0,max(y)*1.1))
  } else {
    plot(x, y, main=gt, type="l", xlab="t", ylab="Density")
  }
  
  #shading area
  if(direction!="not equal to"){
    if(direction=="greater than"){
      lower_shade <- qt(1-alpha, df=df)
      upper_shade <- upper_bound
      axis(side=1, at=lower_shade, labels=paste0("CV=", fr(lower_shade,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    } else if (direction=="less than"){
      lower_shade <- lower_bound
      upper_shade <- qt(alpha, df=df)
      axis(side=1, at=upper_shade, labels=paste0("CV=", fr(upper_shade,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    }
    shade_x <- c(lower_shade, seq(lower_shade, upper_shade, by=.01), upper_shade)
    shade_y <- c(0,dt(seq(lower_shade, upper_shade, by=.01), df=df),0)
    polygon(shade_x, shade_y, col="skyblue")
  } else if (direction=="not equal to"){
    lower_shade_l <- lower_bound
    upper_shade_l <- qnorm(alpha/2)
    shade_x_l <- c(lower_shade_l, seq(lower_shade_l, upper_shade_l, by=.01), upper_shade_l)
    shade_y_l <- c(0,dt(seq(lower_shade_l, upper_shade_l, by=.01), df=df),0)
    
    lower_shade_u <- qnorm(1-alpha/2)
    upper_shade_u <- upper_bound
    shade_x_u <- c(lower_shade_u, seq(lower_shade_u, upper_shade_u, by=.01), upper_shade_u)
    shade_y_u <- c(0,dt(seq(lower_shade_u, upper_shade_u, by=.01), df=df),0)
    
    axis(side=1, at=upper_shade_l, labels=paste0("CV=", fr(upper_shade_l,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    axis(side=1, at=lower_shade_u, labels=paste0("CV=", fr(lower_shade_u,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    
    polygon(shade_x_l, shade_y_l, col="skyblue")
    polygon(shade_x_u, shade_y_u, col="skyblue")
  }
  
  #plotting ts
  ts_col <- "red"
  segments(x0=ts, y=0, x1=ts, y1=dt(ts, df=df), col=ts_col)
  text(x=ts, y=dt(ts, df=df), labels = paste0("TS = ", round(ts,2)), col=ts_col, offset = 1, pos=3)
}
# foo <- c8p_ttest(); foo; c8s_ttest_plot(foo$hidden_data$data, foo$hidden_data$direction, foo$hidden_data$alpha, foo$hidden_data$h0)










# P Test ----
## P Test Steps ----
c8s_ptest_steps <- function(n, x, direction, alpha, nh, cv_dig=2, ts_dig=4){
  p <- x/n
  
  # checking that the null hypothesis is between 0 and 1
  if (!(0 <= nh | nh <= 1))
    stop("The variable nh must be between 0 and 1.")
  
  # Step 1: Stating the Hypothesis
  if(direction=="less than"){
    s1 <- paste0("H0: p = ", nh, "   vs   H1: p < ", nh)
  } else if(direction=="greater than"){
    s1 <- paste0("H0: p = ", nh, "   vs   H1: p > ", nh)
  } else if(direction=="not equal to"){
    s1 <- paste0("H0: p = ", nh, "   vs   H1: p != ", nh)
  }
  
  # Step 2: Finding the Critical Value
  if(direction=="less than"){
    cv <- round(qnorm(alpha), dig=cv_dig)
  } else if(direction=="greater than"){
    cv <- round(qnorm(1-alpha), dig=cv_dig)
  } else if(direction=="not equal to"){
    cv <- round(qnorm(c(alpha/2, 1-alpha/2)), dig=cv_dig)
  }
  
  # Step 3: Computing the Test Statistic
  ts <- round((p - nh)/sqrt(nh*(1-nh)/n), dig=ts_dig)
  
  # Step 4: Make the Decision
  if(direction=="less than"){
    if(ts<cv){
      s4 <- paste0("Since ", ts, " is less than ", cv, ", we reject the null hypothesis.")
    } else {
      s4 <- paste0("Since ", ts, " is not less than ", cv, ", we fail to reject the null hypothesis.")
    }
    
  } else if(direction=="greater than"){
    if(ts>cv){
      s4 <- paste0("Since ", ts, " is greater than ", cv, ", we reject the null hypothesis.")
    } else {
      s4 <- paste0("Since ", ts, " is not greater than ", cv, ", we fail to reject the null hypothesis.")
    }
    
  } else if(direction=="not equal to"){
    if(ts>max(cv)){
      s4 <- paste0("Since ", ts, " is greater than ", max(cv), ", we reject the null hypothesis.")
    } else if (ts<min(cv)){
      s4 <- paste0("Since ", ts, " is less than ", min(cv), ", we reject the null hypothesis.")
    } else {
      s4 <- paste0("Since ", ts, " is between ", max(cv), " and ", min(cv), ", we fail to reject the null hypothesis.")
    }
  }
  
  list(`Step 1`=s1, 
       `Step 2`=paste0("Critical Value(s): ", paste0(cv, collapse=", ")), 
       `Step 3`=paste0("Test Statistic: z = (p-p_0)/(sqrt(p0*(1-p0)/n)) = (", fr(p,4), "-", nh,")/(sqrt(", nh,"*(1-", nh,")/", n, ")) = ", ts), 
       `Step 4`=s4, 
       `Step 5`="Translate Step 4 into a context dependent answer to the research question.")
}
# foo <- c8p_ptest(); foo; c8s_ptest_steps(foo$hidden_data$n, foo$hidden_data$x, foo$hidden_data$direction, foo$hidden_data$alpha, foo$hidden_data$p0)

## P Test Plot ----
c8s_ptest_plot <- function(n, x, direction, alpha, nh, cv_dig=2, ts_dig=4, tail_exp=1.1, type="z"){
  gt <- "Z-Distribution for the Test of Proportions\n(reject in shaded region)"
  
  # Computing the Test Statistic
  p <- x/n
  ts <- round((p - nh)/sqrt(nh*(1-nh)/n), dig=ts_dig)
  
  if(ts < qnorm(alpha/2) | ts > qnorm(1-alpha/2)){
    lower_bound <- -abs(ts)*tail_exp
    upper_bound <- abs(ts)*tail_exp
    x <- seq(lower_bound, upper_bound, by=.01)
    y <- dnorm(x)
  } else{
    lower_bound <- qnorm(alpha/2)*tail_exp
    upper_bound <- qnorm(1-alpha/2)*tail_exp
    x <- seq(lower_bound, upper_bound, by=.01)
    y <- dnorm(x)
  }
  
  #base plot
  if(abs(ts)<0.3){ #if we get too close to the middle, need to extend the plot upwards so the label appears...
    plot(x, y, main=gt, type="l", xlab="t", ylab="Density", ylim=c(0,max(y)*1.1))
  } else {
    plot(x, y, main=gt, type="l", xlab="t", ylab="Density")
  }
  
  #shading area
  if(direction!="not equal to"){
    if(direction=="greater than"){
      lower_shade <- qnorm(1-alpha)
      upper_shade <- upper_bound
      axis(side=1, at=lower_shade, labels=paste0("CV=", fr(lower_shade,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    } else if (direction=="less than"){
      lower_shade <- lower_bound
      upper_shade <- qnorm(alpha)
      axis(side=1, at=upper_shade, labels=paste0("CV=", fr(upper_shade,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    }
    shade_x <- c(lower_shade, seq(lower_shade, upper_shade, by=.01), upper_shade)
    shade_y <- c(0,dnorm(seq(lower_shade, upper_shade, by=.01)),0)
    polygon(shade_x, shade_y, col="skyblue")
  } else if (direction=="not equal to"){
    lower_shade_l <- lower_bound
    upper_shade_l <- qnorm(alpha/2)
    shade_x_l <- c(lower_shade_l, seq(lower_shade_l, upper_shade_l, by=.01), upper_shade_l)
    shade_y_l <- c(0,dnorm(seq(lower_shade_l, upper_shade_l, by=.01)),0)
    
    lower_shade_u <- qnorm(1-alpha/2)
    upper_shade_u <- upper_bound
    shade_x_u <- c(lower_shade_u, seq(lower_shade_u, upper_shade_u, by=.01), upper_shade_u)
    shade_y_u <- c(0,dnorm(seq(lower_shade_u, upper_shade_u, by=.01)),0)
    
    axis(side=1, at=upper_shade_l, labels=paste0("CV=", fr(upper_shade_l,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    axis(side=1, at=lower_shade_u, labels=paste0("CV=", fr(lower_shade_u,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    
    polygon(shade_x_l, shade_y_l, col="skyblue")
    polygon(shade_x_u, shade_y_u, col="skyblue")
  }
  
  #plotting ts
  ts_col <- "red"
  segments(x0=ts, y=0, x1=ts, y1=dnorm(ts), col=ts_col)
  text(x=ts, y=dnorm(ts), labels = paste0("TS = ", round(ts,2)), col=ts_col, offset = 1, pos=3)
}
# foo <- c8p_ptest(); foo; c8s_ptest_plot(foo$hidden_data$n, foo$hidden_data$x, foo$hidden_data$direction, foo$hidden_data$alpha, foo$hidden_data$p0)










# Complete Solution Function ----
## Steps Solution Function ----
c8s_steps <- function(prob_obj){
  prob_type <- prob_obj$hidden_data$prob_type
  hdat <- prob_obj$hidden_data
  
  if(prob_type == "ztest"){
    out <- c8s_ztest_steps(data=prob_obj$data, sigma=hdat$sigma, direction=hdat$direction, alpha=hdat$alpha, nh=hdat$h0, cv_dig=2, ts_dig=4)
  } else if (prob_type == "ttest"){
    out <- c8s_ttest_steps(data=prob_obj$data, direction=hdat$direction, alpha=hdat$alpha, nh=hdat$h0, cv_dig=2, ts_dig=4)
  } else if (prob_type == "ptest"){
    out <- c8s_ptest_steps(n=hdat$n, x=hdat$x, direction=hdat$direction, alpha=hdat$alpha, nh=hdat$p0)
  } else {
    stop("Problem type currently not supported in c8s.")
  }
  out
}
# foo <- c8p(); foo; c8s_steps(foo)

## Plot Solution Function ----
c8s_plot <- function(prob_obj){
  prob_type <- prob_obj$hidden_data$prob_type
  hdat <- prob_obj$hidden_data
  
  if(prob_type == "ztest"){
    c8s_ztest_plot(data=prob_obj$data, sigma=hdat$sigma, direction=hdat$direction, alpha=hdat$alpha, nh=hdat$h0, cv_dig=2, ts_dig=4)
  } else if (prob_type == "ttest"){
    c8s_ttest_plot(data=prob_obj$data, direction=hdat$direction, alpha=hdat$alpha, nh=hdat$h0, cv_dig=2, ts_dig=4)
  } else if (prob_type == "ptest"){
    c8s_ptest_plot(n=hdat$n, x=hdat$x, direction=hdat$direction, alpha=hdat$alpha, nh=hdat$p0)
  } else {
    stop("Problem type currently not supported in c8s.")
  }
}
# foo <- c8p(); foo; c8s_plot(foo)


