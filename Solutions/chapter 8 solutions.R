# Header ----
# Functions
fr <- function(x, dig=2){
  format(round(x, dig), nsmall = dig)
}

cv_t_plot <- function(direction, alpha, df=NULL, tail_exp=1.1){
  # checking on df if it is a t-test
  if(is.null(df))
    stop("Need degrees of freedom for the t-test.")
  
  gt <- "T-Distribution for the T-Test\n(reject in shaded region)"
  lower_bound <- qt(alpha/2, df=df)*tail_exp
  upper_bound <- qt(1-alpha/2, df=df)*tail_exp
  x <- seq(lower_bound, upper_bound, by=.01)
  y <- dt(x, df=df)
  
  #base plot
  plot(x, y, main=gt, type="l", xlab="t", ylab="Density")
  
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
    upper_shade_l <- qt(alpha/2, df=df)
    shade_x_l <- c(lower_shade_l, seq(lower_shade_l, upper_shade_l, by=.01), upper_shade_l)
    shade_y_l <- c(0,dt(seq(lower_shade_l, upper_shade_l, by=.01), df=df),0)
    
    lower_shade_u <- qt(1-alpha/2, df=df)
    upper_shade_u <- upper_bound
    shade_x_u <- c(lower_shade_u, seq(lower_shade_u, upper_shade_u, by=.01), upper_shade_u)
    shade_y_u <- c(0,dt(seq(lower_shade_u, upper_shade_u, by=.01), df=df),0)
    
    axis(side=1, at=upper_shade_l, labels=paste0("CV=", fr(upper_shade_l,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    axis(side=1, at=lower_shade_u, labels=paste0("CV=", fr(lower_shade_u,2)), col.axis="blue", mgp=c(0,2,0), col="blue")
    
    polygon(shade_x_l, shade_y_l, col="skyblue")
    polygon(shade_x_u, shade_y_u, col="skyblue")
  }
  
  # need to add the critical value line, shading, and maybe a note about the rejection region? Maybe a separate function for plotting the test statistic, which can expand the graphic to make sure it includes where the test statistic is (maybe a zoomable graphic?)
}

cv_z_plot <- function(direction, alpha, tail_exp=1.1, type="z"){
  if(type=="z"){
    gt <- "Z-Distribution for the Z-Test\n(reject in shaded region)"
  } else if (type=="p"){
    gt <- "Z-Distribution for the Test of Proportions\n(reject in shaded region)"
  }
  
  lower_bound <- qnorm(alpha/2)*tail_exp
  upper_bound <- qnorm(1-alpha/2)*tail_exp
  x <- seq(lower_bound, upper_bound, by=.01)
  y <- dnorm(x)
  
  #base plot
  plot(x, y, main=gt, type="l", xlab="t", ylab="Density")
  
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
  
  # need to add the critical value line, shading, and maybe a note about the rejection region? Maybe a separate function for plotting the test statistic, which can expand the graphic to make sure it includes where the test statistic is (maybe a zoomable graphic?)
}

t_test_steps <- function(data, direction, alpha, nh=0, dig=3){
  df <- length(data)-1
  
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
    cv <- round(qt(alpha, df=df), dig=dig)
  } else if(direction=="greater than"){
    cv <- round(qt(1-alpha, df=df), dig=dig)
  } else if(direction=="not equal to"){
    cv <- round(qt(c(alpha/2, 1-alpha/2), df=df), dig=dig)
  }
  
  # Step 3: Computing the Test Statistic
  ts <- round((mean(data) - nh)/sqrt(var(data)/length(data)), dig=dig)
  
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
      s4 <- paste0("Since ", ts, " is between ", max(cv), " and ", min(cv), ", we faile to reject the null hypothesis.")
    }
  }
  
  
  list(`Step 1`=s1, 
       `Step 2`=paste0("Critical Value(s): ", paste0(cv, collapse=", ")), 
       `Step 3`=paste0("Test Statistic: ", ts), 
       `Step 4`=s4, 
       `Step 5`="Translate Step 4 into a context dependent answer to the research question.")
}

# t_test_steps(data=rnorm(15), direction="less than", alpha=.10, nh=0)
# t_test_steps(data=rnorm(15), direction="greater than", alpha=.10, nh=0)
# t_test_steps(data=rnorm(15), direction="not equal to", alpha=.10, nh=0)

z_test_steps <- function(data, sigma=NULL, direction, alpha, nh=0, dig=3){
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
    cv <- round(qnorm(alpha), dig=dig)
  } else if(direction=="greater than"){
    cv <- round(qnorm(1-alpha), dig=dig)
  } else if(direction=="not equal to"){
    cv <- round(qnorm(c(alpha/2, 1-alpha/2)), dig=dig)
  }
  
  # Step 3: Computing the Test Statistic
  ts <- round((mean(data) - nh)/sqrt(sigma^2/length(data)), dig=dig)
  
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
      s4 <- paste0("Since ", ts, " is between ", max(cv), " and ", min(cv), ", we faile to reject the null hypothesis.")
    }
  }
  
  
  list(`Step 1`=s1, 
       `Step 2`=paste0("Critical Value(s): ", paste0(cv, collapse=", ")), 
       `Step 3`=paste0("Test Statistic: ", ts), 
       `Step 4`=s4, 
       `Step 5`="Translate Step 4 into a context dependent answer to the research question.")
}

# z_test_steps(data=rnorm(15), sigma=1, direction="less than", alpha=.10, nh=0)
# z_test_steps(data=rnorm(15), sigma=1, direction="greater than", alpha=.10, nh=0)
# z_test_steps(data=rnorm(15), sigma=1, direction="not equal to", alpha=.10, nh=0)

prop_test_steps <- function(phat, n=NULL, direction, alpha, nh=0.5, dig=3){
  # checking that the null hypothesis is between 0 and 1
  if (!(0 <= nh | nh <= 1))
    stop("The variable nh must be between 0 and 1.")
  
  #checking that we have a sample size
  if (is.null(n))
    stop("The sample size n must be set.")
  
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
    cv <- round(qnorm(alpha), dig=dig)
  } else if(direction=="greater than"){
    cv <- round(qnorm(1-alpha), dig=dig)
  } else if(direction=="not equal to"){
    cv <- round(qnorm(c(alpha/2, 1-alpha/2)), dig=dig)
  }
  
  # Step 3: Computing the Test Statistic
  ts <- round((phat - nh)/sqrt(nh*(1-nh)/n), dig=dig)
  
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
      s4 <- paste0("Since ", ts, " is between ", max(cv), " and ", min(cv), ", we faile to reject the null hypothesis.")
    }
  }
  
  
  list(`Step 1`=s1, 
       `Step 2`=paste0("Critical Value(s): ", paste0(cv, collapse=", ")), 
       `Step 3`=paste0("Test Statistic: ", ts), 
       `Step 4`=s4, 
       `Step 5`="Translate Step 4 into a context dependent answer to the research question.")
}

# prop_test_steps(phat=.3, n=20, alpha=.10, nh=0.5, direction="less than")
# prop_test_steps(phat=.7, n=20, alpha=.10, nh=0.5, direction="greater than")
# prop_test_steps(phat=.5, n=20, alpha=.10, nh=0.5, direction="not equal to")
