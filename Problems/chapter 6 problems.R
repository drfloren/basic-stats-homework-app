# Header ----
# Description: 4 types of problems. z test to prob, z test to prob in context, prob to value in context, group mean z test.


# Data ----
sysbpdg <- function(n){sample(80:120, size=n, replace=TRUE)} #Systolic Blood Pressure
agedg <- function(n){sample(18:38, size=n, replace=TRUE)} #olympic athletes
weightdg <- function(n){sample(60:100, size=n, replace=TRUE)} #kilograms
heightdg <- function(n){sample(157:193, size=n, replace=TRUE)} #centimeters




# Problem Generation ----
c6p_z2p <- function(direction="random"){
  z_value <- round(runif(1,-3,3),2)
  rdirection <- sample(c("greater than", "less than"), size=1) #do this here too (not needed, but just cause so that all randomization is done every time, and fixed values are set later if needed)
  
  if(direction=="random")
    #getting direction (greater than, less than, ne to)
    direction <- rdirection
  if(!(direction %in% c("random", "greater than", "less than"))) 
    stop('Direction must be "random", "greater than", or "less than".')
  
  stem <- paste0("Find the probability that z is ", direction, " ",  fr(z_value, 2), ".")
  
  list(stem=stem, data="", hidden_data = list(direction=direction, z_value=z_value))
}

c6p_z2p_word <- function(direction="random"){
  z_value <- runif(1,-2.5,2.5)
  n <- 15 # definitely needed here (can be constant)
  ds <- sample(1:4, size=1) # 4 sets of problems: pick one.
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n))[[ds]] #do these two first so that we can play around with the direction and alpha without changing the data or the null hypothesis (ie we can look at a problem, then change the direction and alpha as desired while keeping that dataset).
  rdirection <- sample(c("greater than", "less than"), size=1) #do this here too (not needed, but just cause so that all randomization is done every time, and fixed values are set later if needed)
  
  if(direction=="random")
    #getting direction (greater than, less than, ne to)
    direction <- rdirection
  if(!(direction %in% c("random", "greater than", "less than"))) 
    stop('Direction must be "random", "greater than" or "less than".')
  
  voi <- c("systolic blood pressure", "age", "weight in kilograms", "height in centimeters")[ds]
  md <- round(mean(dat), 2) #mean of the data
  sdd <- round(sd(dat), 2) #sd of the data
  comp_val <- round(z_value*sdd + md) #comparison value (no decimals)
  
  
  stem <- paste0("Consider ", voi, ". Assume that the American population has an average of ", md, " with a standard deviation of ", sdd, ". Find the percentage of Americans who have a value ", direction, " ", comp_val, ".")
  
  
  list(stem=stem, data="", hidden_data = list(direction=direction, md=md, sdd=sdd, comp_val=comp_val))
}

c6p_z2p_samp <- function(n=NULL, direction="random"){
  if(is.null(n))
    n <- sample(9:15, size=1) # dont think this is neeeded: shiny will give default values anyways...
  z_value <- runif(1,-2.5,2.5)
  ds <- sample(1:4, size=1) # 4 sets of problems: pick one.
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n))[[ds]] #do these two first so that we can play around with the direction and alpha without changing the data or the null hypothesis (ie we can look at a problem, then change the direction and alpha as desired while keeping that dataset).
  rdirection <- sample(c("greater than", "less than"), size=1) #do this here too (not needed, but just cause so that all randomization is done every time, and fixed values are set later if needed)
  
  if(direction=="random")
    #getting direction (greater than, less than, ne to)
    direction <- rdirection
  if(!(direction %in% c("random", "greater than", "less than"))) 
    stop('Direction must be "random", "greater than" or "less than".')
  
  voi <- c("systolic blood pressure", "age", "weight in kilograms", "height in centimeters")[ds]
  md <- round(mean(dat), 2) #mean of the data
  sdd <- round(sd(dat), 2) #sd of the data
  comp_val <- round(z_value*(sdd/sqrt(n)) + md) #comparison value (no decimals)
  
  
  stem <- paste0("Consider ", voi, ". Assume that the American population has an average of ", md, " with a standard deviation of ", sdd, ". If ", n, " Americans are selected at random, find the probability that their mean ", voi, " is ", direction, " ", comp_val, ".")
  
  
  list(stem=stem, data="", hidden_data = list(direction=direction, n=n, md=md, sdd=sdd, comp_val=comp_val))
}

c6p_p2v <- function(direction="random"){
  p <- round(runif(1, min=0.01, max=.30), 2)
  n <- 15 # definitely needed here (can be constant)
  ds <- sample(1:4, size=1) # 4 sets of problems: pick one.
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n))[[ds]] #do these two first so that we can play around with the direction and alpha without changing the data or the null hypothesis (ie we can look at a problem, then change the direction and alpha as desired while keeping that dataset).
  rdirection <- sample(c("greater than", "less than"), size=1) #do this here too (not needed, but just cause so that all randomization is done every time, and fixed values are set later if needed)
  
  if(direction=="random")
    #getting direction (greater than, less than, ne to)
    direction <- rdirection
  if(!(direction %in% c("random", "greater than", "less than"))) 
    stop('Direction must be "random", "greater than" or "less than".')
  
  if(direction=="greater than"){
    pdir <- "top"
  } else {
    pdir <- "bottom"
  }
  
  voi <- c("systolic blood pressure", "age", "weight in kilograms", "height in centimeters")[ds]
  md <- round(mean(dat), 2) #mean of the data
  sdd <- round(sd(dat), 2) #sd of the data
  
  
  stem <- paste0("Consider ", voi, ". Assume that the American population has an average of ", md, " with a standard deviation of ", sdd, ". What value would you have to have to be in the ", pdir, " ", p*100, "%?")
  
  
  list(stem=stem, data="", hidden_data = list(direction=direction, pdir=pdir, md=md, sdd=sdd, p=p))
}

c6p <- function(prob_type="random", n=NULL, direction="random"){
  ds <- sample(1:4, size=1) # 4 types of problems: pick one.
  prob_names <- c("z2p", "z2p_word", "z2p_samp", "p2v")
  if(prob_type=="random"){
    pn <- prob_names[ds]
  } else {
    if(!(prob_type %in% prob_names))
      stop(paste0("Problems must be one of the following: random, ", paste0(prob_names, collapse=", ")))
    pn <- prob_type
  }
  
  if(pn == "z2p"){
    out <- c6p_z2p(direction=direction)
  } else if (pn == "z2p_word"){
    out <- c6p_z2p_word(direction=direction)
  } else if (pn == "z2p_samp"){
    out <- c6p_z2p_samp(n=n, direction=direction)
  } else if (pn == "p2v"){
    out <- c6p_p2v(direction=direction)
  }
  out$hidden_data$prob_type <- pn
  out
}


