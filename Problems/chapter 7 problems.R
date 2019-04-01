# Header ----
# Notes: 
# Improvement ideas: eventually, consider generating data using a variety of different stems for different problems?
# Continuous ----
sysbpdg <- function(n){sample(80:120, size=n, replace=TRUE)} #Systolic Blood Pressure
agedg <- function(n){sample(18:38, size=n, replace=TRUE)} #olympic athletes
weightdg <- function(n){sample(60:100, size=n, replace=TRUE)} #kilograms
heightdg <- function(n){sample(157:193, size=n, replace=TRUE)} #centimeters

c7p_t <- function(n=NULL, alpha="random"){
  if(is.null(n))
    n <- sample(9:15, size=1) # dont think this is neeeded: shiny will give default values anyways...
  ds <- sample(1:4, size=1) # 4 sets of problems: pick one.
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n))[[ds]] #do these two first so that we can play around with the direction and alpha without changing the data or the null hypothesis (ie we can look at a problem, then change the direction and alpha as desired while keeping that dataset).
  ralpha <- sample(c(0.01, 0.05, 0.10), size=1) #set this just in case (change later if we are using a fixed one). Setting here allows changes without out affecting randomization at all.
  
  if(alpha=="random"){
    alpha <- ralpha
  } else if(alpha!="random"){ #if alpha isn't random, check that it is right
    alpha <- as.numeric(alpha)
    if(alpha>=1 | alpha<=0) #only need to check if the user sets alpha themselves.
      stop("alpha must be a number between 0 and 1 (usually 0.05).")
  }
  
  stem_start <- c("Consider the data below on systolic blood pressure. Construct a",
                  "Consider the data below on age. Construct a",
                  "Consider the data below on weight in kilograms. Construct a",
                  "Consider the data below on height in centimeters. Construct a")[ds]
  stem_end <- paste0((1-alpha)*100, "% confidence interval for the mean.")
  stem <- paste(stem_start, stem_end)
  
  list(stem=stem, data=dat, hidden_data = list(data=dat, alpha=alpha))
}

c7p_z <- function(n=NULL, alpha="random"){
  if(is.null(n))
    n <- sample(9:15, size=1) # dont think this is neeeded: shiny will give default values anyways...
  ds <- sample(1:4, size=1) # 4 sets of problems: pick one.
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n))[[ds]] #do these two first so that we can play around with the direction and alpha without changing the data or the null hypothesis (ie we can look at a problem, then change the direction and alpha as desired while keeping that dataset).
  sdd <- round(sd(dat))
  ralpha <- sample(c(0.01, 0.05, 0.10), size=1) #set this just in case (change later if we are using a fixed one). Setting here allows changes without out affecting randomization at all.
  
  if(alpha=="random"){
    alpha <- ralpha
  } else if(alpha!="random"){ #if alpha isn't random, check that it is right
    alpha <- as.numeric(alpha)
    if(alpha>=1 | alpha<=0) #only need to check if the user sets alpha themselves.
      stop("alpha must be a number between 0 and 1 (usually 0.05).")
  }
  
  stem_start <- c("Consider the data below on systolic blood pressure. Construct a",
                  "Consider the data below on age. Construct a",
                  "Consider the data below on weight in kilograms. Construct a",
                  "Consider the data below on height in centimeters. Construct a")[ds]
  stem_end <- paste0((1-alpha)*100, "% confidence interval for the mean, assuming the population standard deviation is ", sdd, ".")
  stem <- paste(stem_start, stem_end)
  
  list(stem=stem, data=dat, hidden_data = list(data=dat, alpha=alpha, sdd=sdd))
}

c7p_p <- function(n=NULL, alpha="random"){
  if(is.null(n))
    n <- sample(9:15, size=1) # dont think this is neeeded: shiny will give default values anyways...
  ds <- sample(1:4, size=1, prob = c(.33,.33,.01,.33)) # 4 sets of problems: pick one (reduced weight on the Dr. Floren being the best prof ever)
  x <- round(runif(1, min=.3, max=.7)*n) # population proportion
  ralpha <- sample(c(0.01, 0.05, 0.10), size=1) #set this just in case (change later if we are using a fixed one). Setting here allows changes without out affecting randomization at all.
  
  if(alpha=="random"){
    alpha <- ralpha
  } else if(alpha!="random"){ #if alpha isn't random, check that it is right
    alpha <- as.numeric(alpha)
    if(alpha>=1 | alpha<=0) #only need to check if the user sets alpha themselves.
      stop("alpha must be a number between 0 and 1 (usually 0.05).")
  }
  
  stem_start <- c("Americans were recently asked if they believed that ghosts are real. Construct a",
                  "Americans were recently asked if they believed that marijuana should be legalized. Construct a",
                  "Americans were recently asked if they believed that Dr. Floren is the best prof ever (hey, whats the point of making these problems if I can't do blatant self-promotion?!). Construct a",
                  "Americans were recently asked if they believed that Thor is the best super hero ever. Construct a")[ds]
  stem_end <- paste0((1-alpha)*100, "% confidence interval for the proportion who said yes, assuming that ", x, " said yes.")
  stem <- paste(n, stem_start, stem_end)
  
  list(stem=stem, data="", hidden_data = list(alpha=alpha, n=n, x=x))
}

c7p <- function(prob_type="random", n=NULL, alpha="random"){
  ds <- sample(1:3, size=1) # 3 types of problems: pick one.
  prob_names <- c("z", "t", "p")
  if(prob_type=="random"){
    pn <- prob_names[ds]
  } else {
    if(!(prob_type %in% prob_names))
      stop(paste0("Problems must be one of the following: random, ", paste0(prob_names, collapse=", ")))
    pn <- prob_type
  }
  
  if(pn == "z"){
    out <- c7p_z(n=n, alpha=alpha)
  } else if(pn == "t"){
    out <- c7p_t(n=n, alpha=alpha)
  } else if(pn == "p"){
    out <- c7p_p(n=n, alpha=alpha)
  }
  out$hidden_data$prob_type <- pn
  out
}