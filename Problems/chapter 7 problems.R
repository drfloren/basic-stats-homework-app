# Header ----
# Notes: 
# Improvement ideas: eventually, consider generating data using a variety of different stems for different problems?
# Continuous ----
sysbpdg <- function(n){sample(80:120, size=n, replace=TRUE)} #Systolic Blood Pressure
agedg <- function(n){sample(18:38, size=n, replace=TRUE)} #olympic athletes
weightdg <- function(n){sample(60:100, size=n, replace=TRUE)} #kilograms
heightdg <- function(n){sample(157:193, size=n, replace=TRUE)} #centimeters

c7p <- function(n=NULL, alpha="random"){
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



