# Header ----
# Notes: 
# Improvement ideas: eventually, consider generating data using a variety of different stems for different problems?
# Continuous ----
sysbpdg <- function(n){sample(80:120, size=n, replace=TRUE)} #Systolic Blood Pressure
agedg <- function(n){sample(18:38, size=n, replace=TRUE)} #olympic athletes
weightdg <- function(n){sample(60:100, size=n, replace=TRUE)} #kilograms
heightdg <- function(n){sample(157:193, size=n, replace=TRUE)} #centimeters

c8p <- function(n=NULL, direction="random", alpha="random"){
  if(is.null(n))
    n <- sample(9:15, size=1) # dont think this is neeeded: shiny will give default values anyways...
  ds <- sample(1:4, size=1) # 4 sets of problems: pick one.
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n))[[ds]] #do these two first so that we can play around with the direction and alpha without changing the data or the null hypothesis (ie we can look at a problem, then change the direction and alpha as desired while keeping that dataset).
  h0 <- round(mean(dat) + rt(1, df=2)*sd(dat)/sqrt(length(dat)), 1)
  ralpha <- sample(c(0.01, 0.05, 0.10), size=1) #set this just in case (change later if we are using a fixed one). Setting here allows changes without out affecting randomization at all.
  rdirection <- sample(c("greater than", "less than", "not equal to"), size=1) #do this here too (not needed, but just cause so that all randomization is done every time, and fixed values are set later if needed)
  
  if(direction=="random")
    #getting direction (greater than, less than, ne to)
    direction <- rdirection
  if(!(direction %in% c("random", "greater than", "less than", "not equal to"))) 
    stop('Direction must be "random", "greater than", "less than", or "not equal to".')
  
  if(alpha=="random"){
    alpha <- ralpha
  } else if(alpha!="random"){ #if alpha isn't random, check that it is right
    alpha <- as.numeric(alpha)
    if(alpha>=1 | alpha<=0) #only need to check if the user sets alpha themselves.
      stop("alpha must be a number between 0 and 1 (usually 0.05).")
  }
  
  stem_start <- c("Consider the data below on systolic blood pressure. Test the hypothesis that the population mean is",
                  "Consider the data below on age. Test the hypothesis that the population mean is",
                  "Consider the data below on weight in kilograms. Test the hypothesis that the population mean is",
                  "Consider the data below on height in centimeters. Test the hypothesis that the population mean is")[ds]
  stem_middle <- direction
  stem_end <- paste0(h0, " at the alpha=", alpha, " level.")
  stem <- paste(stem_start, stem_middle, stem_end)
  
  list(stem=stem, data=dat, hidden_data = list(h0=h0, alpha=alpha, direction=direction,  data=dat))
}


# above is a good draft: need to update with t, z, and prop.






