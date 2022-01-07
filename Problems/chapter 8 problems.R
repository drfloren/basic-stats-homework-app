# Header ----
# Notes: 
# Improvement ideas: eventually, consider generating data using a variety of different stems for different problems?
# Data ----
sysbpdg <- function(n){sample(80:120, size=n, replace=TRUE)} #Systolic Blood Pressure
agedg <- function(n){sample(18:38, size=n, replace=TRUE)} #olympic athletes
weightdg <- function(n){sample(60:100, size=n, replace=TRUE)} #kilograms
heightdg <- function(n){sample(157:193, size=n, replace=TRUE)} #centimeters
thorawesomenessdg <- function(n){sample(75:100, size=n, replace=TRUE)} #awesomeness


# Z Test ----
c8p_ztest <- function(n=NULL, direction="random", alpha="random"){
  ts_range <- c(-4.5, 4.5) #range of test statistic 
  direction_list <- c("greater than", "less than", "not equal to")
  
  if(is.null(n))
    n <- sample(9:15, size=1)
  
  context_list <- c("systolic blood pressure", "age", "weight", "height", "how amazing people think Thor is") #for stem
  eep <- 0.01 #easter egg probability
  ds <- sample(1:length(context_list), prob = c(rep((1-eep)/(length(context_list)-1), (length(context_list)-1)), eep), size=1) #pick 1 stem/context (used in selecting text and data)
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n), thorawesomenessdg(n))[[ds]] #do these two first so that we can play around with the direction and alpha without changing the data or the null hypothesis (ie we can look at a problem, then change the direction and alpha as desired while keeping that dataset).
  h0 <- round(mean(dat) + runif(1,min(ts_range), max(ts_range))*sd(dat)/sqrt(length(dat)), 1) #this is both positive and negative
  ralpha <- sample(c(0.01, 0.05, 0.10), size=1) #random alpha. set this just in case (change later if we are using a fixed one). Setting here allows changes without out affecting randomization at all.
  rdirection <- sample(direction_list, size=1) #random direction. do this here too (not needed, but just cause so that all randomization is done every time, and fixed values are set later if needed)
  sigma <- round(sd(dat))
  
  if(direction=="random")
    #getting direction (greater than, less than, ne to)
    direction <- rdirection
  if(!(direction %in% direction_list)) 
    stop(paste0("Direction must be: ", paste0(direction_list, collapse=", "), "."))
  
  if(alpha=="random"){
    alpha <- ralpha
  } else if(alpha!="random"){ #if alpha isn't random, check that it is right
    alpha <- as.numeric(alpha)
    if(alpha>=1 | alpha<=0) #only need to check if the user sets alpha themselves.
      stop("alpha must be a number between 0 and 1 (usually 0.05).")
  }
  
  stem_start <- paste0("Consider the data below on ", context_list[ds], ". Test the hypothesis that the population mean is")
  stem_middle <- direction
  stem_end <- paste0(h0, " at the alpha=", alpha, " level.")
  stem_sd <- paste0("Assume the population standard deviation is ", sigma, ".")
  stem <- paste(stem_start, stem_middle, stem_end, stem_sd)
  
  list(stem=stem, data=dat, hidden_data = list(h0=h0, alpha=alpha, direction=direction,  data=dat, sigma=sigma))
}










# T Test ----
c8p_ttest <- function(n=NULL, direction="random", alpha="random"){
  ts_range <- c(-4.5, 4.5) #range of test statistic 
  direction_list <- c("greater than", "less than", "not equal to")
  
  if(is.null(n))
    n <- sample(9:15, size=1)
  
  context_list <- c("systolic blood pressure", "age", "weight", "height", "how amazing people think Thor is") #for stem
  eep <- 0.01 #easter egg probability
  ds <- sample(1:length(context_list), prob = c(rep((1-eep)/(length(context_list)-1), (length(context_list)-1)), eep), size=1) #pick 1 stem/context (used in selecting text and data)
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n), thorawesomenessdg(n))[[ds]] #do these two first so that we can play around with the direction and alpha without changing the data or the null hypothesis (ie we can look at a problem, then change the direction and alpha as desired while keeping that dataset).
  h0 <- round(mean(dat) + runif(1,min(ts_range), max(ts_range))*sd(dat)/sqrt(length(dat)), 1) #this is both positive and negative
  ralpha <- sample(c(0.01, 0.05, 0.10), size=1) #random alpha. set this just in case (change later if we are using a fixed one). Setting here allows changes without out affecting randomization at all.
  rdirection <- sample(direction_list, size=1) #random direction. do this here too (not needed, but just cause so that all randomization is done every time, and fixed values are set later if needed)
  
  if(direction=="random")
    #getting direction (greater than, less than, ne to)
    direction <- rdirection
  if(!(direction %in% direction_list)) 
    stop(paste0("Direction must be: ", paste0(direction_list, collapse=", "), "."))
  
  if(alpha=="random"){
    alpha <- ralpha
  } else if(alpha!="random"){ #if alpha isn't random, check that it is right
    alpha <- as.numeric(alpha)
    if(alpha>=1 | alpha<=0) #only need to check if the user sets alpha themselves.
      stop("alpha must be a number between 0 and 1 (usually 0.05).")
  }
  
  stem_start <- paste0("Consider the data below on ", context_list[ds], ". Test the hypothesis that the population mean is")
  stem_middle <- direction
  stem_end <- paste0(h0, " at the alpha=", alpha, " level.")
  stem <- paste(stem_start, stem_middle, stem_end)
  
  list(stem=stem, data=dat, hidden_data = list(h0=h0, alpha=alpha, direction=direction,  data=dat))
}










# P Test ----
c8p_ptest <- function(direction="random", alpha="random"){
  ts_range <- c(-4.5, 4.5) #range of test statistic 
  direction_list <- c("greater than", "less than", "not equal to")
  
  n <- sample(10:100, 1)
  x <- sample(1:(n-1),1)
  p <- x/n
  
  context_list <- c("a candy bar for breakfast",
                       "a car older than 10 years old",
                       "an Apple product",
                       "a deep, abiding (and sometime secretive) belief that Thor is the best super hero ever")
  eep <- 0.01 #easter egg probability
  ds <- sample(1:length(context_list), prob = c(rep((1-eep)/(length(context_list)-1), (length(context_list)-1)), eep), size=1) #pick 1 stem/context (used in selecting text and data)
  p0 <- round(p + runif(1,min(ts_range), max(ts_range))*sqrt(p*(1-p)/n), 2) #this is both positive and negative; not perfectly accurate, but hopefully close enough (can tweak range if not)
  if(p0>1) p0 <- 2-p0 #1-(p0-1), so how much higher is p0 then 1, then remove that from 1
  if(p0<0) p0 <- abs(p0)
  if(p0==0) p0 <- 0.10 #can't have 0
  if(p0==1) p0 <- 0.90 #can't have 0
  ralpha <- sample(c(0.01, 0.05, 0.10), size=1) #random alpha. set this just in case (change later if we are using a fixed one). Setting here allows changes without out affecting randomization at all.
  rdirection <- sample(direction_list, size=1) #random direction. do this here too (not needed, but just cause so that all randomization is done every time, and fixed values are set later if needed)
  
  if(direction=="random")
    #getting direction (greater than, less than, ne to)
    direction <- rdirection
  if(!(direction %in% direction_list)) 
    stop(paste0("Direction must be: ", paste0(direction_list, collapse=", "), "."))
  
  if(alpha=="random"){
    alpha <- ralpha
  } else if(alpha!="random"){ #if alpha isn't random, check that it is right
    alpha <- as.numeric(alpha)
    if(alpha>=1 | alpha<=0) #only need to check if the user sets alpha themselves.
      stop("alpha must be a number between 0 and 1 (usually 0.05).")
  }
  
  stem_start <- paste0("In a news article, it was reported that ", fr(p0*100,0),"% Americans had ", context_list[ds],". A researcher didn't think this sounded quite right, so they sampled ", n, " people and found that ", x, " people had ", context_list[ds],". Test the hypothesis that the population proportion is")
  stem_middle <- direction
  stem_end <- paste0(p0, " at the alpha=", alpha, " level.")
  stem <- paste(stem_start, stem_middle, stem_end)
  
  list(stem=stem, data="", hidden_data = list(p0=p0, alpha=alpha, direction=direction, n=n, x=x))
}










# Complete Problem Function ----
c8p <- function(prob_type="random", n=NULL, direction="random", alpha="random"){
  prob_names <- c("ztest", "ttest", "ptest")
  ds <- sample(1:length(prob_names), size=1) #pick one type of problem, for if it is random
  if(prob_type=="random"){
    pn <- prob_names[ds] 
  } else {
    if(!(prob_type %in% prob_names))
      stop(paste0("There are not currently problems of that type. Problems must be one of the following: random, ", paste0(prob_names, collapse=", ")))
    pn <- prob_type
  }
  
  if(pn == "ztest"){
    out <- c8p_ztest(n=n, direction=direction, alpha=alpha)
  } else if (pn == "ttest"){
    out <- c8p_ttest(n=n, direction=direction, alpha=alpha)
  } else if (pn == "ptest"){
    out <- c8p_ptest(direction=direction, alpha=alpha)
  } else {
    stop("Problem type currently not supported in c8p.")
  }
  
  out$hidden_data$prob_type <- pn
  out
}





