# Header ----
# Notes: 
# Improvement ideas: eventually, consider generating data using a variety of different stems for different problems?
# Continuous ----
sysbpdg <- function(n){sample(80:120, size=n, replace=TRUE)} #Systolic Blood Pressure
agedg <- function(n){sample(18:38, size=n, replace=TRUE)} #olympic athletes
weightdg <- function(n){sample(60:100, size=n, replace=TRUE)} #kilograms
heightdg <- function(n){sample(157:193, size=n, replace=TRUE)} #centimeters

c3randdg <- function(n){
  ds <- sample(1:4, size=1)
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n))
  dat[[ds]]
}

c3p <- function(n=NULL){
  if(is.null(n))
    n <- sample(9:15, size=1) # dont think this is neeeded: shiny will give default values anyways...
  stem <- "Consider the data below on systolic blood pressure, age, weight, or height. Using this data, you may calculate the mean, median, mode, midrange, range, population variance, population standard deviation, sample variance, sample standard deviation, percentiles, deciles, quartiles, the inter-quartile range, outliers, and boxplots."
  data <- c3randdg(n)
  list(stem=stem, data=data)
}
