# Header ----
# Notes: 
# Improvement ideas: eventually, consider generating data using a variety of different stems for different problems?
# Continuous ----
sysbpdg <- function(n){sample(80:120, size=n, replace=TRUE)} #Systolic Blood Pressure
agedg <- function(n){sample(18:38, size=n, replace=TRUE)} #olympic athletes
weightdg <- function(n){sample(60:100, size=n, replace=TRUE)} #kilograms
heightdg <- function(n){sample(157:193, size=n, replace=TRUE)} #centimeters

c2cont_randdg <- function(n){
  dat <- list(sysbpdg(n), agedg(n), weightdg(n), heightdg(n))
  ds <- sample(1:length(dat), size=1)
  dat[[ds]]
}

c2contp <- function(n=NULL){
  if(is.null(n))
    n <- sample(15:25, size=1)
  stem <- "Consider the data below on systolic blood pressure, age, weight, or height. Using this data, you may calculate the upper and lower class boundaries and class limits, frequencies, relative frequencies (in fractional, reduced, and/or decimal forms), percentages, cumulative frequencies, and midpoints. Using this information, you can graph frequency histograms, relative frequency histograms, frequency polygons, relative frequency polygons, ogives, and stem-and-leaf plots."
  data <- c2cont_randdg(n)
  list(stem=stem, data=data)
}



# Categorical ----
btdg <- function(n){sample(c("A","B","AB","O"), size=n, replace = TRUE)} # blood type
hcdg <- function(n){sample(c("Yellow", "Brown", "Black", "Red"), size=n, replace = TRUE)} # hair color
posdg <- function(n){sample(c("Gold", "Silver", "Bronze"), size=n, replace = TRUE)} # medals
racedg <- function(n){sample(c("White", "Black", "Hispanic", "Asian"), size=n, replace = TRUE)} # race
gradedg <- function(n){sample(c("A", "B", "C", "D", "F"), size=n, replace = TRUE)}

c2cat_randdg <- function(n){
  dat <- list(btdg(n), hcdg(n), posdg(n), racedg(n), gradedg(n))
  ds <- sample(1:length(dat), size=1)
  dat[[ds]]
}

c2catp <- function(n=NULL){
  if(is.null(n))
    n <- sample(15:25, size=1)
  stem <- "Consider the data below on blood types, hair colors, medals, races, or grades. Using this data, you may calculate the frequencies, relative frequencies (in fractional, reduced, and/or decimal forms), percentages, and midpoints. Using this information, you can graph bar charts, pareto charts, and pie charts."
  data <- c2cat_randdg(n)
  list(stem=stem, data=data)
}












