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
  
  stem <- paste0("Find the probability that z is ", direction, " ",  z_value, ".")
  
  list(stem=stem, data=NA, hidden_data = list(direction=direction, z_value=z_value))
}

c6p_z2p_word <- function(direction="random"){
  z_value <- round(runif(1,-3,3),2)
  rdirection <- sample(c("greater than", "less than"), size=1) #do this here too (not needed, but just cause so that all randomization is done every time, and fixed values are set later if needed)
  
  if(direction=="random")
    #getting direction (greater than, less than, ne to)
    direction <- rdirection
  if(!(direction %in% c("random", "greater than", "less than"))) 
    stop('Direction must be "random", "greater than", or "less than".')
  
  stem <- paste0("Find the probability that z is ", direction, " ",  z_value, ".")
  
  list(stem=stem, data=NA, hidden_data = list(direction=direction, z_value=z_value))
}



