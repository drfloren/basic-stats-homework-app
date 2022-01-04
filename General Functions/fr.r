fr <- function(x, dig=4){
  format(round(x, digits = dig), nsmall = dig, scientific = F)
}