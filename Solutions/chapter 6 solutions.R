# Header ----
# Description: 4 types of problems. z test to prob, z test to prob in context, prob to value in context, group mean z test.

# Solution Generation ----
c6s_z2p <- function(direction, z_value){
  lt <- !(direction=="greater than")
  list(`Probability` = fr(pnorm(z_value, lower.tail = lt), 4))
}

c6s_z2p_word <- function(direction, md, sdd, comp_val){
  # Step 1: calculate z
  z_value <- (comp_val - md)/sdd
  s1 <- z_value
  
  # Step 2: find the area
  lt <- !(direction=="greater than")
  s2 <- paste0(fr(pnorm(z_value, lower.tail = lt)*100, 2), "%")
  
  list(`Z` = s1, `Percentage`=s2)
}

c6s_z2p_samp <- function(direction, n, md, sdd, comp_val){
  # Step 1: calculate z
  z_value <- (comp_val - md)/(sdd/sqrt(n))
  s1 <- z_value
  
  # Step 2: find the area
  lt <- !(direction=="greater than")
  s2 <- fr(pnorm(z_value, lower.tail = lt), 4)
  
  list(`Z` = s1, `Probability`=s2)
}

c6s_p2v <- function(direction, md, sdd, p){
  # Step 1: calculate z
  lt <- !(direction=="greater than")
  z_value <- qnorm(p, lower.tail = lt)
  s1 <- fr(z_value,2)
  
  # Step 2: find the value
  s2 <- round(sdd*round(z_value,2) + md, 4)
  
  list(`Z` = s1, `Value`=s2)
}

c6s_text <- function(prob_type, hdat){ #get the list of hidden data and, depending on the problem type, use whatever parts are expected...
  prob_names <- c("z2p", "z2p_word", "z2p_samp", "p2v")
  if(!(prob_type %in% prob_names))
    stop(paste0("Problems must be one of the following: ", paste0(prob_names, collapse=", ")))
  
  if(prob_type == "z2p"){
    out <- c6s_z2p(direction=hdat$direction, z_value=hdat$z_value)
  } else if (prob_type == "z2p_word"){
    out <- c6s_z2p_word(direction=hdat$direction, md=hdat$md, sdd=hdat$sdd, comp_val=hdat$comp_val)
  } else if (prob_type == "z2p_samp"){
    out <- c6s_z2p_samp(direction=hdat$direction, n=hdat$n, md=hdat$md, sdd=hdat$sdd, comp_val=hdat$comp_val)
  } else if (prob_type == "p2v"){
    out <- c6s_p2v(direction=hdat$direction, md=hdat$md, sdd=hdat$sdd, p=hdat$p)
  }
  out
}



# Plot Generation ----
#c62_z2p_plot <- 