

# Discrete ----
c5s_discrete <- function(tab){
  
  
  mean <- list(Equation = paste0("Sum of (X*P(x))"),
               Work = paste0("(", paste0(paste0(tab$x, "*", fr(tab$px,2)), collapse=") + ("), ")"),
               Answer = round(sum(tab$x*tab$px), 4))
  
  variance <- list(Equation = paste0("Sum of ((X-mu)^2*P(x))"),
                   Work = paste0("((", paste0(paste0(tab$x, "-", mean$Answer, ")^2*", fr(tab$px,2)), collapse=") + ("), ")"),
                   Answer = round(sum((tab$x-mean$Answer)^2*tab$px), 4))
  
  standard_deviation <- list(Equation = paste0("Square root of Variance"),
                       Work = paste0("Square root of ", variance$Answer),
                       Answer = round(sqrt(variance$Answer), 4))
  
  list(
    Mean = mean,
    Variance = variance,
    `Standard Deviation` = standard_deviation
  )
}
# foo <- c5p_discrete(); foo; c5s_discrete(foo$hidden_data$tab)

# Expected Value ----
c5s_expect <- function(num_tick, cost, prize){
  #win then lose...
  pw <- round(1/num_tick, 4) #probability of a win
  pl <- 1-pw
  gain <- c(prize-cost, -cost)
  
  tab <- data.frame(x=gain, px=c(pw,pl))
  
  ptab <- t(tab)
  ptab[2,] <- c(paste0("1/", num_tick), paste0(num_tick-1, "/", num_tick))
  rownames(ptab) <- c("Gain (X)", "P(X)")
  colnames(ptab) <- c("Win", "Lose")
  
  
  list(Table=ptab,
       Equation = paste0("Sum of (X*P(x))"),
       Work = paste0("(", paste0(paste0(tab$x, "*", fr(tab$px,4)), collapse=") + ("), ")"),
       Answer = paste0("$",round(sum(tab$x*tab$px), 4)))
}
# foo <- c5p_expect(); foo; c5s_expect(foo$hidden_data$num_tick, foo$hidden_data$cost, foo$hidden_data$prize)

# Single Event Binomial ----
c5s_sebin <- function(p, n, x){
  p <- round(p,4)
  list(Equation = paste0("P(X=x) = (n!)/((n-x)!*x!) * p^x * q^(n-x)"),
       Work = paste0("P(X=", x, ") = (", n, "!)/((", n,"-", x,")!*", x,"!) * ", p, "^", x, " * ", 1-p, "^(", n, "-", x, ")"),
       Answer = round(dbinom(x, n, p),4))
}
# foo <- c5p_sebin(); foo; c5s_sebin(foo$hidden_data$p, foo$hidden_data$n, foo$hidden_data$x)

# Binomial (mean, var, sd) ----
c5s_binomial <- function(p, n){
  fullp <- p
  p <- round(p,4)
  
  mean <- list(Equation = paste0("n*p"),
               Work = paste0(n, "*", p),
               Answer = round(n*p, 4))
  
  variance <- list(Equation = paste0("n*p*q"),
                   Work = paste0(n, "*", p, "*", 1-p),
                   Answer = round(n*p*(1-p), 4))
  
  standard_deviation <- list(Equation = paste0("Square root of Variance"),
                       Work = paste0("Square root of ", variance$Answer),
                       Answer = round(sqrt(variance$Answer), 4))
  
  list(Probability = paste0("p = ", MASS::fractions(fullp), " = ", p),
       Mean = mean,
       Variance = variance,
       `Standard Deviation` = standard_deviation)
}
# foo <- c5p_binomial(); foo; c5s_binomial(foo$hidden_data$p, foo$hidden_data$n)

# Complete Solution Function ----
c5s <- function(prob_obj){ #get the list of hidden data and, depending on the problem type, use whatever parts are expected...
  prob_type <- prob_obj$hidden_data$prob_type
  hdat <- prob_obj$hidden_data
  
  if(prob_type == "discrete"){
    out <- c5s_discrete(tab = hdat$tab)
  } else if (prob_type == "expect"){
    out <- c5s_expect(num_tick=hdat$num_tick, cost=hdat$cost, prize=hdat$prize)
  } else if (prob_type == "sebin"){
    out <- c5s_sebin(p=hdat$p, n=hdat$n, x=hdat$x)
  } else if (prob_type == "binomial"){
    out <- c5s_binomial(p=hdat$p, n=hdat$n)
  } else {
    stop("Problem type currently not supported in c5p.")
  }
  out
}
# foo <- c5p(); foo; c5s(foo)


