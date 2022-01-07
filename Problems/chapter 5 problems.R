# Problem types: 
# - discrete: mean, var, sd (could all be same problem, like in chapter 3)
# - expected value (just do 1 table at a time)
# - binomial: probability of a single event
# - binomial: mean, var, sd of binomial dist (all be same prob, like in c3)

source("General Functions/fr.r")

# Discrete ----
c5p_discrete <- function(){
  nm1 <- sample(2:4,1)
  p <- diff(c(0,sort(runif(nm1, 0, 1)), 1)) #this will add another point, making total length of n
  p <- round(p,2)
  p[nm1+1] <- 1-sum(p[1:nm1]) #addresses any rounding errors, making sure this adds to 1 every time...
  
  x <- 0:nm1
  tab <- data.frame(px=p, x=x) #the one for me to work with
  
  ptab <- t(tab) #the one to display nicely
  rownames(ptab) <- c("P(X)", "X")
  colnames(ptab) <- rep("", nm1+1)
  
  stem <- "For the below discrete probability distribution, calculate the mean, variance, and standard deviation."
  
  list(stem=stem, 
       data=ptab, 
       hidden_data=list(tab=tab))
}

# Expected Value ----
c5p_expect <- function(){
  num_tick <- sample(100:1000, 1)
  cost <- sample(1:10, 1)
  prize <- sample(100:1000, 1)
  
  stem <- paste0("Consider a lottery game where ", num_tick, " tickets are sold at $", cost, " each, and the winner gets a cash prize of $", prize,". How much money would you expect to gain if you purchased a ticket?")
  
  list(stem=stem,
       data="",
       hidden_data=list(num_tick=num_tick,
                        cost=cost,
                        prize=prize))
}

# Single Event Binomial ----
c5p_sebin <- function(){
  context_options <- c("a candy bar for breakfast",
                "a car older than 10 years old",
                "an Apple product",
                "a deep, abiding (and sometime secretive) belief that Thor is the best super hero ever")
  context_probs <- c(rep(0.99/(length(context_options)-1), length(context_options)-1), 0.01) #making the easter egg...
  context <- sample(context_options, prob=context_probs, 1)
  
  prob <- 0
  k <- 1
  while(prob < 0.01){ #make sure that we get reasonable numbers before moving on...
    pn <- sample(5:10, 1) #the n for the probability
    px <- sample(1:(pn-1),1)
    
    n <- sample(5:15, 1)
    x <- sample(4:n, 1)
    
    prob <- dbinom(x,n,prob=px/pn)
    k <- k+1
    if(k>100) #if you do this more than 100 times... shouldn't be an issue: did this a bunch without hitting this...
      stop("data generation error in c5p_sebin")
  }

  stem <- paste0("In a recent study, it was determined that ", px, " out of ", pn," Americans had ", context,". If ", n, " people are selected at random, find the probability that exactly ", x, " will have ", context, ".")
  
  list(stem=stem,
       data="",
       hidden_data=list(p=px/pn,
                        n=n,
                        x=x))
}

# Binomial (mean, var, sd) ----
c5p_binomial <- function(){
  sides <- sample(4:10, 1) # for calculating the probability
  soi <- sample(1:sides, 1)
  n <- sample(5:1000, 1)
  
  stem <- paste0("Consider a case where a ", sides," sided die is rolled ", n, " times. Find the mean, variance, and standard deviation of the number of ", soi,"s rolled.")
  
  list(stem=stem,
       data="",
       hidden_data=list(p=1/sides,
                        n=n))
}

# Complete Problem Function ----
c5p <- function(prob_type="random"){
  prob_names <- c("discrete", "expect", "sebin", "binomial")
  ds <- sample(1:length(prob_names), size=1) #pick one type of problem, for if it is random
  if(prob_type=="random"){
    pn <- prob_names[ds] 
  } else {
    if(!(prob_type %in% prob_names))
      stop(paste0("There are not currently problems of that type. Problems must be one of the following: random, ", paste0(prob_names, collapse=", ")))
    pn <- prob_type
  }
  
  if(pn == "discrete"){
    out <- c5p_discrete()
  } else if (pn == "expect"){
    out <- c5p_expect()
  } else if (pn == "sebin"){
    out <- c5p_sebin()
  } else if (pn == "binomial"){
    out <- c5p_binomial()
  } else {
    stop("Problem type currently not supported in c5p.")
  }
  out$hidden_data$prob_type <- pn
  out
}


