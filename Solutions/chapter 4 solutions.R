# Header ----
# Description: There are a TON of types of problems for this chapter, especially if I utilize multiple mediums (the goal). I'll start with a single medium, work all of the problems (and all of the solutions) then move on to the next one. Below is a list of problem types and mediums that should be covered.
# Problem Types: Basic, Basic Compliment, Advanced Compliment, And with Replacement, And without Replacement, Or with Mutually Exclusive, Or without Mutually Exclusive, Conditional
# Mediums (or context): Cards in a Deck, Balls in an Urn, Coins (when appropriate), Die (when appropriate), Empirical

# Actually, lets make a matrix of when things are appropriate:
approp <- structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                      TRUE, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, TRUE, TRUE, 
                      NA, NA, NA, NA, TRUE, TRUE, TRUE, TRUE, NA, TRUE, NA, NA, TRUE, 
                      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(8L, 5L), .Dimnames = list(
                        c("Basic", "Basic Complement", "Advanced Complement", "And w/ Rep", 
                          "And w/o Rep", "Or w/ ME", "Or w/o ME", "Conditional"), c("Cards", 
                                                                                    "Balls", "Coins", "Die", "Empirical")))

# Cards ----
## Basic Probability ----
c4s_cards_basic <- function(type, id){
  list(`Equation`=paste0("(#Success)/(#Total)"),
       `Fraction`= paste0(sum(deck_of_cards[[type]]==id),"/52"),
       `Reduced Fraction`= paste0(MASS::fractions(sum(deck_of_cards[[type]]==id)/52)),
       `Probability`= fr(sum(deck_of_cards[[type]]==id)/52, 4))
}

## Basic Complement ----
c4s_cards_bascomp <- function(type, id){
  list(`Equation`=paste0("1-P(x)"),
       `Fraction`= paste0(52-sum(deck_of_cards[[type]]==id),"/52"),
       `Reduced Fraction`= paste0(MASS::fractions((52-sum(deck_of_cards[[type]]==id))/52)),
       `Probability`= fr((52-sum(deck_of_cards[[type]]==id))/52, 4))
}

## Advanced Complement ----
c4s_cards_advcomp <- function(type, id, n){
  num <- seq(from=(52-sum(deck_of_cards[[type]]==id)), by=-1, length=n) #numerator and denominator, as vector
  den <- seq(from=52, by=-1, length=n)
  
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  
  list(`Equation`=paste0("1-P(no ", id_stem,"s)"),
       `Probability of Complement`= paste0(paste0("P(no ", id_stem,"s)"), " = ", paste0("(", paste0(paste0(num, "/", den), collapse=")*("), ")"), " = ", prod(num), "/", prod(den)),
       `Fraction`= paste0("1-(",prod(num), "/", prod(den), ") = ", prod(den)-prod(num), "/", prod(den)),
       `Reduced Fraction`= paste0(MASS::fractions(1-prod(num)/prod(den), max.denominator=1e7)),
       `Probability`= fr(1-prod(num)/prod(den), 4))
}

## And (with replacement) ----
c4s_cards_and_wr <- function(type, id, n){
  num <- numeric(length=n)
  for(i in 1:n)
    num[i] <- sum(deck_of_cards[[type]]==id[i])
  den <- rep(52, n)
  
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  
  list(`Equation`=paste0("P(",paste0(id_stem, collapse=","),")"),
       `Work`= paste0("(", paste0(paste0(num, "/", den), collapse=")*("), ")"),
       `Fraction`= paste0(prod(num), "/", prod(den)),
       `Reduced Fraction`= paste0(MASS::fractions(prod(num)/prod(den), max.denominator=1e7)),
       `Probability`= fr(prod(num)/prod(den), 4))
}

## And (without replacement) ----
c4s_cards_and_wor <- function(type, id, n){
  num <- numeric(length=n)
  for(i in 1:n)
    num[i] <- sum(deck_of_cards[[type]]==id[i])
  num <- num - (run_dup_tally(id)-1) #dup tally base is 1, so -1 to make base 0 (i.e., don't remove anything unless its the duplicate)
  den <- seq(from=52, by=-1, length=n)
  
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  
  list(`Equation`=paste0("P(",paste0(id_stem, collapse=","),")"),
       `Work`= paste0("(", paste0(paste0(num, "/", den), collapse=")*("), ")"),
       `Fraction`= paste0(prod(num), "/", prod(den)),
       `Reduced Fraction`= paste0(MASS::fractions(prod(num)/prod(den), max.denominator=1e7)),
       `Probability`= fr(prod(num)/prod(den), 4))
}

## Or (with ME) ----
c4s_cards_or_wme <- function(type, id){
  num <- numeric(length=2) #only ever 2 cards
  for(i in 1:2)
    num[i] <- sum(deck_of_cards[[type]]==id[i])
  den <- rep(52,2)
  
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  
  list(`Equation`=paste0("P(",paste0(id_stem, collapse=" or "),")"),
       `Work`= paste0("(", paste0(paste0(num, "/", den), collapse=")+("), ")"),
       `Fraction`= paste0(sum(num), "/52"),
       `Reduced Fraction`= paste0(MASS::fractions(sum(num)/52)),
       `Probability`= fr(sum(num)/52, 4))
}

## Or (without ME) ----
c4s_cards_or_wome <- function(type, id){
  num <- numeric(length=2) #only ever 2 cards
  for(i in 1:2)
    num[i] <- sum(deck_of_cards[[type[i]]]==id[i])
  den <- rep(52,2)
  
  id_stem <- id
  id_stem[2] <- substr(id[2], start=1, stop=nchar(id[2])-1)
  
  list(`Equation`=paste0("P(",paste0(id_stem, collapse=" or "),")"),
       `Work`= paste0("(", paste0(paste0(num, "/", den), collapse=")+("), ")-(1/52)"),
       `Fraction`= paste0(sum(num)-1, "/52"),
       `Reduced Fraction`= paste0(MASS::fractions((sum(num)-1)/52)),
       `Probability`= fr((sum(num)-1)/52, 4))
}

## Card Solution Function ----
c4s_cards <- function(prob_obj){ #get the list of hidden data and, depending on the problem type, use whatever parts are expected...
  card_prob_type <- prob_obj$hidden_data$card_prob_type
  hdat <- prob_obj$hidden_data
  
  if(card_prob_type == "basic_prob"){
    out <- c4s_cards_basic(type=hdat$type, id=hdat$id)
  } else if (card_prob_type == "basic_comp"){
    out <- c4s_cards_bascomp(type=hdat$type, id=hdat$id)
  } else if (card_prob_type == "adv_comp"){
    out <- c4s_cards_advcomp(type=hdat$type, id=hdat$id, n=hdat$n)
  } else if (card_prob_type == "and_wr"){
    out <- c4s_cards_and_wr(type=hdat$type, id=hdat$id, n=hdat$n)
  } else if (card_prob_type == "and_wor"){
    out <- c4s_cards_and_wor(type=hdat$type, id=hdat$id, n=hdat$n)
  } else if (card_prob_type == "or_wme"){
    out <- c4s_cards_or_wme(type=hdat$type, id=hdat$id)
  } else if (card_prob_type == "or_wome"){
    out <- c4s_cards_or_wome(type=hdat$type, id=hdat$id) #no choice here: needs both suit and value...
  } else {
    stop("Problem type is not currently supported by c4s_cards.")
  }
  out
}










# Empirical ----
# Start with conditional probability, then move on.
c4s_empirical_cond <- function(tab, wr, wc, wg){
  cn <- colnames(tab)
  rn <- rownames(tab)
  
  if(wg=="r"){
    equation <- paste0("P(", cn[wc], "|", rn[wr],")")
    work <- paste0("n(", cn[wc], " and ", rn[wr],") / n(", rn[wr], ") = ", tab[wr,wc], "/", sum(tab[wr,]))
    answer <- tab[wr,wc]/sum(tab[wr,])
  } else if (wg=="c") {
    equation <- paste0("P(", rn[wr], "|", cn[wc],")")
    work <- paste0("n(", rn[wr], " and ", cn[wc],") / n(", cn[wc], ") = ", tab[wr,wc], "/", sum(tab[,wc]))
    answer <- tab[wr,wc]/sum(tab[,wc])
  } else {
    stop("error in wg entry to c4s_empirical_cond")
  }
  
  list(`Equation`= equation,
       `Work`= work,
       `Answer`= fr(answer, dig = 4))
}
# foo <- c4p_empirical_cond(); foo; c4s_empirical_cond(foo$data, foo$hidden_data$wr, foo$hidden_data$wc, foo$hidden_data$wg)










# Other ----
# These problems will note have any particular context associated with them...
## Arranging ----
c4s_other_arrange <- function(n){
  list(`Equation`=paste0("n!"),
       `Work`= paste0(n,"!"),
       `Answer`= factorial(n))
}
foo <- c4p_other_arrange(); foo; c4s_other_arrange(foo$hidden_data$n)

## Sample Space (fundamental counting rule) ----
c4s_other_fcr <- function(die_faces){
  n <- length(die_faces)

  list(`Equation`= paste0("n(S) = (", paste0(paste0("k_", 1:n), collapse=")*("), ")"),
       `Work`= paste0("n(S) = (", paste0(die_faces, collapse=")*("), ")"),
       `Answer`= prod(die_faces))
}
foo <- c4p_other_fcr(); foo; c4s_other_fcr(foo$hidden_data$die_faces)

## Combination ----
c4s_other_combn <- function(n, r){
  list(`Equation`=paste0("nCr = (n!) / ((n-r)!*r!)"),
       `Work`= paste0(n, "C", r, " = (", n, "!) / ((", n,"-", r,")!*", r,"!)"),
       `Answer`= choose(n, r))
}

## Permutation ----
c4s_other_perm <- function(n, r){
  list(`Equation`=paste0("nPr = (n!) / (n-r)!"),
       `Work`= paste0(n, "P", r, " = (", n, "!) / (", n,"-", r,")!"),
       `Answer`= choose(n, r) * factorial(r))
}































# Complete Solution Function ----
c4s <- function(prob_obj){ #get the list of hidden data and, depending on the problem type, use whatever parts are expected...
  prob_type <- prob_obj$hidden_data$prob_type
  hdat <- prob_obj$hidden_data
  
  if(prob_type == "basic_prob"){
    out <- c4s_cards_basic(type=hdat$type, id=hdat$id)
  } else if (prob_type == "basic_comp"){
    out <- c4s_cards_bascomp(type=hdat$type, id=hdat$id)
  } else if (prob_type == "adv_comp"){
    out <- c4s_cards_advcomp(type=hdat$type, id=hdat$id, n=hdat$n)
  } else if (prob_type == "and_wr"){
    out <- c4s_cards_and_wr(type=hdat$type, id=hdat$id, n=hdat$n)
  } else if (prob_type == "and_wor"){
    out <- c4s_cards_and_wor(type=hdat$type, id=hdat$id, n=hdat$n)
  } else if (prob_type == "or_wme"){
    out <- c4s_cards_or_wme(type=hdat$type, id=hdat$id)
  } else if (prob_type == "or_wome"){
    out <- c4s_cards_or_wome(type=hdat$type, id=hdat$id) #no choice here: needs both suit and value...
  } else if (prob_type == "cond"){
    out <- c4s_empirical_cond(tab=prob_obj$data, wr=hdat$wr, wc=hdat$wc, wg=hdat$wg)
  } else if (prob_type == "arrange"){
    out <- c4s_other_arrange(n=hdat$n)
  } else if (prob_type == "fcr"){
    out <- c4s_other_fcr(die_faces = hdat$die_faces)
  } else if (prob_type == "combn"){
    out <- c4s_other_combn(n=hdat$n, r=hdat$r)
  } else if (prob_type == "perm"){
    out <- c4s_other_perm(n=hdat$n, r=hdat$r)
  } else {
    stop("Problem type currently not supported in c4s.")
  }
  out
}
# foo <- c4p(); foo; c4s(foo)
















# foo <- problem
# foo
# soln(problem$hidden_data$, )
