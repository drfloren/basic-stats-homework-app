# Header ----
# Description: There are a TON of types of problems for this chapter, especially if I utilize multiple mediums (the goal). I'll start with a single medium, work all of the problems (and all of the solutions) then move on to the next one. Below is a list of problem types and mediums that should be covered.
# Problem Types: Basic, Basic Compliment, Advanced Compliment, And with Replacement, And without Replacement, Or with Mutually Exclusive, Or without Mutually Exclusive, Conditional, and then counting rules
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
# source("General Functions/cards.r")
# source("General Functions/fr.r")
# source("General Functions/run_dup_tally.r")
## Basic Probability ----
c4p_cards_basic <- function(type="random"){
  if(type=="random")
    type <- sample(c("suit", "value"), size=1)
  if(!(type=="suit" | type=="value"))
    stop("type must be random, suit, or value.")
  
  id <- as.character(sample(deck_of_cards[[type]], 1))
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  stem <- paste0("If a single card is drawn, find the probability that a card is a ", id_stem, ".")
  
  list(stem=stem,
       data="",
       hidden_data=list(type=type,
                        id=id))
}

## Basic Complement ----
c4p_cards_bascomp <- function(type="random"){
  if(type=="random")
    type <- sample(c("suit", "value"), size=1)
  if(!(type=="suit" | type=="value"))
    stop("type must be random, suit, or value.")
  
  id <- as.character(sample(deck_of_cards[[type]], 1))
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  stem <- paste0("If a single card is drawn, find the probability that a card is not a ", id_stem, ".")
  
  list(stem=stem,
       data="",
       hidden_data=list(type=type,
                        id=id))
}

## Advanced Complement ----
c4p_cards_advcomp <- function(type="random", n=NULL){
  if(type=="random")
    type <- sample(c("suit", "value"), size=1)
  if(!(type=="suit" | type=="value"))
    stop("type must be random, suit, or value.")
  if(is.null(n))
    n <- sample(2:5, 1)

  id <- as.character(sample(deck_of_cards[[type]], 1))
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  stem <- paste0("If ", n, " cards are drawn without replacement, find the probability that least one ", id_stem, " is drawn.")

  list(stem=stem,
       data="",
       hidden_data=list(type=type,
                        id=id,
                        n=n))
}

## And (with replacement) ----
c4p_cards_and_wr <- function(type="random", n=NULL){
  if(type=="random")
    type <- sample(c("suit", "value"), size=1)
  if(!(type=="suit" | type=="value"))
    stop("type must be random, suit, or value.")
  if(is.null(n))
    n <- sample(2:4, 1)
  
  id <- as.character(sample(deck_of_cards[[type]], n))
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  stem <- paste0("If ", n, " cards are drawn with replacement, find the probability of drawing the following: ", paste(id_stem, collapse=", then a "), " (i.e., P(",paste0(id_stem, collapse=","),"))")
  
  list(stem=stem,
       data="",
       hidden_data=list(type=type,
                        id=id,
                        n=n))
}

## And (without replacement) ----
c4p_cards_and_wor <- function(type="random", n=NULL){
  if(type=="random")
    type <- sample(c("suit", "value"), size=1)
  if(!(type=="suit" | type=="value"))
    stop("type must be random, suit, or value.")
  if(is.null(n))
    n <- sample(2:4, 1)
  
  id <- as.character(sample(deck_of_cards[[type]], n))
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  stem <- paste0("If ", n, " cards are drawn without replacement, find the probability of drawing the following: ", paste(id_stem, collapse=", then a "), " (i.e., P(",paste0(id_stem, collapse=","),"))")
  
  list(stem=stem,
       data="",
       hidden_data=list(type=type,
                        id=id,
                        n=n))
}

## Or (with ME) ----
c4p_cards_or_wme <- function(type="random"){
  if(type=="random")
    type <- sample(c("suit", "value"), size=1)
  if(!(type=="suit" | type=="value"))
    stop("type must be random, suit, or value.")
  
  id <- as.character(sample(unique(deck_of_cards[[type]]), 2))
  id_stem <- id
  if(type=="suit")
    id_stem <- substr(id, start=1, stop=nchar(id)-1)
  stem <- paste0("If a single card is drawn, find the probability of drawing a ", paste(id_stem, collapse=" or a "), " (i.e., P(",paste0(id_stem, collapse=" or "),"))")
  
  list(stem=stem,
       data="",
       hidden_data=list(type=type,
                        id=id))
}

## Or (without ME) ----
c4p_cards_or_wome <- function(){
  type <- c("value", "suit")
  id <- as.character(c(sample(unique(deck_of_cards[[type[1]]]), 1), sample(unique(deck_of_cards[[type[2]]]), 1)))
  id_stem <- id
  id_stem[2] <- substr(id[2], start=1, stop=nchar(id[2])-1)
  stem <- paste0("If a single card is drawn, find the probability of drawing a ", paste(id_stem, collapse=" or a "), " (i.e., P(",paste0(id_stem, collapse=" or "),"))")
  
  list(stem=stem,
       data="",
       hidden_data=list(type=type,
                        id=id))
}

## Card Problem Function ----
c4p_cards <- function(card_prob_type="random", n=NULL, card_opts="random"){
  cds <- sample(1:7, size=1) #7 types of card problems: pick one.
  card_prob_names <- c("basic_prob", "basic_comp", "adv_comp", "and_wr", "and_wor", "or_wme", "or_wome")
  if(card_prob_type=="random"){
    cpn <- card_prob_names[cds] #card, so start with c (so this doesn't get confused later on...)
  } else {
    if(!(card_prob_type %in% card_prob_names))
      stop(paste0("There are not currently card problems of that type. Card problems must be one of the following: random, ", paste0(card_prob_names, collapse=", ")))
    cpn <- card_prob_type
  }
  
  if(cpn == "basic_prob"){
    out <- c4p_cards_basic(type=card_opts)
  } else if (cpn == "basic_comp"){
    out <- c4p_cards_bascomp(type=card_opts)
  } else if (cpn == "adv_comp"){
    out <- c4p_cards_advcomp(type=card_opts, n=n)
  } else if (cpn == "and_wr"){
    out <- c4p_cards_and_wr(type=card_opts, n=n)
  } else if (cpn == "and_wor"){
    out <- c4p_cards_and_wor(type=card_opts, n=n)
  } else if (cpn == "or_wme"){
    out <- c4p_cards_or_wme(type=card_opts)
  } else if (cpn == "or_wome"){
    out <- c4p_cards_or_wome() #no choice here: needs both suit and value...
  }
  out$hidden_data$card_prob_type <- cpn
  out
}










# Empirical ----
# Start with conditional probability, then move on.
c4p_empirical_cond <- function(){
  # can change these for context...
  context <- sample(c("medicine", "business"), 1)
  
  if (context=="medicine"){
    cn <- c("Nurse", "Doctor")
    rn <- c("ER", "Family Practice")
  } else {
    cn <- c("Windows user", "Mac user")
    rn <- c("Sales", "Accounting")
  }
  
  tab <- matrix(sample(1:100, size=4, replace=TRUE), ncol=2)
  rownames(tab) <- rn
  colnames(tab) <- cn
  
  wr <- sample(1:2, 1) #which row
  wc <- sample(1:2, 1) #which column
  wg <- sample(c("r", "c"), 1) #which is given
  
  if(wg=="r"){
    stem <- paste0("Using the data below, find the probability than an individual is a ", cn[wc],", given that they work in the ", rn[wr], " department.")
  } else {
    stem <- paste0("Using the data below, find the probability than an individual works in the ", rn[wr]," department, given that they are a ", cn[wc], ".")
  }
  
  list(stem=stem,
       data=tab,
       hidden_data=list(wr=wr,
                        wc=wc,
                        wg=wg))
}










# Other ----
# These problems will not have any particular context associated with them...
## Arranging ----
c4p_other_arrange <- function(context="random", n=NULL){
  context_options <- c("none", "race", "paper", "guests")
  if(context=="random")
    context <- sample(context_options, size=1)
  if(!(context %in% context_options))
    stop(paste0("context must be one of the following: ", paste0(context_options, collapse=", ")))
  
  if(is.null(n))
    n <- sample(3:12, size = 1)
  
  if(context=="none"){
    stem <- paste0("Calculate how many unique ways you can arrange ", n, " objects.")
  } else if (context=="race") {
    stem <- paste0("If ", n, " runners are participating in a race, determine the number of unique ways they can finish.")
  } else if (context=="paper") {
    stem <- paste0("A professor just dropped all their notes on the ground! If they had ", n, " pieces of paper and picked them up at random, determine the number of arrangements the papers could be in now.")
  } else if (context=="guests") {
    stem <- paste0("A clinic just had ", n, " clients check in online at the exact same time, and need to determine what order to see them. Determine how many unique orders they could see the clients in.")
  }  else {stop("context error in c4p_other_arrange")}
  
  list(stem=stem,
       data="",
       hidden_data=list(n=n))
}

## Sample Space (fundamental counting rule) ----
c4p_other_fcr <- function(n=NULL){
  if(is.null(n))
    n <- sample(2:5, size = 1)
  
  die_faces <- sample((2:10)*2, size=n, replace = TRUE)
  
  stem <- paste0("Consider a case where you are 'rolling' ", n, " die with different numbers of faces (", paste0(die_faces, collapse=", "), ") and observing the outcome. How many different outcomes are possible (i.e., what is the size of the sample space)?")
  
  list(stem=stem,
       data="",
       hidden_data=list(die_faces=die_faces))
}

## Combination ----
c4p_other_combn <- function(context="random", n=NULL){
  context_options <- c("none", "surf", "work", "class")
  if(context=="random")
    context <- sample(context_options, size=1)
  if(!(context %in% context_options))
    stop(paste0("context must be one of the following: ", paste0(context_options, collapse=", ")))
  
  if(is.null(n))
    n <- sample(10:100, size = 1)
  r <- sample(2:10, size=1)
  
  if(context=="none"){
    stem <- paste0("Calculate how many ways you can choose ", r," objects from ", n, " total objects, if order doesn't matter.")
  } else if (context=="surf") {
    stem <- paste0("In a recent local surfing competition, the top ", r," surfers are automatically bumped up to the next level (nationals). If ", n, " total surfers entered the competition, how many different ways can the winners be selected?")
  } else if (context=="work") {
    stem <- paste0("A local business is selecting the top ", r," individuals from sales for a week long cruise. If ", n, " total individuals work in the sales department, how many different ways can the winners be selected?")
  } else if (context=="class") {
    stem <- paste0("We are selecting ", r," individuals from your year to serve as your 'class council' (for reunions and such). If ", n, " total individuals are in your class, how many different ways can the council be selected?")
  }
  
  list(stem=stem,
       data="",
       hidden_data=list(r=r,
                        n=n))
}

## Permutation ----
c4p_other_perm <- function(context="random", n=NULL){
  context_options <- c("none", "surf", "work", "class")
  if(context=="random")
    context <- sample(context_options, size=1)
  if(!(context %in% context_options))
    stop(paste0("context must be one of the following: ", paste0(context_options, collapse=", ")))
  
  if(is.null(n))
    n <- sample(10:100, size = 1)
  r <- sample(2:10, size=1)
  
  if(context=="none"){
    stem <- paste0("Calculate how many ways you can choose ", r," objects from ", n, " total objects, if order matters.")
  } else if (context=="surf") {
    stem <- paste0("In a recent local surfing competition, the top ", r," surfers are automatically bumped up to the next level (professional), with the top placed surfer getting placed in the top tier pro circuit, the second placed getting the second tier pro circuit, etc. If ", n, " total surfers entered the competition, how many different ways can the winners be selected?")
  } else if (context=="work") {
    stem <- paste0("A local business is handing out prizes to the top ", r," individuals from their sales department! The top individual will be given $1000, the second individual will be given $900, and so on. If ", n, " total individuals work in the sales department, how many different ways can the winners be selected?")
  } else if (context=="class") {
    stem <- paste0("We are selecting ", r," individuals from your year to serve as your 'class government officials'. The person with the most votes will serve as 'president', while the person with the second most votes will be 'vice president', and so forth. If ", n, " total individuals are in your class, how many different ways can the government officials be selected?")
  }
  
  list(stem=stem,
       data="",
       hidden_data=list(r=r,
                        n=n))
}























# Complete Problem Function ----
c4p <- function(prob_type="random", n=NULL, card_opts="random", context="random"){
  prob_names <- c("basic_prob", "basic_comp", "adv_comp", "and_wr", "and_wor", "or_wme", "or_wome", "cond", "arrange", "fcr", "combn", "perm")
  ds <- sample(1:length(prob_names), size=1) #pick one type of problem, for if it is random
  if(prob_type=="random"){
    pn <- prob_names[ds] 
  } else {
    if(!(prob_type %in% prob_names))
      stop(paste0("There are not currently problems of that type. Problems must be one of the following: random, ", paste0(prob_names, collapse=", ")))
    pn <- prob_type
  }
  
  if(pn == "basic_prob"){
    out <- c4p_cards_basic(type=card_opts)
  } else if (pn == "basic_comp"){
    out <- c4p_cards_bascomp(type=card_opts)
  } else if (pn == "adv_comp"){
    out <- c4p_cards_advcomp(type=card_opts, n=n)
  } else if (pn == "and_wr"){
    out <- c4p_cards_and_wr(type=card_opts, n=n)
  } else if (pn == "and_wor"){
    out <- c4p_cards_and_wor(type=card_opts, n=n)
  } else if (pn == "or_wme"){
    out <- c4p_cards_or_wme(type=card_opts)
  } else if (pn == "or_wome"){
    out <- c4p_cards_or_wome() #no choice here: needs both suit and value...
  } else if (pn == "cond"){
    out <- c4p_empirical_cond()
  } else if (pn == "arrange"){
    out <- c4p_other_arrange(context=context, n=n)
  } else if (pn == "fcr"){
    out <- c4p_other_fcr(n=n)
  } else if (pn == "combn"){
    out <- c4p_other_combn(context=context, n=n)
  } else if (pn == "perm"){
    out <- c4p_other_perm(context=context, n=n)
  } else {
    stop("Problem type currently not supported in c4p.")
  }
  out$hidden_data$prob_type <- pn
  out
}

























