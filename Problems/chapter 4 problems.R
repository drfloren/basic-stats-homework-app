# Header ----
# Description: There are a TON of types of problems for this chapter, especially if I utilize multiple mediums (the goal). I'll start with a single medium, work all of the problems (and all of the solutions) then move on to the next one. Below is a list of problem types and mediums that should be covered.
# Problem Types: Basic, Basic Compliment, Advanced Compliment, And with Replacement, And without Replacement, Or with Mutually Exclusive, Or without Mutually Exclusive, Conditional, and then counting rules
# Mediums: Cards in a Deck, Balls in an Urn, Coins (when appropriate), Die (when appropriate), Empirical

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
      stop(paste0("Problems must be one of the following: random, ", paste0(card_prob_names, collapse=", ")))
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

d










