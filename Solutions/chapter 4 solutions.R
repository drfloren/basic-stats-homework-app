# Header ----
# Description: There are a TON of types of problems for this chapter, especially if I utilize multiple mediums (the goal). I'll start with a single medium, work all of the problems (and all of the solutions) then move on to the next one. Below is a list of problem types and mediums that should be covered.
# Problem Types: Basic, Basic Compliment, Advanced Compliment, And with Replacement, And without Replacement, Or with Mutually Exclusive, Or without Mutually Exclusive, Conditional
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
c4s_cards_basic <- function(type, id){
  list(`Equation`=paste0("(#Success)/(#Total)"),
       `Fraction`= paste0(sum(deck_of_cards[[type]]==id),"/52"),
       `Reduced Fraction`= paste0(MASS::fractions(sum(deck_of_cards[[type]]==id)/52)),
       `Probability`= fr(sum(deck_of_cards[[type]]==id)/52, 4))
}

c4s_cards_bascomp <- function(type, id){
  list(`Equation`=paste0("1-P(x)"),
       `Fraction`= paste0(52-sum(deck_of_cards[[type]]==id),"/52"),
       `Reduced Fraction`= paste0(MASS::fractions((52-sum(deck_of_cards[[type]]==id))/52)),
       `Probability`= fr((52-sum(deck_of_cards[[type]]==id))/52, 4))
}

# c4s_cards_advcomp <- function(type, id, n){
#   list(`Equation`=paste0("1-P(0)"),@
#        `Fraction`= paste0("1-(",52-sum(deck_of_cards[[type]]==id),"/52)^",n, " = 1-", (52-sum(deck_of_cards[[type]]==id))^5, "/", (52^5), " = ", 1-(52-sum(deck_of_cards[[type]]==id))^5/(52^5)),
#        `Reduced Fraction`= paste0(MASS::fractions((52-sum(deck_of_cards[[type]]==id))/52)),
#        `Probability`= fr((52-sum(deck_of_cards[[type]]==id))/52, 4))
# }
# foo <-  c4p_cards_advcomp
# foo$stem
# c4s_cards_advcomp(foo$hidden_data$type, foo$hidden_data$id, foo$hidden_data$n)
# 
# 13 12 11 10 /
#   52 51 50 49

