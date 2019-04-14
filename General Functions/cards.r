deck_of_cards <- data.frame(suit = rep(c("spades", "hearts", "diamonds", "clubs"), 13),
                            value = rep(c(2:10, "jack", "queen", "king", "ace"), each=4))
deck_of_cards <- data.frame(deck_of_cards, card = paste(deck_of_cards$value, "of", deck_of_cards$suit))
deck_of_cards