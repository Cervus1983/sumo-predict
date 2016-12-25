# random guess: picks a number from a beta distribution
randomGuess <- function(train.data, test.data) rbeta(nrow(test.data), 1, 1)
