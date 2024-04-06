play_game <- function() {
  dice1 <- sample(1:6, 1, replace = TRUE)
  dice2 <- sample(1:6, 1, replace = TRUE)
  sum_dice <- dice1 + dice2
  
  if (sum_dice %in% c(7, 11)) {
    return(TRUE)
  } else if (sum_dice %in% c(2, 3, 12)) {
    return(FALSE)
  } else {
    first_roll_sum <- sum_dice
    repeat {
      dice1 <- sample(1:6, 1, replace = TRUE)
      dice2 <- sample(1:6, 1, replace = TRUE)
      sum_dice <- dice1 + dice2
      if (sum_dice == 7) {
        return(FALSE)
      } else if (sum_dice == first_roll_sum) {
        return(TRUE)
      }
    }
  }
}

craps_game <- function(num_simulations, bet_size) {
  wins <- 0
  for (i in 1:num_simulations) {
    if (play_game()) {
      wins <- wins + 1
    }
  }
  
  win_prob <- wins / num_simulations
  
  expected_value <- (win_prob * bet_size) + ((1 - win_prob) * -bet_size)
  
  return(expected_value)
}
