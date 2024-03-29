#' @title Random Bet Result
#'
#' @description This function simulates a bet
#'
#' @param risk The dollar amount risked to place the bet
#' @param payout The dollar amount the bet pays out if it wins
#' @param num_bets Number of bets to simulate
#' @param win_rate The average expected win rate of the bets (0-1)
#'
#' @return The dollar amount won/lost from the bet
#'
#' @examples random_bet(
#'   risk = 110,
#'   payout = 100,
#'   num_bets = 1,
#'   win_rate = 0.55
#' )
#'
#' @export
random_bet <-
  function(risk = 110,
           payout = 100,
           num_bets = 1,
           win_rate = 0.55) {
    return(sample(
      c(-1 * risk, payout),
      num_bets,
      prob = c(1 - win_rate, win_rate),
      replace = TRUE
    ))
  }
