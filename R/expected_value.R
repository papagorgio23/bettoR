#' @title Expected Value
#'
#' @description This function calculates the expected value for a given bet.
#'
#' @param payout The dollar amount won if the bet pays out (500)
#' @param risk The dollar amount risked (550)
#' @param win_prob The probability that the bet wins (0.54)
#'
#' @return Expected Value of a bet
#'
#' @examples expected_value(100, 110, 0.55)
#' @examples expected_value(175, 100, 0.35)
#'
#' @export
expected_value <- function(payout, risk, win_prob) {
  ## Error handling
  if (!is.numeric(payout)) {
    stop("Payout must be numeric")
  }
  if (!is.numeric(risk)) {
    stop("Risk must be numeric")
  }
  if (!is.numeric(win_prob)) {
    stop("win_prob must be numeric and between 0-1")
  }
  if (win_prob < 0 | win_prob > 1) {
    stop("win_prob must between 0-1")
  }
  exp_value <- ((payout * win_prob) - ((1 - win_prob) * risk))
  return(exp_value)
}
