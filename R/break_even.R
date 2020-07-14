#' Break Even Probability
#'
#' This function returns the break even win percentage needed based on the amount risked and the amount returned on the bet.
#' It also factors in parlay bets with the number of legs needed to convert to payout.
#' Gives you the percent chance you need to break even on the bet
#'
#' @param risk Amount risked on the bet
#' @param rtrn Amount returned on the bet. (Risk + the amount won in the bet)
#' @param legs The number of legs needed to win the bet. (straight bets = 1, parlay >= 2)
#'
#' @return prob
#' @export
#'
#' @examples break_even(110, 210, 1)
#' @examples break_even(50, 750, 4)
break_even <- function(risk, rtrn, legs = 1){
  ## Error handling
  if (!is.numeric(risk)) {
    stop("Risk must be numeric")
  }
  ## Error handling
  if (!is.numeric(rtrn)) {
    stop("rtrn must be numeric")
  }
  ## Error handling
  if (!is.numeric(legs)) {
    stop("Teams must be numeric")
  }
  prob <- (risk / rtrn) ** (1 / legs)
  return(prob)
}
