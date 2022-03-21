#' @title Closing Line Value
#'
#' @description This function calculates the Closing Line Value (CLV) of your bets.
#'
#' @param bet_odds Odds for your bet(s) (-110, -105)
#' @param closing_odds Closing Odds for the same bet (-132, -115)
#' @param type Type of odds. Possible values are:
#' * `us`, American Odds
#' * `dec`, Decimal Odds
#' * `frac`, Fractional Odds
#'
#' @return clv Closing Line Value of your bet
#'
#' @examples clv_calc(
#'   bet_odds = -110,
#'   closing_odds = -132,
#'   type = "us"
#' )
#' @examples clv_calc(
#'   bet_odds = 2.5,
#'   closing_odds = 2.3,
#'   type = "dec"
#' )
#' @examples clv_calc(
#'   bet_odds = 50/1,
#'   closing_odds = 20/1,
#'   type = "frac"
#' )
#'
#' @export
clv_calc <- function(bet_odds, closing_odds, type = "us") {
  ## Error handling
  if (!is.numeric(bet_odds)) {
    stop("Bet Odds must be numeric")
  }
  if (!is.numeric(closing_odds)) {
    stop("Closing Odds must be numeric")
  }
  if (!type %in% c("us", "frac", "dec", "prob")) {
    stop("type must be either: ('us', 'dec', 'frac', or 'prob')")
  }
  ## American Odds
  if (type == "us") {
    bet_prob <- implied_prob(bet_odds, type = "us")
    close_prob <- implied_prob(closing_odds, type = "us")
  }

  ## Decimal Odds
  if (type == "dec") {
    bet_prob <- implied_prob(bet_odds, type = "dec")
    close_prob <- implied_prob(closing_odds, type = "dec")
  }
  ## Fractional Odds
  if (type == "frac") {
    bet_prob <- implied_prob(bet_odds, type = "frac")
    close_prob <- implied_prob(closing_odds, type = "frac")
  }
  ## Probabilities
  if (type == "prob") {
    bet_prob <- implied_prob(bet_odds, type = "frac")
    close_prob <- implied_prob(closing_odds, type = "frac")
  }

  ## the magic
  clv <- (close_prob - bet_prob) / bet_prob

  return(clv)
}


