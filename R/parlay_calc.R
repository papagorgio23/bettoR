#' @title Parlay Calculation
#'
#' @description This function calculates the payout of parlay bets.
#'
#' @param risk Odds for your bet(s)
#' @param odds Vector of odds for each leg of the parlay (-132, -115, -110, -110)
#' @param type Type of odds. Possible values are:
#' * `us`, American Odds
#' * `dec`, Decimal Odds
#' * `frac`, Fractional Odds
#'
#' @return payout Parlay payout
#'
#' @examples parlay_calc(
#'   risk = 75,
#'   odds = c(-110, -110, -110),
#'   type = "us"
#' )
#' @examples parlay_calc(
#'   risk = 50,
#'   odds = c(-110, -110, -110, -110),
#'   type = "us"
#' )
#' @examples parlay_calc(
#'   risk = 100,
#'   odds = c(-110, -150, -175, -325, -220),
#'   type = "us"
#' )
#'
#' @export
parlay_calc <- function(risk, odds, type = "us") {
  ## Error handling
  if (!is.numeric(risk)) {
    stop("Risk amount must be numeric")
  }
  if (!is.numeric(odds)) {
    stop("Odds must be numeric")
  }
  if (!type %in% c("us", "frac", "dec")) {
    stop("type must be either: ('us', 'dec', 'frac')")
  }

  # add the decimal odds of each leg of the parlay
  total_odds <-
    prod(convert_odds(
      odds = odds,
      input = type,
      output = "dec"
    ))

  # the magic
  payout <- round(total_odds * risk) - risk

  return(payout)
}
