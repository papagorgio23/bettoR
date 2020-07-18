#' Closing Line Value
#'
#' This function calculates the Closing Line Value (CLV) of your bets.
#'
#' @param risk Amount risked for your bet(s) (110, 105)
#' @param odds Odds for the bet (-132, -115)
#' @param type Odds Type ("us", "dec", "frac") "us" == American Odds, "dec" == Decimal Odds, "frac" == Fractual Odds
#'
#' @return payout Closing Line Value of your bet
#'
#' @examples bet_calc(risk = 100, odds = -132, type = "us")
#' @examples bet_calc(risk = 500, odds = 2.3, type = "dec")
#' @examples bet_calc(risk = 25, odds = 20/1, type = "frac")
#'
#' @export
bet_calc <- function(risk, odds, type = "us"){
  ## Error handling
  if (!is.numeric(risk)) {
    stop("Risk amount must be numeric")
  }
  if (!is.numeric(odds)) {
    stop("Odds must be numeric")
  }
  if (!type %in% c("us", "frac", "dec", "prob")){
    stop("type must be either: ('us', 'dec', 'frac')")
  }

  # Convert odds to decimal because decimal is the easiest odds in the world to deal with...
  odds <- convert_odds(odds, input = type, output = "dec")

  ## the magic
  payout <- round(risk * odds, 2)

  return(payout)
}


