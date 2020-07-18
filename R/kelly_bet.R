#' @title Kelly Criterion Bet Size
#'
#' @description This function calculates the Kelly Criterion and returns the dollar amount to bet in order to maximize returns.
#'
#' @param unit_size Unit size of your bankroll, typically 1% of bankroll (100)
#' @param win_prob Probability of winning a bet (0.55)
#' @param odds Odds for a bet (-110)
#' @param type Type of odds. Possible values are:
#' * `us`, American Odds
#' * `dec`, Decimal Odds
#' * `frac`, Fractional Odds
#' @param kelly_type Optional input altering the Kelly formula. Possible values are:
#' * `Half`, Half Kelly
#' * `Quarter`, Quarter Kelly
#' * `Eighth`, Eighth Kelly
#'
#' @return Percentage of bankroll to risk on bet
#'
#' @examples kelly_bet(unit_size = 100, win_prob = 0.58, odds = -132, type = "us")
#' @examples kelly_bet(unit_size = 50, win_prob = 0.53, odds = -105, type = "us", kelly_type = "Half")
#' @examples kelly_bet(unit_size = 500, win_prob = 0.545, odds = 2.1, type = "dec")
#' @examples kelly_bet(unit_size = 300, win_prob = 0.27, odds = 5.5, type = "dec", kelly_type = "Quarter")
#' @examples kelly_bet(unit_size = 250, win_prob = 0.10, odds = 40/1, type = "frac")
#'
#' @references [https://en.wikipedia.org/wiki/Kelly_criterion](https://en.wikipedia.org/wiki/Kelly_criterion)
#'
#' @export
kelly_bet <- function(unit_size, win_prob, odds, type = "dec", kelly_type = "Full"){
  ## Error handling
  if (!is.numeric(unit_size)) {
    stop("Unit size must be numeric")
  }
  if (!is.numeric(win_prob)) {
    stop("Win Probability must be numeric")
  }
  if (win_prob < 0 | win_prob > 1) {
    stop("Win Probability must be between 0-1")
  }
  if (!is.numeric(odds)) {
    stop("Odds must be numeric")
  }
  if (!type %in% c("us", "frac", "dec")){
    stop("type must be either: ('us', 'dec', 'frac')")
  }

  # Convert to fractional odds for the equation
  odds <- as.numeric(convert_odds(odds, input = type, output = "frac"))

  # Kelly Criterion equation
  kelly <- round(((odds * win_prob) - (1 - win_prob)) / odds, 4)

  if (kelly_type == "Half") {
    kelly <- round(kelly / 2, 4)
  }

  if (kelly_type == "Quarter") {
    kelly <- round(kelly / 4, 4)
  }

  if (kelly_type == "Eighth") {
    kelly <- round(kelly / 8, 4)
  }

  bet_size <- round(kelly * unit_size * 100)

  return(bet_size)
}
