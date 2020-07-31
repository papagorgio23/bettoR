#' @title Kelly Criterion Calculation
#'
#' @description This function calculates the Kelly Criterion percentage of your bankroll to bet in order to maximize returns.
#'
#' @param win_prob Probability of winning a bet (0.55)
#' @param odds Odds for a bet (-110)
#' @param type Type of odds. Possible values are:
#' * `us`, American Odds
#' * `dec`, Decimal Odds
#' * `frac`, Fractional Odds
#' @param kelly_type Optional input altering the kelly formula. Possible values are:
#' * `half`, Half Kelly
#' * `quarter`, Quarter Kelly
#' * `eighth`, Eighth Kelly
#'
#' @return Percentage of bankroll to risk on bet
#'
#'
#' @examples kelly(win_prob = 0.58, odds = -132, type = "us")
#' @examples kelly(win_prob = 0.53, odds = -105, type = "us", kelly_type = "half")
#' @examples kelly(win_prob = 0.545, odds = 2.1, type = "dec")
#' @examples kelly(win_prob = 0.27, odds = 5.5, type = "dec", kelly_type = "quarter")
#' @examples kelly(win_prob = 0.10, odds = 40/1, type = "frac")
#'
#' @references [Kelly Criterion wikipedia](https://en.wikipedia.org/wiki/Kelly_criterion) page
#'
#' @export
kelly <- function(win_prob, odds, type = "dec", kelly_type = "full"){
  ## Error handling
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

  if (kelly_type == "half") {
    kelly <- round(kelly / 2, 4)
  }

  if (kelly_type == "quarter") {
    kelly <- round(kelly / 4, 4)
  }

  if (kelly_type == "eighth") {
    kelly <- round(kelly / 8, 4)
  }

  return(kelly)
}
