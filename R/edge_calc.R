#' @title Calculate Edge
#'
#' @description This function will calculate the edge of a given bet based on your predicted win probability of the bet and the bet's odds.
#'
#' @param win_prob Your predicted probability for winning the bet (0-1)
#' @param odds The odds for the given bet
#' @param type Type of odds. Possible values are:
#' * `us`, American Odds
#' * `dec`, Decimal Odds
#' * `frac`, Fractional Odds
#'
#'
#' @return edge The edge that you have for the given bet.
#'
#' @examples edge_calc(0.75, -175, type = "us")
#' @examples edge_calc(0.75, 1.2855, type = "dec")
#' @examples edge_calc(0.75, 3/7, type = "frac")
#' @examples edge_calc(
#'   win_prob = c(0.6, 0.7, 0.52, 0.6),
#'   odds = c(-110, -150, 140, 150),
#'   type = "us"
#' )
#'
#' @export
edge_calc <- function(win_prob, odds, type = "us") {
  if (!is.numeric(win_prob)) {
    stop("Probabilities must be numeric")
  }
  if (!type %in% c("us", "frac", "dec")) {
    stop("Type must be either: 'us' or 'dec' or 'frac'")
  }
  edge <- win_prob - implied_prob(odds, type = type)
  return(edge)
}
