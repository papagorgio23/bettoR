#' Probability of Winning Conversion to Odds (American Odds)
#'
#' This function provides the fair odds for a given win probability for a given bet.
#'
#' @param prob Probability of winning a bet
#' @param type Odds Type ("us", "dec", "frac") "us" == American Odds, "dec" == Decimal Odds, "frac" == Fractual Odds
#'
#'
#' @return odds American Odds of that bet
#' @export
#'
#' @examples implied_odds(0.4)
#' @examples implied_odds(0.5238095)
#' @examples implied_odds(c(0.3, 0.2, 0.909, 0.7143), type = "dec")
#' @examples implied_odds(c(0.3, 0.2, 0.95, 0.7), type = "frac")
implied_odds <- function(prob, type = "us"){
  if (!is.numeric(prob)) {
    stop("Probabilities must be numeric")
  }

  if (type == "us") {
    odds <- prob
    odds[] <- NA_real_
    odds[which(prob > 0.5)] <- prob[which(prob > 0.5)] / (1 - prob[which(prob > 0.5)]) * -100
    odds[which(prob <= 0.5)] <- (1 - prob[which(prob <= 0.5)]) / prob[which(prob <= 0.5)] * 100
    odds
  }
  if (type == "dec") {
    odds <- prob
    odds[] <- NA_real_
    odds[which(prob < 1)] <- 1 / prob[which(prob < 1)]
    odds
  }
  if (type == "frac") {
    odds <- prob
    odds[] <- NA_real_
    odds[which(prob < 1)] <- 1 / prob[which(prob < 1)]
    odds <- odds - 1
    odds <- MASS::fractions(odds)
  }
  return(odds)
}
