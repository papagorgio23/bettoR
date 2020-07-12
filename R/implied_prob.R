#' Implied Probability of Winning (American Odds)
#'
#' This function provides the implied probability or "Break Even" win percentage for a given bet.
#'
#' @param odds American Odds of the bet
#' @param type Odds Type ("us", "dec", "frac") "us" == American Odds, "dec" == Decimal Odds, "frac" == Fractual Odds
#' @return Implied probability of winning or the "Break Even" percentage
#' @export
#'
#' @examples implied_prob(-150)
#' @examples implied_prob(360)
#' @examples implied_prob(c(360, -500, -110, 140))
#' @examples implied_prob(c(3.60, 5.00, 1.10, 1.40), type = "dec")
#' @examples implied_prob(c(5/2, 1/2, 7/1, 20/1, 10/11), type = "frac")
implied_prob <- function(odds, type = "us"){
  if (type == "us") {
    imp_prob <- odds
    imp_prob[] <- NA_real_
    imp_prob[which(odds <= 0)] <- 1 / (1 - 100 / odds[which(odds <= -100)])
    imp_prob[which(odds > 0)] <- 1 / (1 + odds[which(odds >= 100)] / 100)
    imp_prob
  }
  if (type == "dec") {
    imp_prob <- odds
    imp_prob[] <- NA_real_
    imp_prob[which(odds > 1)] <- 1 / odds[which(odds > 1)]
    imp_prob
  }
  if (type == "frac") {
    odds <- odds + 1
    imp_prob <- odds
    imp_prob[] <- NA_real_
    imp_prob[which(odds > 1)] <- 1 / odds[which(odds > 1)]
    imp_prob
  }
  return(imp_prob)
}
