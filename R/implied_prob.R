#' @title Implied Probability of Winning (American Odds)
#'
#' @description This function provides the implied probability or "Break Even" win percentage for a given bet.
#'
#' @param odds American Odds of the bet
#' @param type Type of odds. Possible values are:
#' * `us`, American Odds
#' * `dec`, Decimal Odds
#' * `frac`, Fractional Odds
#'
#' @return Implied probability of winning or the "Break Even" percentage
#'
#' @examples implied_prob(-150)
#' @examples implied_prob(360)
#' @examples implied_prob(c(360, -500, -110, 140))
#' @examples implied_prob(c(3.60, 5.00, 1.10, 1.40), type = "dec")
#' @examples implied_prob(c(5/2, 1/2, 7/1, 20/1, 10/11), type = "frac")
#'
#' @export
implied_prob <- function(odds, type = "us") {
  ## Error Handling
  if (!is.numeric(odds)) {
    stop("Odds must be numeric")
  }
  if (!type %in% c("us", "frac", "dec")) {
    stop("type must be either: ('us', 'dec', or 'frac')")
  }
  if (type == "us") {
    imp_prob <- odds
    imp_prob[] <- NA_real_
    imp_prob[which(odds <= 0)] <-
      1 / (1 - 100 / odds[which(odds <= -100)])
    imp_prob[which(odds > 0)] <-
      1 / (1 + odds[which(odds >= 100)] / 100)
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
