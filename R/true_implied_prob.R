#' @title True Implied Probability of Winning (American Odds)
#'
#' @description This function provides the true implied win probability for a bet.
#'
#' @param odds American Odds of the bet
#' @param type Type of odds. Possible values are:
#' * `us`, American Odds
#' * `dec`, Decimal Odds
#' * `frac`, Fractional Odds
#'
#' @return Implied probability of winning or the "Break Even" percentage
#'
#' @examples true_implied_prob(-150)
#' @examples true_implied_prob(360)
#' @examples true_implied_prob(c(360, -500, -110, 140))
#' @examples true_implied_prob(c(3.60, 5.00, 1.10, 1.40), type = "dec")
#' @examples true_implied_prob(c(5/2, 1/2, 7/1, 20/1, 10/11), type = "frac")
#'
#' @export
true_implied_prob <- function(odds, type = "us"){
  ## Error Handling
  if (!is.numeric(odds)) {
    stop("Odds must be numeric")
  }
  if (!type %in% c("us", "frac", "dec")){
    stop("type must be either: ('us', 'dec', or 'frac')")
  }

  # implied probability divided by the sum of all implied probabilities for the wager
  true_prob <-
    round(implied_prob(odds = odds, type = type) / sum(implied_prob(odds = odds, type = type)), 4)

  imp_prob <-
    round(implied_prob(odds = odds, type = type), 4)

  results <- data.frame(true_prob, imp_prob)

  return(results)
}
