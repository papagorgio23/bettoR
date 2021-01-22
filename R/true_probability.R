#' @title True Expected Probability
#'
#' @description This function calculates the fair odds for a bet.
#'
#' @param line Line for the specific bet (-115)
#' @param odds Vector with all lines for a wager (-115, -105)
#' @param type Type of odds. Possible values are:
#' * `us`, American Odds
#' * `dec`, Decimal Odds
#' * `frac`, Fractional Odds
#'
#' @return True Expected probability of bet
#'
#' @examples true_probability(1.8, c(1.8, 2.1), type = "dec")
#' @examples true_probability(-115, c(-115, -105))
#' @examples true_probability(258, c(285, -122, 258))
#' @examples true_probability(line = -100, odds = c(-100, -120))
#'
#' @export
true_probability <-
  function(line = -110,
           odds = c(-110,-110),
           type = "us") {
    ## Error handling
    if (!is.numeric(c(line, odds))) {
      stop("Odds must be numeric")
    }
    if (!type %in% c("us", "frac", "dec")) {
      stop("Type must be either: 'us' or 'dec' or 'frac'")
    }

    # implied probability divided by the sum of all implied probabilities for the wager
    true_prob <-
      round(implied_prob(odds = line, type = type) / sum(implied_prob(odds = odds, type = type)), 4)

    return(true_prob)
  }
