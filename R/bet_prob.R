#' @title Bet Probability
#'
#' @description This function returns the win, loss, and push probabilities of a given bet based on the current line and your predicted line.
#'
#' @param pred_spread Predicted spread for the team you want to bet on
#' @param spread Spread for the team that you want to bet on (-3.5, -7, 4, 2.5)
#' @param sport Sport/League of the teams being bet on. Possible values are:
#' * `NBA`, National Basketball Association
#' * `NCAAB`, College Basketball
#' * `NFL`, National Football League
#' * `NCAAF`, College Football
#'
#' @return probs Plot of the simulation as well as the percentage of simulations that were positive and negative.
#'
#' @examples bet_prob(-9, -3.5, sport = "NFL")
#' @examples bet_prob(-7, -3, sport = "NBA")
#' @examples bet_prob(21, 10.5, sport = "NCAAF")
#' @examples bet_prob(-3, 5, sport = "NCAAB")
#'
#' @references Stern, Hal. "The Probability of Winning a Football Game as a function of the Pointspread." The American Statistician 45, no. 3 (1991): 179-83. Accessed July 18, 2020. doi:10.2307/2684286. \url{https://statistics.stanford.edu/sites/g/files/sbiybj6031/f/COV%20NSF%2059.pdf}
#' @references Stern, Hal. "On the Probability of Winning a Football Game." The American Statistician 45, no. 3 (1991): 179-83. Accessed July 18, 2020. doi:10.2307/2684286. \url{https://www-jstor-org.turing.library.northwestern.edu/stable/2684286}
#' @references Winston, Wayne L. "From Point Ratings to Probabilities." In Mathletics: How Gamblers, Managers, and Sports Enthusiasts Use Mathematics in Baseball, Basketball, and Football, 290-97. PRINCETON; OXFORD: Princeton University Press, 2009. Accessed July 18, 2020. doi:10.2307/j.ctt7sj9q.48.
#'
#'
#' @export
bet_prob <- function(pred_spread, spread, sport = "NBA"){
  ## Error handling
  if (!is.numeric(pred_spread)) {
    stop("Spread must be numeric")
  }
  if (!is.numeric(spread)) {
    stop("Spread must be numeric")
  }
  if (!sport %in% c("NBA", "NCAAB", "NFL", "NCAAF")){
    stop("Sport must be either: ('NBA', 'NCAAB', 'NFL', or 'NCAAF')")
  }
  if (sport == "NBA"){
    sd <- 12
  }
  if (sport == "NCAAB"){
    sd <- 10
  }
  if (sport == "NFL"){
    sd <- 13.86
  }
  if (sport == "NCAAF"){
    sd <- 16
  }

  # This checks if the spread is a whole number or not
  if (spread %% 1 == 0) {
    win_prob <- 1 - (stats::pnorm(pred_spread + 0.5, mean = spread, sd = sd)) # correct for home winning probability
    lose_prob <- (stats::pnorm(pred_spread - 0.5, mean = spread, sd = sd)) # correct for away team winning probability
    push_prob <- 1 - win_prob - lose_prob
  } else {
    win_prob <- 1 - (stats::pnorm(pred_spread, mean = spread, sd = sd)) # correct for home winning probability
    lose_prob <- (stats::pnorm(pred_spread, mean = spread, sd = sd)) # correct for away team winning probability
    push_prob <- 0
  }

  probs <- data.frame(`Win Probability` = win_prob,
                      `Lose Probability` = lose_prob,
                      `Push Probability` = push_prob)

  return(probs)
}
