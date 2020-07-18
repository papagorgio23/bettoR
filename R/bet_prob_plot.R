#' @title Bet Probability Plot
#'
#' @description This function plots the win, loss, and push probabilities of a given bet based on the current line and your predicted line.
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
#' @examples bet_prob_plot(-9, -3.5, sport = "NFL")
#' @examples bet_prob_plot(-7, -3, sport = "NBA")
#' @examples bet_prob_plot(21, 10.5, sport = "NCAAF")
#' @examples bet_prob_plot(-3, 5, sport = "NCAAB")
#'
#' @references Stern, Hal. "The Probability of Winning a Football Game as a function of the Pointspread." The American Statistician 45, no. 3 (1991): 179-83. Accessed July 18, 2020. doi:10.2307/2684286. \url{https://statistics.stanford.edu/sites/g/files/sbiybj6031/f/COV%20NSF%2059.pdf}
#' @references Stern, Hal. "On the Probability of Winning a Football Game." The American Statistician 45, no. 3 (1991): 179-83. Accessed July 18, 2020. doi:10.2307/2684286. \url{https://www-jstor-org.turing.library.northwestern.edu/stable/2684286}
#' @references Winston, Wayne L. "From Point Ratings to Probabilities." In Mathletics: How Gamblers, Managers, and Sports Enthusiasts Use Mathematics in Baseball, Basketball, and Football, 290-97. PRINCETON; OXFORD: Princeton University Press, 2009. Accessed July 18, 2020. doi:10.2307/j.ctt7sj9q.48.
#'
#'
#' @export
bet_prob_plot <- function(pred_spread, spread, sport = "NBA"){
  ## Error handling
  if (!is.numeric(pred_spread)) {
    stop("Spread must be numeric")
  }
  if (!is.numeric(spread)) {
    stop("Spread must be numeric")
  }
  if (spread %% 1 != 0 & spread %% 1 != 0.5) {
    stop("Invalid Spread")
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
    win_prob <- round(1 - (stats::pnorm(pred_spread + 0.5, mean = spread, sd = sd)), 4) # correct for home winning probability
    lose_prob <- round((stats::pnorm(pred_spread - 0.5, mean = spread, sd = sd)), 4) # correct for away team winning probability
    push_prob <- round(1 - win_prob - lose_prob, 4)
  } else {
    win_prob <- round(1 - (stats::pnorm(pred_spread, mean = spread, sd = sd)), 4) # correct for home winning probability
    lose_prob <- round((stats::pnorm(pred_spread, mean = spread, sd = sd)), 4) # correct for away team winning probability
    push_prob <- 0
  }

  probs <- data.frame(`Win Probability` = win_prob,
                      `Lose Probability` = lose_prob,
                      `Push Probability` = push_prob)

  edge <- spread - pred_spread
  if (spread < 0) {
    edge <- pred_spread - spread
  }

  p <- ggplot2::ggplot(data = data.frame(x = c(pred_spread - (sd * 3), pred_spread + (sd * 3))), ggplot2::aes(x)) +
    ggplot2::stat_function(fun = stats::dnorm, n = 501, args = list(mean = pred_spread, sd = sd), geom = "line") +
    ggplot2::stat_function(fun = stats::dnorm,
                  xlim = c(pred_spread - (sd * 3), spread),
                  n = 501, args = list(mean = pred_spread, sd = sd),
                  geom = "area",
                  fill = "green",
                  alpha = 0.75) +
    ggplot2::stat_function(fun = stats::dnorm,
                  xlim = c(spread, pred_spread + (sd * 3)),
                  n = 501,
                  args = list(mean = pred_spread, sd = sd),
                  geom = "area",
                  fill = "firebrick1",
                  alpha = 0.75) +
    ggplot2::geom_vline(xintercept = spread, color = "black", linetype = "dashed") +
    ggplot2::geom_vline(xintercept = pred_spread, color = "black", linetype = "dashed") +
    ggplot2::annotate("label",
             x = -Inf,
             y = Inf,
             hjust = 0,
             # x = Inf,
             # y = Inf,
             # hjust = 1,
             vjust = 1,
             size = 4,
             fontface = "bold",
             label = paste0("Point Edge: ",
                            spread - pred_spread,
                            "\nWin Probability: ",
                            win_prob * 100, "%",
                            "\nLose Probability: ",
                            lose_prob * 100, "%",
                            "\nPush Probability: ",
                            push_prob * 100, "%")) +
    ggplot2::labs(#title = "Bet Probabilities",
         title = glue::glue("Predicted Line: {pred_spread}  -  Actual Line: {spread}"),
         y = "",
         x = "Final Margin",
         caption = paste0("Based on a standard deviation of ", sd)) +
    ggplot2::scale_y_continuous(breaks = NULL, expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                      face = "bold"))

  return(p)
}

