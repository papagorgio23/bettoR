#' Bankroll Simulation
#'
#' This function simulates and visualizes a bettors bankroll over a number of bets using their edge.
#'
#' @param bets The number of bets (256)
#' @param win_rate The average expected win rate of the bets (0-1)
#' @param bet_size The dollar amount of each bet. (100)
#' @param sim_length The number of simulations. (1,000)
#' @param avg_odds The average odds of the bets (-110)
#' @param odds_type Type of odds for the output ("us", "dec", "frac")
#' @param current_bet Optional input - Your current total number of tracked bets. (125)
#' @param current_win Optional input - Your current total amount won/loss from your tracked bets. (950)
#'
#' @return Plot showing the bankroll results of the simulated bets. The function also outputs the percentage of positive and negative final bankroll over the course of the simulation.
#'
#' @examples bankroll_plot(bets = 256, win_rate = 0.55, bet_size = 100, sim_length = 1000, avg_odds = -110, odds_type = "us")
#' @examples bankroll_plot(sim_length = 500, avg_odds = -110, win_rate = 0.5455)
#' @examples bankroll_plot(sim_length = 250, avg_odds = -115, win_rate = 0.5255, current_bet = 100, current_win = -500)
#' @examples bankroll_plot(sim_length = 300, avg_odds = -109, win_rate = 0.57, current_bet = 175, current_win = 5000)
#' @examples bankroll_plot()
#'
#' @export
bankroll_plot <- function(bets = 256, win_rate = 0.55, bet_size = 100, sim_length = 1000, avg_odds = -110, odds_type = "us", current_bet = NULL, current_win = NULL){
  ## Error handling
  if (!is.numeric(c(bets, win_rate, bet_size, sim_length, avg_odds))) {
    stop("Inputs must be numeric")
  }
  if (win_rate < 0 | win_rate > 1) {
    stop("Win rate must be between 0-1")
  }
  if (!odds_type %in% c("us", "frac", "dec")){
    stop("Odds Type must be either: ('us', 'dec', or 'frac')")
  }

  ## implied probability
  break_even <- implied_prob(avg_odds, odds_type)
  edge <- round(win_rate - break_even, 4)
  edge <- round(edge_calc(win_prob = win_rate, odds = avg_odds, type = odds_type), 4)

  # initialize result dataframe
  #results_bank_sim <- data.frame()
  results_bank_sim <- data.frame(bet = NA, Bankroll = NA, color = NA, Sim = NA)

  ## need to loop this many times
  for (i in 1:sim_length) {

    # simulate season
    simulation <- data.frame(bet = 1:bets,
                             Bankroll = cumsum(random_bet(risk = avg_odds * -1,
                                                          payout = bet_size,
                                                          num_bets = bets,
                                                          win_rate = win_rate)),
                             color = "green",
                             Sim = i)
    simulation$color <- ifelse(simulation[which(simulation$bet == bets), 2] < 0, "red", "green")

    # append each season sim into dataframe
    results_bank_sim <- rbind(results_bank_sim, simulation)
  }

  # Plot
  p <- ggplot2::ggplot(results_bank_sim, ggplot2::aes(bet, Bankroll, group = Sim)) +
    ggplot2::geom_line(ggplot2::aes(colour = color), alpha = 0.3) +
    ggplot2::scale_colour_manual(values = c("green", "red"), guide = FALSE) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(title = glue::glue("{sim_length} Simulations of {bets} Wagers"),
         subtitle = glue::glue("with a {win_rate * 100}% Win Rate, Risking ${avg_odds * -1} to win ${bet_size}  ({edge * 100}% Edge)"),
         caption = "A.I. Sports",
         x = "Bets",
         y = "Profit") +
    ggplot2::theme_bw()

  if (!is.null(current_bet) & !is.null(current_bet)) {
    p <- p +
      ggplot2::geom_hline(yintercept = current_win, linetype = "dashed") +
      ggplot2::annotate("label", x = current_bet, y = current_win, label = "Current")
  }

  # Get Positive and Negative simulation bankroll results
  results_bank_sim$win <- ifelse(results_bank_sim$color == "green", "Positive", "Negative")
  results_bank_sim$win <- factor(results_bank_sim$win, levels = c("Positive", "Negative"))

  # Return the ratio of positive and negative results
  print(table(results_bank_sim$win[results_bank_sim$bet == bets])/sim_length)
  return(p)
}
