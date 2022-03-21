#' @title Sportsbook's Over Round Percentage
#'
#' @description This function calculates the extra implied probability from each line of the bet. House Edge or Over Round.
#'
#' @param ... Lines for a given bet (-115, -105)
#'
#' @return Bet's Over Round percent
#'
#' @examples over_round(-110, -110)
#' @examples over_round(-125, -125)
#' @examples over_round(285, -122, 258)
#'
#' @export
over_round <- function(...) {
  lines <- c(...)
  ## Error handling
  if (!is.numeric(lines)) {
    stop("Lines must be numeric")
  }
  imp_probs <- sapply(lines, implied_prob)
  house_edge <- sum(imp_probs) - 1
  return(house_edge)
}
