#' @title Sportsbook Hold Percentage
#'
#' @description This function calculates the hold perrcentage that the sportsbook has for the given bet.
#'
#' @param ... Lines for a given bet (-115, -105)
#'
#' @return Hold percent
#'
#' @examples hold_calc(-110, -110)
#' @examples hold_calc(-125, -125)
#' @examples hold_calc(285, -122, 258)
#'
#' @export
hold_calc <- function(...) {
  lines <- c(...)
  ## Error handling
  if (!is.numeric(lines)) {
    stop("Lines must be numeric")
  }
  imp_probs <- sapply(lines, implied_prob)
  hold <- 1 - (1 / sum(imp_probs))
  return(hold)
}
