#' Sportsbook Hold Percentage
#'
#' This function calculates the hold perrcentage that the sportsbook has for the given bet.
#'
#' @param ... Vector of lines for a given bet (-115, -105)
#'
#' @return Hold percent
#' @export
#'
#' @examples hold_calc(-110, -110)
#' @examples hold_calc(-125, -125)
#' @examples hold_calc(285, -122, 258)
hold_calc <- function(...){
  lines <- c(...)
  ## Error handling
  if (!is.numeric(lines)) {
    stop("Lines must be numeric")
  }
  imp_probs <- sapply(lines, implied_prob)
  hold <- sum(imp_probs) - 1
  return(hold)
}
