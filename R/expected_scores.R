#' @title Expected Scores
#'
#' @description This function calculates the expected scores for a given game.
#'
#' @param home_line Line for the home team (-6)
#' @param total Total for the game (212)
#'
#' @return dataframe with expected home score and away score for the game
#'
#' @examples expected_scores(home_line = -6, total = 212)
#' @examples expected_scores(home_line = -2.5, total = 51)
#' @examples expected_scores(home_line = 10.5, total = 231)
#'
#' @export
expected_scores <- function(home_line, total) {
  ## Error handling
  if (!is.numeric(c(home_line, total))) {
    stop("Inputs must be numeric")
  }

  # math
  scores <- data.frame(
    home_line = home_line,
    total = total,
    home_score = (total - home_line) / 2,
    away_score = (total + home_line) / 2
  )

  return(scores)
}
