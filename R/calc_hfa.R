#' Calculate a team's Home Field Advantage
#'
#' Calculate the percentage of time team A beats team B at home based of the
#' percentage of time team A wins on a neutral field and the league average win
#' percentage at home.
#'
#' @param win_p_neutral percentage of time team A beats team B on neutral field.
#' @param win_p_leag_hm league average win percentage for home teams.
#'
#' @return numeric scalar between 0 and 1
#'
#' @examples calc_hfa(.142, .61)
#'
#' @references
#' See post by Ganchrow at the SBR forum for details [Home field advantage](https://www.sportsbookreview.com/forum/handicapper-think-tank/122312-home-field-advantage.html)
#'
#' @export
calc_hfa <- function(win_p_neutral, win_p_leag_hm) {
  ex_input_obj <- expression(win_p_neutral > 0,
                             win_p_neutral < 1,
                             win_p_leag_hm > 0,
                             win_p_leag_hm < 1)
  stopifnot(exprObject = ex_input_obj)
  logodds_tm <- log(win_p_neutral) - log(1 - win_p_neutral)
  logodds_lg <- log(win_p_leag_hm) - log(1 - win_p_leag_hm)
  p <-
    exp((logodds_tm + logodds_lg)) / (1 + exp(logodds_tm + logodds_lg))
  ex_output_obj <- expression(p > 0,
                              p < 1)
  stopifnot(exprObject = ex_output_obj)
  p
}
