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
#' @example calc_hfa(.142, .61)
#'
#' @references
#' See post by Ganchrow at the SBR forum for details [Home field advantage](https://www.sportsbookreview.com/forum/handicapper-think-tank/122312-home-field-advantage.html)
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


## tests ----
testthat::test_that(
  "calf_hfa should only take values 0 < x < 1",
  {
    testthat::expect_error(calc_hfa(1,.5))
    testthat::expect_error(calc_hfa(.5,1.5))
    testthat::expect_error(calc_hfa(-5,.25))
    testthat::expect_error(calc_hfa(0,1))
  })

testthat::test_that(
  "Team wins .5 vs opp at neutral site, in .5 hfa league, win p is .5 at home",
  {
    testthat::expect_equal(calc_hfa(.5, .5),.5)
  })
