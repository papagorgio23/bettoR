testthat::test_that("calf_hfa should only take values 0 < x < 1",
                    {
                      testthat::expect_error(calc_hfa(1, .5))
                      testthat::expect_error(calc_hfa(.5, 1.5))
                      testthat::expect_error(calc_hfa(-5, .25))
                      testthat::expect_error(calc_hfa(0, 1))
                    })

testthat::test_that("Team wins .5 vs opp at neutral site, in .5 hfa league, win p is .5 at home",
                    {
                      testthat::expect_equal(calc_hfa(.5, .5), .5)
                    })
