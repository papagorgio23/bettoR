#' @title Scrape SportsBook Review's Historical Betting Lines
#'
#' @description This function scrapes the sportsbookreview.com website to return betting lines
#'
#' @param sport Sport to scrape. Possible values are:
#' * `NFL`, National Football League
#' * `NBA`, National Basketball Association
#' * `NHL`, National Hockey League
#' * `MLB`, Major League Baseball
#' * `NCAAF`, College Football
#' * `NCAAB`, College Basketball
#'
#' @param bet_type Type of bets. Possible values are:
#' * `spread`,
#' * `total`,
#' * `moneyline`,
#'
#' @param period Length of bet. Possible values are:
#' * `full`, Full game
#' * `1H`, 1st Half
#' * `2H`, 2nd Half
#' * `1Q`, 1st Quarter
#' * `2Q`, 2nd Quarter
#' * `3Q`, 3rd Quarter
#' * `4Q`, 4th Quarter
#'
#'
#' @param start_date Start Date of sporting events
#' @param end_date End Date of sporting events
#'
#' @return dataframe conatining betting lines for the given day
#'
#' @examples get_lines_range(sport = "NFL", bet_type = "spread", period = "full", start_date = 20191222, end_date = 20191231)
#'
#' @export
#'
get_lines_range <- function(sport = "NFL",
                            bet_type = "spread",
                            period = "full",
                            start_date = 20191222,
                            end_date = 20191223){

  df_names <- c("Date",
                "Sport",
                "bet_type",
                "period",
                "away_Team",
                "home_Team",
                "away_1Q",
                "away_2Q",
                "away_3Q",
                "away_4Q",
                "home_1Q",
                "home_2Q",
                "home_3Q",
                "home_4Q",
                "away_score",
                "home_score",
                "away_open",
                "home_open",
                "pinnacle1",
                "pinnacle2",
                "fiveDimes1",
                "fiveDimes2",
                "bookmaker1",
                "bookmaker2",
                "BOL1",
                "BOL2",
                "Bovada1",
                "Bovada2",
                "Heritage1",
                "Heritage2",
                "Intertops1",
                "Intertops2",
                "youwager1",
                "youwager2",
                "justbet1",
                "justbet2",
                "sportsbet1",
                "sportsbet2",
                "oddsURL")

  # initialize results
  all_lines <- data.frame()

  # loop through the days
  for (day in start_date:end_date) {
    tryCatch(temp <- get_lines(sport = sport, bet_type = bet_type, period = period, start_date = day),
             error = function(e){
               print(glue::glue("No games played on {day}"))
               temp <- data.frame(matrix(ncol = 39, nrow = 0))
               temp <- colnames(temp) <- df_names
               })
    all_lines <- rbind(all_lines, temp)
  }

  all_lines <- dplyr::distinct(all_lines)

  # Done and done
  message(glue::glue("\n\nCompleted Scrape: \n\nSport - {sport}\nBet Type - {bet_type} \nPeriod - {period} \nDates - {start_date} - {end_date}\nTotal Games - {nrow(all_lines)} \n\n"))
  return(all_lines)
}
