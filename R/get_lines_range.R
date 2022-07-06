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
#' @return dataframe containing betting lines for the given day
#'
#' @examples
#' \dontrun{ get_lines_range(
#'   sport = "NFL",
#'   bet_type = "spread",
#'   period = "full",
#'   start_date = 20191222,
#'   end_date = 20191231
#' )
#' get_lines_range(
#'   sport = "NBA",
#'   bet_type = "moneyline",
#'   period = "1H",
#'   start_date = 20191230,
#'   end_date = 20200105
#' )
#' }
#'
get_lines_range <- function(sport = "NFL",
                            bet_type = "spread",
                            period = "full",
                            start_date = 20191222,
                            end_date = 20191223) {
  ## Error handling
  if (is.na(as.Date(as.character(start_date), "%Y%m%d"))) {
    stop("Start Date format is wrong")
  }
  if (is.na(as.Date(as.character(end_date), "%Y%m%d"))) {
    stop("End Date format is wrong")
  }


  range <- seq(as.Date(as.character(start_date), "%Y%m%d"),
               as.Date(as.character(end_date), "%Y%m%d"),
               "day")


  if (bet_type == "moneyline") {

    # column names
    df_names <-
      c(
        "game_id",
        "Date",
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
        "away_pinnacle",
        "home_pinnacle",
        "away_fiveDimes",
        "home_fiveDimes",
        "away_bookmaker",
        "home_bookmaker",
        "away_BOL",
        "home_BOL",
        "away_Bovada",
        "home_Bovada",
        "away_Heritage",
        "home_Heritage",
        "away_Intertops",
        "home_Intertops",
        "away_youwager",
        "home_youwager",
        "away_justbet",
        "home_justbet",
        "away_sportsbet",
        "home_sportsbet",
        "oddsURL"
      )
  }

  if (bet_type == "total") {

    # column names
    df_names <-
      c(
        "game_id",
        "Date",
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
        "over_open_line",
        "over_open_odds",
        "under_open_line",
        "under_open_odds",
        "over_pinnacle_line",
        "over_pinnacle_odds",
        "under_pinnacle_line",
        "under_pinnacle_odds",
        "over_fiveDimes_line",
        "over_fiveDimes_odds",
        "under_fiveDimes_line",
        "under_fiveDimes_odds",
        "over_bookmaker_line",
        "over_bookmaker_odds",
        "under_bookmaker_line",
        "under_bookmaker_odds",
        "over_BOL_line",
        "over_BOL_odds",
        "under_BOL_line",
        "under_BOL_odds",
        "over_Bovada_line",
        "over_Bovada_odds",
        "under_Bovada_line",
        "under_Bovada_odds",
        "over_Heritage_line",
        "over_Heritage_odds",
        "under_Heritage_line",
        "under_Heritage_odds",
        "over_Intertops_line",
        "over_Intertops_odds",
        "under_Intertops_line",
        "under_Intertops_odds",
        "over_youwager_line",
        "over_youwager_odds",
        "under_youwager_line",
        "under_youwager_odds",
        "over_justbet_line",
        "over_justbet_odds",
        "under_justbet_line",
        "under_justbet_odds",
        "over_sportsbet_line",
        "over_sportsbet_odds",
        "under_sportsbet_line",
        "under_sportsbet_odds",
        "oddsURL"
      )

  }


  if (bet_type == "spread") {

    # column names
    df_names <-
      c(
        "game_id",
        "Date",
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
        "away_open_line",
        "away_open_odds",
        "home_open_line",
        "home_open_odds",
        "away_pinnacle_line",
        "away_pinnacle_odds",
        "home_pinnacle_line",
        "home_pinnacle_odds",
        "away_fiveDimes_line",
        "away_fiveDimes_odds",
        "home_fiveDimes_line",
        "home_fiveDimes_odds",
        "away_bookmaker_line",
        "away_bookmaker_odds",
        "home_bookmaker_line",
        "home_bookmaker_odds",
        "away_BOL_line",
        "away_BOL_odds",
        "home_BOL_line",
        "home_BOL_odds",
        "away_Bovada_line",
        "away_Bovada_odds",
        "home_Bovada_line",
        "home_Bovada_odds",
        "away_Heritage_line",
        "away_Heritage_odds",
        "home_Heritage_line",
        "home_Heritage_odds",
        "away_Intertops_line",
        "away_Intertops_odds",
        "home_Intertops_line",
        "home_Intertops_odds",
        "away_youwager_line",
        "away_youwager_odds",
        "home_youwager_line",
        "home_youwager_odds",
        "away_justbet_line",
        "away_justbet_odds",
        "home_justbet_line",
        "home_justbet_odds",
        "away_sportsbet_line",
        "away_sportsbet_odds",
        "home_sportsbet_line",
        "home_sportsbet_odds",
        "oddsURL"
      )

  }



  # initialize results
  all_lines <- data.frame()

  # loop through the days
  for (day in as.list(range)) {
    current_day <- gsub("-", "", x = day)
    tryCatch(
      temp <-
        get_lines(
          sport = sport,
          bet_type = bet_type,
          period = period,
          start_date = current_day
        ),
      error = function(e) {
        print(glue::glue("No games played on {day}"))
        temp <- data.frame(matrix(ncol = 61, nrow = 0))
        temp <- colnames(temp) <- df_names
      }
    )
    all_lines <- rbind(all_lines, temp)

    # being nice to the system
    Sys.sleep(1)
  }

  all_lines <- dplyr::distinct(all_lines)

  # Done and done
  message(
    glue::glue(
      "\n\nCompleted Scrape: \n\nSport - {sport}\nBet Type - {bet_type} \nPeriod - {period} \nDates - {start_date} - {end_date}\nTotal Games - {nrow(all_lines)} \n\n"
    )
  )
  return(all_lines)
}
