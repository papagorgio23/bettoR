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
#' @param start_date Date of sporting events
#'
#' @return dataframe containing betting lines for the given day
#'
#' @examples get_lines(
#'   sport = "NFL",
#'   bet_type = "spread",
#'   period = "full",
#'   start_date = "20191222"
#' )
#' @examples get_lines(
#'   sport = "NBA",
#'   bet_type = "total",
#'   period = "2Q",
#'   start_date = "20191221"
#' )
#' @examples get_lines(
#'   sport = "NCAAF",
#'   bet_type = "moneyline",
#'   period = "1H",
#'   start_date = "20191019"
#' )
#'
#' @export
#'
get_lines <- function(sport = "NFL",
                      bet_type = "spread",
                      period = "full",
                      start_date = "20191222") {
  ## Error handling
  if (is.na(as.Date(as.character(start_date), "%Y%m%d"))) {
    stop("Start Date format is wrong")
  }


  # Sport for URL
  SPORT <- dplyr::case_when(
    sport == "NFL" ~ "nfl-football",
    sport == "NBA" ~ "nba-basketball",
    sport == "MLB" ~ "mlb-baseball",
    sport == "NHL" ~ "nhl-hockey",
    sport == "NCAAF" ~ "college-football",
    sport == "NCAAB" ~ "ncaa-basketball",
    sport == "Tennis" ~ "tennis",
    sport == "Soccer" ~ "soccer",
    sport == "UFC" ~ "ufc",
    TRUE ~ NA_character_
  )
  if (is.na(SPORT)) {
    stop("Sport must be in c('NFL', 'NBA', 'MLB', 'NHL', 'MCAAF', 'NCAAB')")
  }

  # Type for URL
  TYPE <- dplyr::case_when(
    bet_type == "spread" ~ "spread",
    bet_type == "total" ~ "totals",
    bet_type == "moneyline" ~ "money-line",
    TRUE ~ NA_character_
  )
  if (is.na(TYPE)) {
    stop("Bet Type must be in c('spread', 'total', 'moneyline')")
  }

  # Period for URL
  PERIOD <- dplyr::case_when(
    period == "full" ~ "full",
    period == "1H" ~ "1st-half",
    period == "2H" ~ "2nd-half",
    period == "1Q" ~ "1st-quarter",
    period == "2Q" ~ "2nd-quarter",
    period == "3Q" ~ "3rd-quarter",
    period == "4Q" ~ "4th-quarter",
    TRUE ~ NA_character_
  )
  if (is.na(PERIOD)) {
    stop("Period must be in c('full', '1H', '2H', '1Q', '2Q', '3Q', '4Q')")
  }

  DATE <- start_date


  ## need to loop eventually...
  # url to scrape
  oddsURL <-
    glue::glue(
      "https://classic.sportsbookreview.com/betting-odds/{SPORT}/{TYPE}/{PERIOD}/?date={DATE}"
    )

  if (PERIOD == "full") {
    oddsURL <-
      glue::glue(
        "https://classic.sportsbookreview.com/betting-odds/{SPORT}/{TYPE}/?date={DATE}"
      )
  }

  ## Spread bets don't need a Type in the url
  if (TYPE == "spread") {
    oddsURL <-
      glue::glue(
        "https://classic.sportsbookreview.com/betting-odds/{SPORT}/{PERIOD}/?date={DATE}"
      )
    ## Full game & Spread bets don't need Period or Type in the url
    if (PERIOD == "full") {
      oddsURL <-
        glue::glue("https://classic.sportsbookreview.com/betting-odds/{SPORT}/?date={DATE}")
    }
  }


  # connect to site
  res <- httr::GET(oddsURL)
  con <- httr::content(res, 'text')
  oddspage <- xml2::read_html(con)
  node <- rvest::html_nodes(oddspage, "div.event-holder.holder-complete")
  games <- length(node)

  # initialize results
  final_lines <- data.frame()

  for (game in 1:games) {
    # main child
    child1 <- rvest::html_children(node[game])

    ## Teams
    teams <- rvest::html_children(child1)[6] %>%
      rvest::html_children() %>%
      rvest::html_text()
    awayTeam <- teams[[1]]
    homeTeam <- teams[[2]]

    # get Scoring
    period_scores <- node[game] %>%
      rvest::html_nodes(".period") %>%
      rvest::html_text()

    # College BBall only has 2 halfs
    if (sport == "NCAAB") {
      away_1 <- as.integer(period_scores[1])
      away_2 <- as.integer(period_scores[2])
      away_3 <- "NA"
      away_4 <- "NA"
      home_1 <- as.integer(period_scores[3])
      home_2 <- as.integer(period_scores[4])
      home_3 <- "NA"
      home_4 <- "NA"
    } else {

    # away period scoring
    away_1 <- as.integer(period_scores[1])
    away_2 <- as.integer(period_scores[2])
    away_3 <- as.integer(period_scores[3])
    away_4 <- as.integer(period_scores[4])

    # home period scoring
    home_1 <- as.integer(period_scores[5])
    home_2 <- as.integer(period_scores[6])
    home_3 <- as.integer(period_scores[7])
    home_4 <- as.integer(period_scores[8])

    }

    ## Final Score
    kids <- rvest::html_children(child1)[2]
    kids <- rvest::html_children(kids)[3]
    kids <- rvest::html_children(kids)[4]

    # away score
    away_score <-
      rvest::html_node(rvest::html_children(kids)[1], "span.first.total") %>%
      rvest::html_text() %>%
      as.integer()

    # home score
    home_score <-
      rvest::html_node(rvest::html_children(kids)[2], "span.total") %>%
      rvest::html_text() %>%
      as.integer()

    ## Opening Line
    open <- rvest::html_children(child1)[8]
    away_open <- rvest::html_children(open)[1] %>%
      rvest::html_text()
    home_open <- rvest::html_children(open)[2] %>%
      rvest::html_text()

    ### SportsBooks
    # Pinnacle == 10
    pinnacle <- rvest::html_children(child1)[10] %>%
      rvest::html_children() %>%
      rvest::html_text()
    pinnacle1 <- pinnacle[[1]]
    pinnacle2 <- pinnacle[[2]]

    # 5 dimes == 11
    fiveDimes <- rvest::html_children(child1)[11] %>%
      rvest::html_children() %>%
      rvest::html_text()
    fiveDimes1 <- fiveDimes[[1]]
    fiveDimes2 <- fiveDimes[[2]]

    # Bookmaker == 12
    bookmaker <- rvest::html_children(child1)[12] %>%
      rvest::html_children() %>%
      rvest::html_text()
    bookmaker1 <- bookmaker[[1]]
    bookmaker2 <- bookmaker[[2]]

    # BetOnline == 13
    BOL <- rvest::html_children(child1)[13] %>%
      rvest::html_children() %>%
      rvest::html_text()
    BOL1 <- BOL[[1]]
    BOL2 <- BOL[[2]]

    # Bovada == 14
    Bovada <- rvest::html_children(child1)[14] %>%
      rvest::html_children() %>%
      rvest::html_text()
    Bovada1 <- Bovada[[1]]
    Bovada2 <- Bovada[[2]]

    # Heritage == 15
    Heritage <- rvest::html_children(child1)[15] %>%
      rvest::html_children() %>%
      rvest::html_text()
    Heritage1 <- Heritage[[1]]
    Heritage2 <- Heritage[[2]]

    # Intertops == 16
    Intertops <- rvest::html_children(child1)[16] %>%
      rvest::html_children() %>%
      rvest::html_text()
    Intertops1 <- Intertops[[1]]
    Intertops2 <- Intertops[[2]]

    # YouWager == 17
    youwager <- rvest::html_children(child1)[17] %>%
      rvest::html_children() %>%
      rvest::html_text()
    youwager1 <- youwager[[1]]
    youwager2 <- youwager[[2]]

    # JustBet == 18
    justbet <- rvest::html_children(child1)[18] %>%
      rvest::html_children() %>%
      rvest::html_text()
    justbet1 <- justbet[[1]]
    justbet2 <- justbet[[2]]

    # SportsBetting == 19
    sportsbet <- rvest::html_children(child1)[19] %>%
      rvest::html_children() %>%
      rvest::html_text()
    sportsbet1 <- sportsbet[[1]]
    sportsbet2 <- sportsbet[[2]]


    ## dataframe results
    game_lines <- as.data.frame(t(
      c(
        DATE,
        sport,
        bet_type,
        period,
        awayTeam,
        homeTeam,
        away_1,
        away_2,
        away_3,
        away_4,
        home_1,
        home_2,
        home_3,
        home_4,
        away_score,
        home_score,
        away_open,
        home_open,
        pinnacle1,
        pinnacle2,
        fiveDimes1,
        fiveDimes2,
        bookmaker1,
        bookmaker2,
        BOL1,
        BOL2,
        Bovada1,
        Bovada2,
        Heritage1,
        Heritage2,
        Intertops1,
        Intertops2,
        youwager1,
        youwager2,
        justbet1,
        justbet2,
        sportsbet1,
        sportsbet2,
        oddsURL
      )
    ))

    ## save game results
    final_lines <- rbind(final_lines, game_lines)
  }

  # initial names
  colnames(final_lines) <- c(
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

  # columns to mutate
  odds_columns <-  c(
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
    "home_sportsbet"
  )


  # clean lines and odds columns
  final_lines <- final_lines %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(dplyr::all_of(odds_columns)),
      .funs = ~ stringr::str_replace_all(., "\302\275", ".5")
    ) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(dplyr::all_of(odds_columns)),
      .funs = ~ stringr::str_replace_all(., "PK", "0 ")
    ) %>%
    dplyr::mutate_at(
      .vars = dplyr::vars(dplyr::all_of(odds_columns)),
      .funs = ~ stringr::str_replace_all(., "[^0-9+-.]", " ") # remove random shit in character string
    )

  if (bet_type == "total") {

    # renmae columns
    colnames(final_lines) <- c(
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
      "over_open",
      "under_open",
      "over_pinnacle",
      "under_pinnacle",
      "over_fiveDimes",
      "under_fiveDimes",
      "over_bookmaker",
      "under_bookmaker",
      "over_BOL",
      "under_BOL",
      "over_Bovada",
      "under_Bovada",
      "over_Heritage",
      "under_Heritage",
      "over_Intertops",
      "under_Intertops",
      "over_youwager",
      "under_youwager",
      "over_justbet",
      "under_justbet",
      "over_sportsbet",
      "under_sportsbet",
      "oddsURL"
    )


    # split columns into lines and odds
    final_lines <- final_lines %>%
      tidyr::separate(
        .data$over_open,
        c("over_open_line", "over_open_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$under_open,
        c("under_open_line", "under_open_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_pinnacle,
        c("over_pinnacle_line", "over_pinnacle_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$under_pinnacle,
        c("under_pinnacle_line", "under_pinnacle_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_fiveDimes,
        c("over_fiveDimes_line", "over_fiveDimes_odds"),
        sep = "[[:space:]]") %>%
      tidyr::separate(
        .data$under_fiveDimes,
        c("under_fiveDimes_line", "under_fiveDimes_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_bookmaker,
        c("over_bookmaker_line", "over_bookmaker_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$under_bookmaker,
        c("under_bookmaker_line", "under_bookmaker_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_BOL,
        c("over_BOL_line", "over_BOL_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$under_BOL,
        c("under_BOL_line", "under_BOL_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_Bovada,
        c("over_Bovada_line", "over_Bovada_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$under_Bovada,
        c("under_Bovada_line", "under_Bovada_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_Heritage,
        c("over_Heritage_line", "over_Heritage_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$under_Heritage,
        c("under_Heritage_line", "under_Heritage_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_Intertops,
        c("over_Intertops_line", "over_Intertops_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$under_Intertops,
        c("under_Intertops_line", "under_Intertops_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_youwager,
        c("over_youwager_line", "over_youwager_odds"),
        sep = "[[:space:]]") %>%
      tidyr::separate(
        .data$under_youwager,
        c("under_youwager_line", "under_youwager_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_justbet,
        c("over_justbet_line", "over_justbet_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$under_justbet,
        c("under_justbet_line", "under_justbet_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$over_sportsbet,
        c("over_sportsbet_line", "over_sportsbet_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$under_sportsbet,
        c("under_sportsbet_line", "under_sportsbet_odds"),
        sep = "[[:space:]]"
      )
  }

  if (bet_type == "spread") {
    # split columns into lines and odds
    final_lines <- final_lines %>%
      tidyr::separate(
        .data$away_open,
        c("away_open_line", "away_open_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$home_open,
        c("home_open_line", "home_open_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_pinnacle,
        c("away_pinnacle_line", "away_pinnacle_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$home_pinnacle,
        c("home_pinnacle_line", "home_pinnacle_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_fiveDimes,
        c("away_fiveDimes_line", "away_fiveDimes_odds"),
        sep = "[[:space:]]") %>%
      tidyr::separate(
        .data$home_fiveDimes,
        c("home_fiveDimes_line", "home_fiveDimes_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_bookmaker,
        c("away_bookmaker_line", "away_bookmaker_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$home_bookmaker,
        c("home_bookmaker_line", "home_bookmaker_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_BOL,
        c("away_BOL_line", "away_BOL_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$home_BOL,
        c("home_BOL_line", "home_BOL_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_Bovada,
        c("away_Bovada_line", "away_Bovada_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$home_Bovada,
        c("home_Bovada_line", "home_Bovada_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_Heritage,
        c("away_Heritage_line", "away_Heritage_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$home_Heritage,
        c("home_Heritage_line", "home_Heritage_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_Intertops,
        c("away_Intertops_line", "away_Intertops_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$home_Intertops,
        c("home_Intertops_line", "home_Intertops_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_youwager,
        c("away_youwager_line", "away_youwager_odds"),
        sep = "[[:space:]]") %>%
      tidyr::separate(
        .data$home_youwager,
        c("home_youwager_line", "home_youwager_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_justbet,
        c("away_justbet_line", "away_justbet_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$home_justbet,
        c("home_justbet_line", "home_justbet_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$away_sportsbet,
        c("away_sportsbet_line", "away_sportsbet_odds"),
        sep = "[[:space:]]"
      ) %>%
      tidyr::separate(
        .data$home_sportsbet,
        c("home_sportsbet_line", "home_sportsbet_odds"),
        sep = "[[:space:]]"
      )
  }

  # add game ID
  final_lines <- final_lines %>%
    dplyr::mutate(game_id = glue::glue(
      "{sport}_{DATE}_{fix_nfl_names(home_Team)}_{fix_nfl_names(away_Team)}"
    )) %>%
    dplyr::select(.data$game_id, dplyr::everything())


  if (sport == "NCAAF") {
    # add game ID
    final_lines <- final_lines %>%
      dplyr::mutate(game_id = glue::glue(
        "{sport}_{DATE}_{fix_ncaaf_names(home_Team)}_{fix_ncaaf_names(away_Team)}"
      )) %>%
      dplyr::select(.data$game_id, dplyr::everything())
  }

  if (sport == "NFL") {
    # add game ID
    final_lines <- final_lines %>%
      dplyr::mutate(game_id = glue::glue(
        "{sport}_{DATE}_{fix_nfl_names(home_Team)}_{fix_nfl_names(away_Team)}"
      )) %>%
      dplyr::select(.data$game_id, dplyr::everything())
  }

  if (sport == "NBA") {
    # add game ID
    final_lines <- final_lines %>%
      dplyr::mutate(game_id = glue::glue(
        "{sport}_{DATE}_{fix_nba_names(home_Team)}_{fix_nba_names(away_Team)}"
      )) %>%
      dplyr::select(.data$game_id, dplyr::everything())
  }

  # convert columns to numeric
  final_lines[, 8:(length(final_lines) - 1)] <-
    sapply(final_lines[, 8:(length(final_lines) - 1)], as.numeric)


  # Done and done
  message(glue::glue("Scraped Day: {DATE}"))
  return(final_lines)
}
