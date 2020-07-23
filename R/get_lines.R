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
#' @return dataframe conatining betting lines for the given day
#'
#' @examples get_lines(sport = "NFL", bet_type = "spread", period = "full", start_date = "20191222")
#' @examples get_lines(sport = "NBA", bet_type = "total", period = "2Q", start_date = "20191221")
#' @examples get_lines(sport = "NCAAF", bet_type = "moneyline", period = "1H", start_date = "20191019")
#'
#' @export
#'
get_lines <- function(sport = "NFL",
                      bet_type = "spread",
                      period = "full",
                      start_date = "20191222"){
  # initialize results
  final_lines <- data.frame()

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
  oddsURL <- glue::glue("https://classic.sportsbookreview.com/betting-odds/{SPORT}/{TYPE}/{PERIOD}/?date={DATE}")

  if (PERIOD == "full") {
    oddsURL <- glue::glue("https://classic.sportsbookreview.com/betting-odds/{SPORT}/{TYPE}/?date={DATE}")
  }

  ## Spread bets don't need a Type in the url
  if (TYPE == "spread") {
    oddsURL <- glue::glue("https://classic.sportsbookreview.com/betting-odds/{SPORT}/{PERIOD}/?date={DATE}")
    ## Full game & Spread bets don't need Period or Type in the url
    if (PERIOD == "full") {
      oddsURL <- glue::glue("https://classic.sportsbookreview.com/betting-odds/{SPORT}/?date={DATE}")
    }
  }

  ## need to loop eventually...
  oddspage <- xml2::read_html(oddsURL)
  node <- rvest::html_nodes(oddspage, "div.event-holder.holder-complete")
  games <- length(node)

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

    ## Final Score
    kids <- rvest::html_children(child1)[2]
    kids <- rvest::html_children(kids)[3]
    kids <- rvest::html_children(kids)[4]

    # away score
    away_score <- rvest::html_node(rvest::html_children(kids)[1],"span.first.total") %>%
      rvest::html_text() %>%
      as.integer()

    # home score
    home_score <- rvest::html_node(rvest::html_children(kids)[2],"span.total") %>%
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
    game_lines <- as.data.frame(t(c(DATE,
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
    )))

    ## save game results
    final_lines <- rbind(final_lines, game_lines)
  }

  # name columns
  colnames(final_lines) <- c("Date",
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


  # Done and done
  message(glue::glue("Scraped Day: {DATE}"))
  return(final_lines)
}
