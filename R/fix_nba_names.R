#' @title Fix NBA Team Names
#'
#' @description This function changes NBA team names to their abbreviated team name.
#'
#' @param x String - NBA Team name (Baltimore Ravens)
#'
#' @return Abbreviated NBA team name.
#'
#' @examples fix_nba_names("atlanta")
#' @examples fix_nba_names("HAWKS")
#'
#'
fix_nba_names <- function(x){
  x[grep("Atlanta", x, ignore.case=TRUE)] <- "ATL"
  x[grep("Hawks", x, ignore.case=TRUE)] <- "ATL"

  x[grep("Brooklyn", x, ignore.case=TRUE)] <- "BKN"
  x[grep("Nets", x, ignore.case=TRUE)] <- "BKN"

  x[grep("Boston", x, ignore.case=TRUE)] <- "BOS"
  x[grep("Celtics", x, ignore.case=TRUE)] <- "BOS"

  x[grep("Charlotte", x, ignore.case=TRUE)] <- "CHA"
  x[grep("Hornets", x, ignore.case=TRUE)] <- "CHA"

  x[grep("Chicago", x, ignore.case=TRUE)] <- "CHI"
  x[grep("Bulls", x, ignore.case=TRUE)] <- "CHI"

  x[grep("Cleveland", x, ignore.case=TRUE)] <- "CLE"
  x[grep("Cavaliers", x, ignore.case=TRUE)] <- "CLE"

  x[grep("Dallas", x, ignore.case=TRUE)] <- "DAL"
  x[grep("Mavericks", x, ignore.case=TRUE)] <- "DAL"

  x[grep("Denver", x, ignore.case=TRUE)] <- "DEN"
  x[grep("Nuggets", x, ignore.case=TRUE)] <- "DEN"

  x[grep("Detroit", x, ignore.case=TRUE)] <- "DET"
  x[grep("Pistons", x, ignore.case=TRUE)] <- "DET"

  x[grep("Free", x, ignore.case=TRUE)] <- "FA"
  x[grep("Agent", x, ignore.case=TRUE)] <- "FA"

  x[grep("Golden State", x, ignore.case=TRUE)] <- "GSW"
  x[grep("Warriors", x, ignore.case=TRUE)] <- "GSW"

  x[grep("Houston", x, ignore.case=TRUE)] <- "HOU"
  x[grep("Rockets", x, ignore.case=TRUE)] <- "HOU"

  x[grep("Indiana", x, ignore.case=TRUE)] <- "IND"
  x[grep("Pacers", x, ignore.case=TRUE)] <- "IND"

  x[grep("Los Angeles Clippers", x, ignore.case=TRUE)] <- "LAC"
  x[grep("L.A. Clippers", x, ignore.case=TRUE)] <- "LAC"
  x[grep("Clippers", x, ignore.case=TRUE)] <- "LAC"

  x[grep("Los Angeles Lakers", x, ignore.case=TRUE)] <- "LAL"
  x[grep("L.A. Lakers", x, ignore.case=TRUE)] <- "LAL"
  x[grep("Lakers", x, ignore.case=TRUE)] <- "LAL"

  x[grep("Memphis", x, ignore.case=TRUE)] <- "MEM"
  x[grep("Grizzlies", x, ignore.case=TRUE)] <- "MEM"

  x[grep("Miami", x, ignore.case=TRUE)] <- "MIA"
  x[grep("Heat", x, ignore.case=TRUE)] <- "MIA"

  x[grep("Milwaukee", x, ignore.case=TRUE)] <- "MIL"
  x[grep("Bucks", x, ignore.case=TRUE)] <- "MIL"

  x[grep("Minnesota", x, ignore.case=TRUE)] <- "MIN"
  x[grep("Timberwolves", x, ignore.case=TRUE)] <- "MIN"

  x[grep("New Orleans", x, ignore.case=TRUE)] <- "NOP"
  x[grep("Pelicans", x, ignore.case=TRUE)] <- "NOP"

  x[grep("New York", x, ignore.case=TRUE)] <- "NYK"
  x[grep("Knicks", x, ignore.case=TRUE)] <- "NYK"

  x[grep("Oklahoma", x, ignore.case=TRUE)] <- "OKC"
  x[grep("Thunder", x, ignore.case=TRUE)] <- "OKC"

  x[grep("Orlando", x, ignore.case=TRUE)] <- "ORL"
  x[grep("Magic", x, ignore.case=TRUE)] <- "ORL"

  x[grep("Philadelphia", x, ignore.case=TRUE)] <- "PHI"
  x[grep("76ers", x, ignore.case=TRUE)] <- "PHI"

  x[grep("Phoenix", x, ignore.case=TRUE)] <- "PHX"
  x[grep("Suns", x, ignore.case=TRUE)] <- "PHX"

  x[grep("Portland", x, ignore.case=TRUE)] <- "POR"
  x[grep("Trail blazers", x, ignore.case=TRUE)] <- "POR"

  x[grep("Sacramento", x, ignore.case=TRUE)] <- "SAC"
  x[grep("Kings", x, ignore.case=TRUE)] <- "SAC"

  x[grep("San Antonio", x, ignore.case=TRUE)] <- "SAS"
  x[grep("Spurs", x, ignore.case=TRUE)] <- "SAS"

  x[grep("Toronto", x, ignore.case=TRUE)] <- "TOR"
  x[grep("Rapters", x, ignore.case=TRUE)] <- "TOR"

  x[grep("Utah", x, ignore.case=TRUE)] <- "UTA"
  x[grep("Jazz", x, ignore.case=TRUE)] <- "UTA"

  x[grep("Washington", x, ignore.case=TRUE)] <- "WAS"
  x[grep("Wizards", x, ignore.case=TRUE)] <- "WAS"

  return(x)
}
