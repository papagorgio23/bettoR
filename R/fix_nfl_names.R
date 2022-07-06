#' @title Fix NFL Team Names
#'
#' @description This function changes NFL team names to their abbreviated team name.
#'
#' @param x String - NFL Team name (Baltimore Ravens)
#'
#' @return Abbreviated NFL team name.
#'
#' @examples fix_nfl_names("arizona")
#' @examples fix_nfl_names("CARDINALS")
#'
#'
fix_nfl_names <- function(x){
  x[grep("Arizona", x, ignore.case=TRUE)] <- "ARZ"
  x[grep("Cardinals", x, ignore.case=TRUE)] <- "ARZ"
  x[grep("ARI", x, ignore.case=TRUE)] <- "ARZ"

  x[grep("Atlanta", x, ignore.case=TRUE)] <- "ATL"
  x[grep("Falcons", x, ignore.case=TRUE)] <- "ATL"

  x[grep("Baltimore", x, ignore.case=TRUE)] <- "BAL"
  x[grep("Ravens", x, ignore.case=TRUE)] <- "BAL"

  x[grep("Buffalo", x, ignore.case=TRUE)] <- "BUF"
  x[grep("Bills", x, ignore.case=TRUE)] <- "BUF"

  x[grep("Carolina", x, ignore.case=TRUE)] <- "CAR"
  x[grep("Panthers", x, ignore.case=TRUE)] <- "CAR"

  x[grep("Chicago", x, ignore.case=TRUE)] <- "CHI"
  x[grep("Bears", x, ignore.case=TRUE)] <- "CHI"

  x[grep("Cincinnati", x, ignore.case=TRUE)] <- "CIN"
  x[grep("Bengals", x, ignore.case=TRUE)] <- "CIN"

  x[grep("Cleveland", x, ignore.case=TRUE)] <- "CLE"
  x[grep("Browns", x, ignore.case=TRUE)] <- "CLE"

  x[grep("Dallas", x, ignore.case=TRUE)] <- "DAL"
  x[grep("Cowboys", x, ignore.case=TRUE)] <- "DAL"

  x[grep("Denver", x, ignore.case=TRUE)] <- "DEN"
  x[grep("Broncos", x, ignore.case=TRUE)] <- "DEN"

  x[grep("Detroit", x, ignore.case=TRUE)] <- "DET"
  x[grep("Lions", x, ignore.case=TRUE)] <- "DET"

  x[grep("Free", x, ignore.case=TRUE)] <- "FA"
  x[grep("Agent", x, ignore.case=TRUE)] <- "FA"

  x[grep("Green Bay", x, ignore.case=TRUE)] <- "GB"
  x[grep("Packers", x, ignore.case=TRUE)] <- "GB"

  x[grep("Houston", x, ignore.case=TRUE)] <- "HOU"
  x[grep("Texans", x, ignore.case=TRUE)] <- "HOU"

  x[grep("Indianapolis", x, ignore.case=TRUE)] <- "IND"
  x[grep("Colts", x, ignore.case=TRUE)] <- "IND"

  x[grep("Jacksonville", x, ignore.case=TRUE)] <- "JAX"
  x[grep("Jaguars", x, ignore.case=TRUE)] <- "JAX"

  x[grep("Kansas City", x, ignore.case=TRUE)] <- "KC"
  x[grep("Chiefs", x, ignore.case=TRUE)] <- "KC"

  x[grep("Miami", x, ignore.case=TRUE)] <- "MIA"
  x[grep("Dolphins", x, ignore.case=TRUE)] <- "MIA"

  x[grep("Minnesota", x, ignore.case=TRUE)] <- "MIN"
  x[grep("Vikings", x, ignore.case=TRUE)] <- "MIN"

  x[grep("New England", x, ignore.case=TRUE)] <- "NE"
  x[grep("Patriots", x, ignore.case=TRUE)] <- "NE"

  x[grep("New Orleans", x, ignore.case=TRUE)] <- "NO"
  x[grep("Saints", x, ignore.case=TRUE)] <- "NO"

  x[grep("Jets", x, ignore.case=TRUE)] <- "NYJ"

  x[grep("Giants", x, ignore.case=TRUE)] <- "NYG"

  x[grep("Las Vegas", x, ignore.case=TRUE)] <- "OAK"
  x[grep("LV", x, ignore.case=TRUE)] <- "OAK"
  x[grep("Oakland", x, ignore.case=TRUE)] <- "OAK"
  x[grep("Raiders", x, ignore.case=TRUE)] <- "OAK"

  x[grep("Philadelphia", x, ignore.case=TRUE)] <- "PHI"
  x[grep("Eagles", x, ignore.case=TRUE)] <- "PHI"

  x[grep("Pittsburgh", x, ignore.case=TRUE)] <- "PIT"
  x[grep("Steelers", x, ignore.case=TRUE)] <- "PIT"

  x[grep("San Diego", x, ignore.case=TRUE)] <- "LAC"
  x[grep("Chargers", x, ignore.case=TRUE)] <- "LAC"
  x[grep("L.A. Chargers", x, ignore.case=TRUE)] <- "LAC"

  x[grep("Saint Louis", x, ignore.case=TRUE)] <- "LAR"
  x[grep("St Louis", x, ignore.case=TRUE)] <- "LAR"
  x[grep("St. Louis", x, ignore.case=TRUE)] <- "LAR"
  x[grep("Rams", x, ignore.case=TRUE)] <- "LAR"
  x[grep("Los Angeles", x, ignore.case=TRUE)] <- "LAR"
  x[grep("L.A. Rams", x, ignore.case=TRUE)] <- "LAR"

  x[grep("San Francisco", x, ignore.case=TRUE)] <- "SF"
  x[grep("49ers", x, ignore.case=TRUE)] <- "SF"

  x[grep("Seattle", x, ignore.case=TRUE)] <- "SEA"
  x[grep("Seahawks", x, ignore.case=TRUE)] <- "SEA"

  x[grep("Tampa Bay", x, ignore.case=TRUE)] <- "TB"
  x[grep("Buccaneers", x, ignore.case=TRUE)] <- "TB"

  x[grep("Tennessee", x, ignore.case=TRUE)] <- "TEN"
  x[grep("Titans", x, ignore.case=TRUE)] <- "TEN"

  x[grep("Washington", x, ignore.case=TRUE)] <- "WAS"
  x[grep("Redskins", x, ignore.case=TRUE)] <- "WAS"

  return(x)
}
