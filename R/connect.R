#' Connect to the AquaCache database for calibrations/instrument management
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the calibrations database.
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form calHost="hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter=value pair of form calPort="1234".
#' @param username Username. By default searches the .Renviron file for parameter=value pair of form calUser="username". Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password. By default searches the .Renviron file for parameter=value pair of form calPass="password".
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

AquaConnect <- function(name = "AquaCache", host = Sys.getenv("AquaCacheHost"), port = Sys.getenv("AquaCachePort"), username = Sys.getenv("AquaCacheAdminUser"), password = Sys.getenv("AquaCacheAdminPass"), silent = FALSE){

  tryCatch({
    cal <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(cal)
  }, error = function(e){
    stop("Connection failed.")
  })
}

#' Connect to the calibrations database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the calibrations database.
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form calHost="hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter=value pair of form calPort="1234".
#' @param username Username. By default searches the .Renviron file for parameter=value pair of form calUser="username". Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password. By default searches the .Renviron file for parameter=value pair of form calPass="password".
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

calConnect <- function(name = "calibrations", host = Sys.getenv("calHost"), port = Sys.getenv("calPort"), username = Sys.getenv("calUser"), password = Sys.getenv("calPass"), silent = FALSE){

  tryCatch({
    cal <- DBI::dbConnect(drv = RPostgres::Postgres(),
                          dbname = name,
                          host = host,
                          port = port,
                          user = username,
                          password = password)
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(cal)
  }, error = function(e){
    stop("Connection failed.")
  })
}
