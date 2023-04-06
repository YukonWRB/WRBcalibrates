#' Connect to calibrations database
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function exists to facilitate connecting to the calibrations database. Cannot be used to create a new database, use [DBI::dbConnect()] with the appropriate driver (e.g. [RSQLite::SQLite()]) for that purpose.
#'
#' @param path The path to the database. 'default' points to "//env-fs/env-data/corp/water/Common_GW_SW/Data/calibrations/WRBcalibrates.sqlite".
#' @param timeout The duration in which to retry an operation in milliseconds. Valid for the duration of the connection.
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#' @export

calConnect <- function(path = "default", timeout = 100000, silent = FALSE){

  if (path == "default"){
    path <- "//env-fs/env-data/corp/water/Common_GW_SW/Data/calibrations/WRBcalibrates.sqlite"
  }

  if(!file.exists(path)){
    stop("The path you specified either does not exist or this computer does not have access to that drive.")
  }

  tryCatch({
    cal <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbExecute(cal, paste0("PRAGMA busy_timeout=", timeout))
    if (!silent){
      print("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(cal)
  }, error = function(e){
    stop("Connection failed.")
  })
}
