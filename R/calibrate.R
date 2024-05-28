#' In-house application for instrument calibrations and maintenance
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param host Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in `port` to be open on the host machine.
#' @param port Port number (numeric). Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network.
#' @param browser Open the application in a browser window right away (TRUE), or in a R window (FALSE). Default is TRUE.
#' @param display.mode The display mode for the application. Default is "normal". See `shiny::runApp()` for more information.
#'
#' @return Opens a Shiny application.
#' @export
#'

calibrate <- function(host = getOption("shiny.host", "127.0.0.1"), port = getOption("shiny.port"), browser = TRUE, display.mode = "normal") {


  appDir <- system.file("app", package = "WRBcalibrates")

  # Load the global variables. Contains modules as well as call to pool::pool() for connection to WRB database, library calls, and loads the translations data.table.
  source(system.file("app/app_globals.R", package = "WRBcalibrates"))

  if (appDir == "") {
    stop("Calibration app not found.")
  }

  shiny::runApp(appDir, display.mode = display.mode, host = host, port = port, launch.browser = browser, quiet = FALSE)

}

