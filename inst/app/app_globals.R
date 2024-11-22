calibrate_globals <- function(dbName, dbHost, dbPort, dbUser, dbPass, RLS_user, RLS_pass) {

  library(shiny)
  library(shinyjs)

  # Establish database connection
  if (!exists("pool")) {
    # pool <- pool::dbPool(
    #   drv = RPostgres::Postgres(),
    #   dbname = "calibrations",
    #   host = Sys.getenv("calHost"),
    #   port = Sys.getenv("calPort"),
    #   user = Sys.getenv("calUser"),
    #   password = Sys.getenv("calPass")
    # )

    pool <<- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = dbName,
      host = dbHost,
      port = dbPort,
      user = dbUser,
      password = dbPass
    )
  }

}
