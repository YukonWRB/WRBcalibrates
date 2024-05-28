library(shiny)
library(shinyjs)

# Establish database connection
if (!exists("pool")) {
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "calibrations",
    host = Sys.getenv("calHost"),
    port = Sys.getenv("calPort"),
    user = Sys.getenv("calUser"),
    password = Sys.getenv("calPass")
  )
}
