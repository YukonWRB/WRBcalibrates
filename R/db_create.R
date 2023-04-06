#' Initial calibration database creation
#'
#'#' @description
#' `r lifecycle::badge("experimental")`
#'
#'Creates an SQLite database or replaces an existing database designed to hold instrument calibration data.
#'
#' @param path The path to the calibration SQLite database or the location where it should be created, with extension. Refer to calConnect for the default path refered to in that function as it may be appropriate to use here.
#' @param overwrite TRUE overwrites the database, if one exists in the same path.
#'
#' @return An SQLite database in the folder location specified by 'path'.
#' @export
#'

db_create <- function(path, overwrite = FALSE) {

  # Create a connection to a new SQLite database
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))

  if (overwrite){
    for (i in DBI::dbListTables(con)){
      DBI::dbExecute(con, paste0("DROP TABLE ", i))
    }
    # The DB will still be taking up space after deleting the tables. VACUUM removes empty space from database if you want to reclaim space. Otherwise, simply deleting tables preserves the "empty" space for future database use:
    DBI::dbExecute(con, "VACUUM")
  }


  # Create "instruments" table
  DBI::dbExecute(con,
                 "CREATE TABLE instruments (
          obs_datetime TEXT NOT NULL,
          observer TEXT NOT NULL,
          ID INTEGER PRIMARY KEY AUTOINCREMENT,
          make TEXT NOT NULL,
          model TEXT NOT NULL,
          type TEXT NOT NULL,
          serial_no TEXT NOT NULL,
          asset_tag TEXT,
          alias TEXT,
          has_sensors TEXT NOT NULL,
          date_in_service TEXT,
          date_purchased TEXT)")

  # Create "sensor_arrays" table
  DBI::dbExecute(con,
                 "CREATE TABLE sensor_arrays (
          ID INTEGER NOT NULL,
          obs_datetime TEXT NOT NULL,
          sensor1 TEXT NOT NULL,
          sensor2 TEXT,
          sensor3 TEXT,
          sensor4 TEXT,
          sensor5 TEXT,
          sensor6 TEXT,
          sensor7 TEXT,
          sensor8 TEXT,
          PRIMARY KEY (ID, obs_datetime)
          FOREIGN KEY (ID) REFERENCES instruments(ID))
          WITHOUT ROWID")

  # Create "calibrations" table
  DBI::dbExecute(con,
                 "CREATE TABLE calibrations (
          obs_datetime TEXT NOT NULL,
          observer TEXT NOT NULL,
          ID_sensor_holder INTEGER NOT NULL,
          ID_meter INTEGER,
          temp_reference_desc TEXT,
          temp_reference NUMERIC,
          temp_observed NUMERIC,
          pH1_std NUMERIC,
          pH1_pre_val NUMERIC,
          pH1_pre_mV NUMERIC,
          pH1_post_val NUMERIC,
          pH1_post_mV NUMERIC,
          pH2_std NUMERIC,
          pH2_pre_val NUMERIC,
          pH2_pre_mV NUMERIC,
          pH2_post_val NUMERIC,
          pH2_post_mV NUMERIC,
          pH3_std NUMERIC,
          pH3_pre_val NUMERIC,
          pH3_pre_mV NUMERIC,
          pH3_post_val NUMERIC,
          pH3_post_mV NUMERIC,
          ORP_std NUMERIC,
          ORP_pre_mV NUMERIC,
          ORP_post_mV NUMERIC,
          SpC1_std NUMERIC,
          SpC1_pre NUMERIC,
          SpC1_post NUMERIC,
          SpC2_std NUMERIC,
          SpC2_pre NUMERIC,
          SpC2_post NUMERIC,
          turb1_std NUMERIC,
          turb1_pre NUMERIC,
          turb1_post NUMERIC,
          turb2_std NUMERIC,
          turb2_pre_val NUMERIC,
          turb2_post_val NUMERIC,
          baro_press_pre NUMERIC,
          baro_press_post NUMERIC,
          DO_pre NUMERIC,
          DO_post NUMERIC,
          depth_check_ok TEXT,
          PRIMARY KEY (ID_sensor_holder, obs_datetime)
          FOREIGN KEY (ID_sensor_holder) REFERENCES instruments(ID)
          FOREIGN KEY (ID_meter) REFERENCES instruments(ID))")

  print(paste0("The database was successfully created at ", path, "."))
}
