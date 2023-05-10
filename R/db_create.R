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
          instrument_ID INTEGER PRIMARY KEY,
          make TEXT NOT NULL,
          model TEXT NOT NULL,
          type TEXT NOT NULL,
          serial_no TEXT NOT NULL,
          asset_tag TEXT,
          date_in_service TEXT,
          date_purchased TEXT,
          retired_by TEXTT,
          date_retired TEXT)
                 WITHOUT ROWID")

  # Create "sensor_arrays" table
  DBI::dbExecute(con,
                 "CREATE TABLE sensors (
          instrument_ID INTEGER NOT NULL,
          observer TEXT NOT NULL,
          obs_datetime TEXT NOT NULL,
          sensor1_type TEXT,
          sensor1_serial TEXT,
          sensor2_type TEXT,
          sensor2_serial TEXT,
          sensor3_type TEXT,
          sensor3_serial TEXT,
          sensor4_type TEXT,
          sensor4_serial TEXT,
          sensor5_type TEXT,
          sensor5_serial TEXT,
          sensor6_type TEXT,
          sensor6_serial TEXT,
          sensor7_type TEXT,
          sensor7_serial TEXT,
          sensor8_type TEXT,
          sensor8_serial TEXT,
          sensor1_notes TEXT,
          sensor2_notes TEXT,
          sensor3_notes TEXT,
          sensor4_notes TEXT,
          sensor5_notes TEXT,
          sensor6_notes TEXT,
          sensor7_notes TEXT,
          sensor8_notes TEXT,
          PRIMARY KEY (obs_datetime)
          FOREIGN KEY (instrument_ID) REFERENCES instruments(instrument_ID))
          WITHOUT ROWID")

  # Create "calibrations" table
  DBI::dbExecute(con,
                 "CREATE TABLE observations (
                 observation_ID INTEGER NOT NULL,
                 observer TEXT NOT NULL,
                 obs_datetime TEXT NOT NULL,
                 ID_sensor_holder TEXT NOT NULL,
                 ID_handheld_meter TEXT,
                 complete TEXT,
                 PRIMARY KEY (observation_ID)
                 FOREIGN KEY (ID_sensor_holder) REFERENCES instruments(instrument_ID)
                 FOREIGN KEY (ID_handheld_meter) REFERENCES instruments(instruments_ID))
                 WITHOUT ROWID")
  DBI::dbExecute(con,
                 "CREATE TABLE temperature (
                 observation_ID INTEGER NOT NULL,
                 temp_reference_desc TEXT NOT NULL,
                 temp_reference	NUMERIC NOT NULL,
                 temp_observed NUMERIC NOT NULL,
                 PRIMARY KEY (observation_ID)
                 FOREIGN KEY (observation_ID) REFERENCES observations(observation_ID))
                 WITHOUT ROWID")
  DBI::dbExecute(con,
                 "CREATE TABLE SpC (
                 observation_ID INTEGER NOT NULL,
                 SpC1_std NUMERIC NOT NULL,
                 SpC2_std	NUMERIC NOT NULL,
                 SpC1_pre NUMERIC NOT NULL,
                 SpC2_pre NUMERIC NOT NULL,
                 SpC1_post NUMERIC NOT NULL,
                 SpC2_post NUMERIC NOT NULL,
                 PRIMARY KEY (observation_ID)
                 FOREIGN KEY (observation_ID) REFERENCES observations(observation_ID))
                 WITHOUT ROWID")
  DBI::dbExecute(con,
                 "CREATE TABLE pH (
                 observation_ID INTEGER NOT NULL,
                 pH1_std NUMERIC NOT NULL,
                 pH2_std	NUMERIC NOT NULL,
                 pH3_std NUMERIC,
                 pH1_pre_val NUMERIC NOT NULL,
                 pH2_pre_val NUMERIC NOT NULL,
                 pH3_pre_val,
                 pH1_mV NUMERIC NOT NULL,
                 pH2_mV NUMERIC NOT NULL,
                 pH3_mV NUMERIC,
                 pH1_post_val NUMERIC NOT NULL,
                 pH2_post_val NUMERIC NOT NULL,
                 pH3_post_val,
                 PRIMARY KEY (observation_ID)
                 FOREIGN KEY (observation_ID) REFERENCES observations(observation_ID))
                 WITHOUT ROWID")
  DBI::dbExecute(con,
                 "CREATE TABLE ORP (
                 observation_ID INTEGER NOT NULL,
                 orp_std	NUMERIC NOT NULL,
                 orp_pre_mV NUMERIC NOT NULL,
                 orp_post_mV NUMERIC NOT NULL,
                 PRIMARY KEY (observation_ID)
                 FOREIGN KEY (observation_ID) REFERENCES observations(observation_ID))
                 WITHOUT ROWID")
  DBI::dbExecute(con,
                 "CREATE TABLE turbidity (
                 observation_ID INTEGER NOT NULL,
                 turb1_std NUMERIC NOT NULL,
                 turb2_std	NUMERIC NOT NULL,
                 turb1_pre NUMERIC NOT NULL,
                 turb2_pre NUMERIC NOT NULL,
                 turb1_post NUMERIC NOT NULL,
                 turb2_post NUMERIC NOT NULL,
                 PRIMARY KEY (observation_ID)
                 FOREIGN KEY (observation_ID) REFERENCES observations(observation_ID))
                 WITHOUT ROWID")
  DBI::dbExecute(con,
                 "CREATE TABLE DO (
                 observation_ID INTEGER NOT NULL,
                 baro_press_pre NUMERIC NOT NULL,
                 baro_press_post	NUMERIC NOT NULL,
                 DO_pre_mgl NUMERIC NOT NULL,
                 DO_post_mgl NUMERIC NOT NULL,
                 PRIMARY KEY (observation_ID)
                 FOREIGN KEY (observation_ID) REFERENCES observations(observation_ID))
                 WITHOUT ROWID")
  DBI::dbExecute(con,
                 "CREATE TABLE depth (
                 observation_ID INTEGER NOT NULL,
                 depth_check_ok NUMERIC NOT NULL,
                 depth_changes_ok	NUMERIC NOT NULL,
                 PRIMARY KEY (observation_ID)
                 FOREIGN KEY (observation_ID) REFERENCES observations(observation_ID))
                 WITHOUT ROWID")


  if (overwrite){
    print(paste0("The database was overwritten at ", path, "."))
  } else {
    print(paste0("The database was successfully created at ", path, "."))
  }

}
