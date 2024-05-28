#' Initial calibration database creation
#'
#'#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Populates a postgres database with the necessary tables for the calibration app. A database should be created before using this function, but it can be left empty.
#'
#' This function will also establish a mapping to the 'hydromet' database, which is used to populate the 'locations' table.
#'
#' @param con A connection to the target database
#' @param overwrite TRUE overwrites the database, if one exists in the same path.
#'
#' @return New tables in the target postgres database.
#' @export
#'

db_create <- function(con = calConnect(), overwrite = FALSE) {

  if (overwrite) {
    for (i in DBI::dbListTables(con)) {
      tryCatch({
        DBI::dbExecute(con, paste0("DROP TABLE ", i, " CASCADE"))
      }, error = function(e) {
        tryCatch({
          DBI::dbExecute(con, paste0("DROP VIEW ", i, " CASCADE"))
        }, error = function(e) {
          DBI::dbExecute(con, paste0("DROP FOREIGN TABLE ", i, " CASCADE"))
        })
      })
    }
    try(DBI::dbExecute(con, "DROP SERVER hydromet_server CASCADE"))
    # The DB will still be taking up space after deleting the tables. VACUUM removes empty space from database if you want to reclaim space. Otherwise, simply deleting tables preserves the "empty" space for future database use:
    DBI::dbExecute(con, "VACUUM")
  }

  # Establish a link to the hydromet/AquaCache database
  DBI::dbExecute(con, "CREATE EXTENSION IF NOT EXISTS postgres_fdw;")

  DBI::dbExecute(con, paste0("CREATE SERVER IF NOT EXISTS hydromet_server
FOREIGN DATA WRAPPER postgres_fdw
OPTIONS (host '", Sys.getenv("hydrometHost"), "', dbname 'hydromet', port '5433');
"))

  DBI::dbExecute(con, paste0("CREATE USER MAPPING FOR current_user
SERVER hydromet_server
OPTIONS (user '", Sys.getenv("hydrometUser"), "', password '", Sys.getenv("hydrometPass"), "');
"))

  DBI::dbExecute(con, "CREATE FOREIGN TABLE locations (
  location_id INTEGER NOT NULL,
  name TEXT,
  location TEXT,
  latitude NUMERIC,
  longitude NUMERIC
)
SERVER hydromet_server
OPTIONS (schema_name 'public', table_name 'locations');
")


  # Create "instruments" and related tables ########################################

  DBI::dbExecute(con, "CREATE TABLE instrument_type (
                 type_id SERIAL PRIMARY KEY,
                 type TEXT NOT NULL,
                 description TEXT NOT NULL
                 )
                 ;")

DBI::dbExecute(con, "CREATE TABLE instrument_make (
                 make_id SERIAL PRIMARY KEY,
                 make TEXT NOT NULL,
                 description TEXT
                 )
                 ;")

DBI::dbExecute(con, "CREATE TABLE instrument_model (
                 model_id SERIAL PRIMARY KEY,
                 model TEXT NOT NULL,
                 description TEXT
                 )
                 ;")

DBI::dbExecute(con, "CREATE TABLE observers (
                 observer_id SERIAL PRIMARY KEY,
                 observer_first TEXT NOT NULL,
                 observer_last TEXT NOT NULL
                 )
                 ;")

  DBI::dbExecute(con,
                 "CREATE TABLE instruments (
                  instrument_id SERIAL PRIMARY KEY,
                  obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                  observer NUMERIC NOT NULL,
                  make TEXT NOT NULL,
                  model INTEGER NOT NULL,
                  type TEXT NOT NULL,
                  holds_replaceable_sensors BOOLEAN NOT NULL DEFAULT FALSE,
                  serial_no TEXT NOT NULL,
                  asset_tag TEXT,
                  date_in_service DATE,
                  date_purchased DATE,
                  retired_by TEXT,
                  date_retired DATE,
                  UNIQUE (serial_no),
                  FOREIGN KEY (make) REFERENCES instrument_make(make_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (model) REFERENCES instrument_model(model_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (type) REFERENCES instrument_type(type_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (observer) REFERENCES observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")

  DBI::dbExecute(con,
                 "CREATE TABLE instrument_deployment (
                  instrument_id INTEGER NOT NULL,
                  deployment_start_date DATE NOT NULL,
                  deployment_end_date DATE,
                  deplyment_location TEXT,
                  deployment_purpose TEXT,
                  deployment_notes TEXT,
                  location_id INTEGER NOT NULL,
                  FOREIGN KEY (instrument_id) REFERENCES instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE
                  )
                 ;")


  # Create function and trigger for referential integrity, as foreign keys are not supported in foreign tables
  DBI::dbExecute(con, "
    CREATE OR REPLACE FUNCTION check_location_exists()
    RETURNS TRIGGER AS $$
    BEGIN
      IF NOT EXISTS (SELECT 1 FROM locations WHERE location_id = NEW.location_id) THEN
        RAISE EXCEPTION 'location_id % does not exist in locations table', NEW.location_id;
      END IF;
      RETURN NEW;
    END;
    $$ LANGUAGE plpgsql;
  ")

  DBI::dbExecute(con, "
    CREATE TRIGGER instrument_deployment_check
    BEFORE INSERT OR UPDATE ON instrument_deployment
    FOR EACH ROW EXECUTE FUNCTION check_location_exists();
  ")

  # Create "sensors" table ########################################
  DBI::dbExecute(con,
                 "CREATE TABLE sensors (
                  sensor_id SERIAL PRIMARY KEY,
                  sensor_type TEXT NOT NULL,
                  sensor_serial TEXT NOT NULL,
                  sensor_make TEXT NOT NULL,
                  sensor_model TEXT NOT NULL,
                  sensor_date_in_service DATE,
                  sensor_date_purchased DATE,
                  sensor_retired_by TEXT,
                  sensor_date_retired DATE,
                  sensor_asset_tag TEXT,
                  sensor_notes TEXT,
                  UNIQUE (sensor_serial)
                 )
                 ;")


  # Create "maintenance" tables ########################################
  DBI::dbExecute(con,
                 "CREATE TABLE instrument_maintenance (
                  instrument_id INTEGER NOT NULL,
                  observer INTEGER NOT NULL,
                  obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                  note TEXT NOT NULL,
                  FOREIGN KEY (instrument_id) REFERENCES instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (observer) REFERENCES observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")

DBI::dbExecute(con,
               "CREATE TABLE array_maintenance_changes (
                instrument_id INTEGER NOT NULL,
                observer INTEGER NOT NULL,
                obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                sensor1_id INTEGER NOT NULL,
                sensor1_notes TEXT,
                sensor2_id INTEGER,
                sensor2_notes TEXT,
                sensor3_id INTEGER,
                sensor3_notes TEXT,
                sensor4_id INTEGER,
                sensor4_notes TEXT,
                sensor5_id INTEGER,
                sensor5_notes TEXT,
                sensor6_id INTEGER,
                sensor6_notes TEXT,
                sensor7_id INTEGER,
                sensor7_notes TEXT,
                sensor8_id INTEGER,
                sensor8_notes TEXT,
                FOREIGN KEY (instrument_id) REFERENCES instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (observer) REFERENCES observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor1_id) REFERENCES sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor2_id) REFERENCES sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor3_id) REFERENCES sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor4_id) REFERENCES sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor5_id) REFERENCES sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor6_id) REFERENCES sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor7_id) REFERENCES sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE,
                FOREIGN KEY (sensor8_id) REFERENCES sensors(sensor_id) ON UPDATE CASCADE ON DELETE CASCADE
                )
               ;")
DBI::dbExecute(con, "COMMENT ON TABLE array_maintenance_changes IS 'This table is used to record changes to the sensors in an instrument array. Each row represents a single maintenance event, and the notes field should contain a description of the changes or maintenance made to each sensor. A simple maintenance event with no change of sensor should have the same sensor_id as the previous entry for that instrument and sensorX_id, while sensor changes must be recorded with a new sensor_id.'")



  # Create "calibrations" table and associated tables
  DBI::dbExecute(con,
                 "CREATE TABLE calibrations (
                  calibration_id SERIAL PRIMARY KEY,
                  observer INTEGER NOT NULL,
                  obs_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
                  id_sensor_holder INTEGER NOT NULL,
                  id_handheld_meter INTEGER,
                  complete BOOLEAN NOT NULL DEFAULT FALSE,
                  FOREIGN KEY (id_sensor_holder) REFERENCES instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (id_handheld_meter) REFERENCES instruments(instrument_id) ON UPDATE CASCADE ON DELETE CASCADE,
                  FOREIGN KEY (observer) REFERENCES observers(observer_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")

  DBI::dbExecute(con,
                 "CREATE TABLE calibrate_temperature (
                  calibration_id INTEGER NOT NULL,
                  temp_reference_desc TEXT NOT NULL,
                  temp_reference NUMERIC NOT NULL,
                  temp_observed NUMERIC NOT NULL,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")

  DBI::dbExecute(con,
                 "CREATE TABLE calibrate_specific_conductance (
                  calibration_id INTEGER NOT NULL,
                  SpC1_std NUMERIC NOT NULL,
                  SpC2_std NUMERIC NOT NULL,
                  SpC1_pre NUMERIC NOT NULL,
                  SpC2_pre NUMERIC NOT NULL,
                  SpC1_post NUMERIC NOT NULL,
                  SpC2_post NUMERIC NOT NULL,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")

 DBI::dbExecute(con,
                 "CREATE TABLE calibrate_pH (
                  calibration_id INTEGER NOT NULL,
                  pH1_std NUMERIC NOT NULL,
                  pH2_std NUMERIC NOT NULL,
                  pH3_std NUMERIC,
                  pH1_pre_val NUMERIC NOT NULL,
                  pH2_pre_val NUMERIC NOT NULL,
                  pH3_pre_val NUMERIC,
                  pH1_mV NUMERIC NOT NULL,
                  pH2_mV NUMERIC NOT NULL,
                  pH3_mV NUMERIC,
                  pH1_post_val NUMERIC NOT NULL,
                  pH2_post_val NUMERIC NOT NULL,
                  pH3_post_val NUMERIC,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                )
                ;")

  DBI::dbExecute(con,
                 "CREATE TABLE calibrate_ORP (
                  calibration_id INTEGER NOT NULL,
                  orp_std NUMERIC NOT NULL,
                  orp_pre_mV NUMERIC NOT NULL,
                  orp_post_mV NUMERIC NOT NULL,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")

  DBI::dbExecute(con,
                 "CREATE TABLE calibrate_turbidity (
                  calibration_id INTEGER NOT NULL,
                  turb1_std NUMERIC NOT NULL,
                  turb2_std NUMERIC NOT NULL,
                  turb1_pre NUMERIC NOT NULL,
                  turb2_pre NUMERIC NOT NULL,
                  turb1_post NUMERIC NOT NULL,
                  turb2_post NUMERIC NOT NULL,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")

  DBI::dbExecute(con,
                 "CREATE TABLE calibrate_dissolved_oxygen (
                  calibration_id INTEGER NOT NULL,
                  baro_press_pre NUMERIC NOT NULL,
                  baro_press_post NUMERIC NOT NULL,
                  DO_pre_mgl NUMERIC NOT NULL,
                  DO_post_mgl NUMERIC NOT NULL,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")

  DBI::dbExecute(con,
                 "CREATE TABLE calibrate_depth (
                  calibration_id INTEGER NOT NULL,
                  depth_check_ok NUMERIC NOT NULL,
                  depth_changes_ok NUMERIC NOT NULL,
                  PRIMARY KEY (calibration_id),
                  FOREIGN KEY (calibration_id) REFERENCES calibrations(calibration_id) ON UPDATE CASCADE ON DELETE CASCADE
                 )
                 ;")


  if (overwrite) {
    message("The database was overwritten.")
  } else {
    print("The database was successfully created")
  }

}
