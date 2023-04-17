#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #TODO: make the pH standards update if the default is changed
  #https://shiny.rstudio.com/reference/shiny/0.14/updatetextinput

  shinyalert::shinyalert("Loading data...", type = "info", timer = 3000)

  #set-up to connect to google drive using API
  googledrive::drive_auth(cache = "app/secret", email = "wrbcalibrates@gmail.com")
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  calibrations_id <- googledrive::drive_get("calibrations/calibrations")$id
  instruments_id <- googledrive::drive_get("calibrations/instruments")$id

  # create a few containers
  validation_check <- reactiveValues()
  calibration_data <- reactiveValues()
  send_table <- reactiveValues()
  incomplete <- reactiveValues()
  incomplete$basic <- FALSE
  incomplete$temperature <- FALSE
  incomplete$SpC <- FALSE
  incomplete$pH <- FALSE
  incomplete$ORP <- FALSE
  incomplete$turbidity <- FALSE
  incomplete$DO <- FALSE
  incomplete$depth <- FALSE

  # query the observations sheet for increment number and unfinished calibrations
  observations <- googlesheets4::read_sheet(calibrations_id, sheet = "observations")
  calibration_data$next_id <- max(observations$observation_ID) + 1  # find out the new observation ID number

  # find out if any calibrations are labelled as incomplete
  incomplete_observations <- observations[observations$complete == FALSE,]
  if (nrow(incomplete_observations) > 0){
    shinyalert::shinyalert(title = "Heads up: incomplete calibrations found!", text = "Go to to the page 'View unfinished calibrations' to see and finish or delete them.")
    incomplete$incomplete <- data.frame("Index" = seq(1, nrow(incomplete_observations)),
                                        "Calibrator" = as.vector(incomplete_observations$observer),
                                        "date" = as.character(incomplete_observations$obs_date),
                                        "time" = substr(as.character(incomplete_observations$obs_time), 12, 16),
                                        check.names = FALSE)
  } else {
    # Make a data.frame with no calibrations
    incomplete$incomplete <- data.frame("Index" = 0,
                                        "Calibrator" = "No unsaved calibrations!",
                                        "date/time" = "No unsaved calibrations!",
                                        check.names = FALSE)
  }

  #render the table if the user is on the proper panel
  output$incomplete_table <- renderTable({
    incomplete$incomplete
  })

  observeEvent(input$restart_calibration, {
    restart_value <- as.numeric(input$restart_index)
    if (restart_value == 0){
      shinyalert::shinyalert("0 is not a valid selection!", type = "error")
    } else {
      incomplete$basic <- TRUE
      shinyalert::shinyalert("Loading old calibration", type = "info", timer = 3000)
      incomplete_ID <- as.numeric(incomplete_observations[restart_value , 1])
      calibration_data$next_id <- incomplete_ID

      updateTextInput(session, "observer", value = incomplete_observations[restart_value , "observer"]$observer)
      updateDateInput(session, "obs_date", value = as.Date(incomplete_observations[restart_value , "obs_date"]$obs_date))
      shinyTime::updateTimeInput(session, "obs_time", value = as.POSIXct(incomplete_observations[restart_value , "obs_time"]$obs_time))
      updateTextInput(session, "ID_sensor_holder", value = incomplete_observations[restart_value , "ID_sensor_holder"]$ID_sensor_holder)
      updateTextInput(session, "ID_handheld_meter", value = incomplete_observations[restart_value , "ID_handheld_meter"]$ID_handheld_meter)

      # Search for entries in parameter-specific sheets with the same observation_ID and update the fields
      calibration_sheets <- googlesheets4::sheet_names(calibrations_id)
      for (i in calibration_sheets[calibration_sheets != "observations"]){
        sheet <- googlesheets4::read_sheet(calibrations_id, sheet = i)
        sheet <- sheet[sheet$observation_ID == incomplete_ID , ]
        if (nrow(sheet) == 1){
          if (i == "temperature"){
            incomplete$temperature <- TRUE
            updateTextInput(session, "temp_reference_desc", value = sheet$temp_reference_desc)
            updateNumericInput(session, "temp_reference", value = sheet$temp_reference)
            updateNumericInput(session, "temp_observed", value = sheet$temp_reference)
          } else if (i == "SpC"){
            incomplete$SpC <- TRUE
            updateNumericInput(session, "SpC1_std", value = sheet$SpC1_std)
            updateNumericInput(session, "SpC1_pre", value = sheet$SpC1_pre)
            updateNumericInput(session, "SpC1_post", value = sheet$SpC1_post)
            updateNumericInput(session, "SpC2_std", value = sheet$SpC2_std)
            updateNumericInput(session, "SpC2_pre", value = sheet$SpC2_pre)
            updateNumericInput(session, "SpC2_post", value = sheet$SpC2_post)
          } else if (i == "pH"){
            incomplete$pH <- TRUE
            updateNumericInput(session, "pH1_std", value = sheet$pH1_std)
            updateNumericInput(session, "pH2_std", value = sheet$pH2_std)
            updateNumericInput(session, "pH3_std", value = sheet$pH3_std)
            updateNumericInput(session, "pH1_pre_val", label = paste0("pH ", sheet$pH1_std, " Pre-Cal Value"), value =  sheet$pH1_pre_val)
            updateNumericInput(session, "pH1_pre_mV", label = paste0("pH ", sheet$pH1_std, " Pre-Cal mV"), value = sheet$pH1_pre_mV)
            updateNumericInput(session, "pH2_pre_val", label = paste0("pH ", sheet$pH2_std, " Pre-Cal Value"), value = sheet$pH2_pre_val)
            updateNumericInput(session, "pH2_pre_mV", label = paste0("pH ", sheet$pH2_std, " Pre-Cal mV"), value = sheet$pH2_pre_mV)
            updateNumericInput(session, "pH3_pre_val", label = paste0("pH ", sheet$pH3_std, " Pre-Cal Value"), value = sheet$pH3_pre_val)
            updateNumericInput(session, "pH3_pre_mV", label = paste0("pH ", sheet$pH3_std, " Pre-Cal mV"), value = sheet$pH3_pre_mV)
            updateNumericInput(session, "pH1_post_val", label = paste0("pH ", sheet$pH1_std, " Post-Cal Value"), value = sheet$pH1_post_val)
            updateNumericInput(session, "pH1_post_mV", label = paste0("pH ", sheet$pH1_std, " Post-Cal mV"), value = sheet$pH1_post_mV)
            updateNumericInput(session, "pH2_post_val", label = paste0("pH ", sheet$pH2_std, " Post-Cal Value"), value = sheet$pH2_post_val)
            updateNumericInput(session, "pH2_post_mV", label = paste0("pH ", sheet$pH2_std, " Post-Cal mV"), value = sheet$pH2_post_mV)
            updateNumericInput(session, "pH3_post_val", label = paste0("pH ", sheet$pH3_std, " Post-Cal Value"), value = sheet$pH3_post_val)
            updateNumericInput(session, "pH3_post_mV", label = paste0("pH ", sheet$pH3_std, " Post-Cal mV"), value = sheet$pH3_post_mV)
          } else if (i == "ORP"){
            incomplete$ORP <- TRUE
            updateNumericInput(session, "orp_std", value = sheet$ORP_std)
            updateNumericInput(session, "orp_pre_mV", value = sheet$ORP_pre_mV)
            updateNumericInput(session, "orp_post_mV", value = sheet$ORP_post_mV)
          } else if (i == "turbidity"){
            incomplete$turbidity <- TRUE
            updateNumericInput(session, "turb1_std", value = sheet$turb1_std)
            updateNumericInput(session, "turb2_std", value = sheet$turb2_std)
            updateNumericInput(session, "turb1_pre", value = sheet$turb1_pre)
            updateNumericInput(session, "turb2_pre", value = sheet$turb2_pre)
            updateNumericInput(session, "turb1_post", value = sheet$turb1_post)
            updateNumericInput(session, "turb2_post", value = sheet$turb2_post)
          } else if (i == "DO"){
            incomplete$DO <- TRUE
            updateNumericInput(session, "baro_press_pre", value = sheet$baro_press_pre)
            updateNumericInput(session, "baro_press_post", value = sheet$baro_press_post)
            updateNumericInput(session, "DO_pre", value = sheet$DO_pre)
            updateNumericInput(session, "DO_post", value = sheet$DO_post)
          } else if (i == "depth"){
            incomplete$depth <- TRUE
            updateRadioButtons(session, inputId = "depth_check_ok", selected = sheet$depth_check_ok)
            updateRadioButtons(session, inputId = "depth_changes_ok", selected = sheet$depth_changes_ok)
          }
        }
      }
    }
  })

  observeEvent(input$delete_calibration, {
    delete_value <- as.numeric(input$restart_index)
    if (delete_value == 0){
      shinyalert::shinyalert("0 is not a valid selection!", type = "error")
    } else {
      shinyalert::shinyalert("Deleting old calibration", type = "info", timer = 3000)
      delete_ID <- as.numeric(incomplete_observations[delete_value , 1])
      calibration_data$next_id <- delete_ID
      calibration_sheets <- googlesheets4::sheet_names(calibrations_id)
      for (i in calibration_sheets){
        sheet <- googlesheets4::read_sheet(calibrations_id, sheet = i)
        if (nrow(sheet[sheet$observation_ID == delete_ID , ]) == 1){
          row <- which(sheet$observation_ID == calibration_data$next_id)+1
          googlesheets4::range_clear(calibrations_id, sheet = i,range = paste0(row, ":", row))
        }
      }
    }
  })

  validation_check$pH <- FALSE
  observeEvent(input$validate_pH, { #Deal with the pH page
    tryCatch({
      #Check the standard values entered
      std1 <- as.numeric(input$pH1_std)
      std2 <- as.numeric(input$pH2_std)
      std3 <- as.numeric(input$pH3_std)
      warn_ph_std <- FALSE
      warn_ph_post <- FALSE
      warn_mv_post <- FALSE
      if (std1 != 4){
        shinyjs::js$backgroundCol("pH1_std", "lemonchiffon")
        warn_ph_std <- TRUE
      }
      if (std2 != 7){
        shinyjs::js$backgroundCol("pH2_std", "lemonchiffon")
        warn_ph_std <- TRUE
      }
      if (std3 != 10){
        shinyjs::js$backgroundCol("pH3_std", "lemonchiffon")
        warn_ph_std <- TRUE
      }
      #Validate the pH measurements vs the standards
      value1 <- as.numeric(input$pH1_post_val)
      if (value1 < (std1 - 0.1) | value1 > (std1 + 0.1) | is.null(value1)){ #tolerance of 0.1 pH units from the stated calibration standard value
        shinyjs::js$backgroundCol("pH1_post_val", "red")
        warn_ph_post <- TRUE
      } else {
        shinyjs::js$backgroundCol("pH1_post_val", "white")
      }
      value2 <- as.numeric(input$pH2_post_val)
      if (value2 < (std2 - 0.1) | value2 > (std2 + 0.1) | is.null(value2)){ #tolerance of 0.1 pH units from the stated calibration standard value
        shinyjs::js$backgroundCol("pH2_post_val", "red")
        warn_ph_post <- TRUE
      } else {
        shinyjs::js$backgroundCol("pH2_post_val", "white")
      }
      value3 <- as.numeric(input$pH3_post_val)
      if (value3 < (std3 - 0.1) | value3 > (std3 + 0.1) | is.null(value3)){ #tolerance of 0.1 pH units from the stated calibration standard value
        shinyjs::js$backgroundCol("pH3_post_val", "red")
        warn_ph_post <- TRUE
      } else {
        shinyjs::js$backgroundCol("pH3_post_val", "white")
      }
      # Validate the mV readings
      pH1_mV <- as.numeric(input$pH1_post_mV)
      pH2_mV <- as.numeric(input$pH2_post_mV)
      pH3_mV <- as.numeric(input$pH3_post_mV)
      if ((pH1_mV < (165 + pH2_mV)) | (pH1_mV > (180 + pH2_mV))   & (std1 > 3.9 & std1 < 4.1)){
        shinyjs::js$backgroundCol("pH1_post_mV", "red")
        warn_mv_post <- TRUE
      } else if (std1 != 4){
        shinyjs::js$backgroundCol("pH1_post_mV", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("pH1_post_mV", "white")
      }
      if ((pH2_mV > 50 | pH2_mV < -50) & (std2 > 6.9 & std2 < 7.1)){
        shinyjs::js$backgroundCol("pH2_post_mV", "red")
        warn_mv_post <- TRUE
      } else if (std2 != 7){
        shinyjs::js$backgroundCol("pH2_post_mV", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("pH2_post_mV", "white")
      }
      if ((pH3_mV > (165 - pH2_mV)) | (pH3_mV < (180 - pH2_mV))   & (std3 > 9.9 & std3 < 10.1)){
        shinyjs::js$backgroundCol("pH3_post_mV", "red")
        warn_mv_post <- TRUE
      } else if (std3 != 10){
        shinyjs::js$backgroundCol("pH3_post_mV", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("pH3_post_mV", "white")
      }
      #TODO: chain these shinyalerts together
      if (warn_ph_std){
        shinyalert::shinyalert(title = "Are you sure your standards are correct? If yes, checks on mV outputs will be invalid; use your judgement.", type = "warning")
      }
      if (warn_ph_post){
        shinyalert::shinyalert(title = "Some of your post calibration pH values are > 0.1 units from their standards! Check your inputs.", type = "warning")
      }
      if (warn_mv_post){
        shinyalert::shinyalert(title = "Some of your post calibration mV values are outside of the valid range!", text = "Re-check your measurements, and if the problem persists consider replacing the electrode (step 1) or entire sensor (last resort). ", type = "warning")
      }
      validation_check$pH <- TRUE
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", text = "If doing a 2-point calibration enter 0 for the third solution values to pass this check.", type = "error")
    })
  })
  validation_check$ORP <- FALSE
  observeEvent(input$validate_ORP, { #Deal with the temp page
    tryCatch({
      ORP_std <- input$orp_std
      ORP_post <- input$orp_post_mV
      ORP_diff <- abs(ORP_std - ORP_post)
      if (ORP_diff > 5){
        shinyjs::js$backgroundCol("orp_std", "red")
        shinyjs::js$backgroundCol("orp_post", "red")
      } else if (ORP_diff > 5){
        shinyjs::js$backgroundCol("orp_std", "lemonchiffon")
        shinyjs::js$backgroundCol("orp_post", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("orp_std", "white")
        shinyjs::js$backgroundCol("orp_post", "white")
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      }
      validation_check$ORP <- TRUE
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  })
  validation_check$temp <- FALSE
  observeEvent(input$validate_temp, { #Deal with the temp page
    tryCatch({
      temp_re_type <- input$temp_reference_desc
      temp_ref <- input$temp_reference
      temp_meas <- input$temp_observed
      temp_diff <- abs(temp_ref - temp_meas)
      if (temp_diff > 0.2){
        shinyjs::js$backgroundCol("temp_observed", "red")
        shinyjs::js$backgroundCol("temp_reference", "red")
        shinyalert::shinyalert(title = "Warning: double check your temperature, consider replacing this sensor!", type = "warning", timer = 2000)
      } else if (temp_diff > 0.1){
        shinyjs::js$backgroundCol("temp_observed", "lemonchiffon")
        shinyjs::js$backgroundCol("temp_reference", "lemonchiffon")
        shinyalert::shinyalert(title = "Warning: double check your temperature, consider replacing this sensor!", type = "warning", timer = 2000)
      } else {
        shinyjs::js$backgroundCol("temp_observed", "white")
        shinyjs::js$backgroundCol("temp_reference", "white")
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      }
      validation_check$temp <- TRUE
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  })
  validation_check$SpC <- FALSE
  observeEvent(input$validate_SpC, { #Deal with SpC
    tryCatch({
      SpC1_ref <- input$SpC1_std
      SpC2_ref <- input$SpC2_std
      SpC1_post <- input$SpC1_post
      SpC2_post <- input$SpC2_post
      SpC1_diff <- abs(SpC1_ref - SpC1_post)
      SpC2_diff <- abs(SpC2_ref - SpC2_post)
      if (SpC1_diff > 10){
        shinyjs::js$backgroundCol("SpC1_std", "red")
        shinyjs::js$backgroundCol("SpC1_post", "red")
        shinyalert::shinyalert(title = "Warning: double check your values, you're way off", type = "warning", timer = 2000)
      } else if (SpC1_diff > 5){
        shinyjs::js$backgroundCol("SpC1_std", "lemonchiffon")
        shinyjs::js$backgroundCol("SpC1_post", "lemonchiffon")
        shinyalert::shinyalert(title = "Warning: double check your values, you're a bit off", type = "warning", timer = 2000)
      } else {
        shinyjs::js$backgroundCol("SpC1_std", "white")
        shinyjs::js$backgroundCol("SpC1_post", "white")
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      }
      if (SpC2_diff > 10){
        shinyjs::js$backgroundCol("SpC2_std", "red")
        shinyjs::js$backgroundCol("SpC2_post", "red")
      } else if (SpC2_diff > 5){
        shinyjs::js$backgroundCol("SpC2_std", "lemonchiffon")
        shinyjs::js$backgroundCol("SpC2_post", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("SpC2_std", "white")
        shinyjs::js$backgroundCol("SpC2_post", "white")
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      }
      validation_check$SpC <- TRUE
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  })
  validation_check$turb <- FALSE
  observeEvent(input$validate_turb, { #Deal with turbidity
    tryCatch({
      turb1_ref <- input$turb1_std
      turb2_ref <- input$turb2_std
      turb1_post <- input$turb1_post
      turb2_post <- input$turb2_post
      turb1_diff <- abs(turb1_ref - turb1_post)
      turb2_diff <- abs(turb2_ref - turb2_post)
      gtg1 <- FALSE
      gtg2 <- FALSE
      if (turb1_diff > 10){
        shinyjs::js$backgroundCol("turb1_std", "red")
        shinyjs::js$backgroundCol("turb1_post", "red")
      } else if (turb1_diff > 5){
        shinyjs::js$backgroundCol("turb1_std", "lemonchiffon")
        shinyjs::js$backgroundCol("turb1_post", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("turb1_std", "white")
        shinyjs::js$backgroundCol("turb1_post", "white")
        gtg1 <- TRUE
      }
      if (turb2_diff > 10){
        shinyjs::js$backgroundCol("turb2_std", "red")
        shinyjs::js$backgroundCol("turb2_post", "red")
      } else if (turb2_diff > 5){
        shinyjs::js$backgroundCol("turb2_std", "lemonchiffon")
        shinyjs::js$backgroundCol("turb2_post", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("turb2_std", "white")
        shinyjs::js$backgroundCol("turb2_post", "white")
        gtg2 <- TRUE
      }
      if (gtg1 & gtg2){
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      } else {
        shinyalert::shinyalert(title = "Your post-cal values are a bit off from the standard. Please check you entries before moving on.", type = "warning", timer = 2000)
      }
      validation_check$turb <- TRUE
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  })
  validation_check$DO <- FALSE
  observeEvent(input$validate_DO, { #Deal with DO
    tryCatch({
      baro_post <- input$baro_press_post
      DO_post <- input$DO_post
      shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      validation_check$DO <- TRUE
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  })
  validation_check$depth <- FALSE
  observeEvent(input$validate_depth, { #Deal with depth
    tryCatch({
      depth_check <- input$depth_check_ok
      depth_change <- input$depth_changes_ok
      if (depth_check == "TRUE" & depth_change == "TRUE"){
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      } else {
        shinyalert::shinyalert(title = "You indicated FALSE for one of the values. Are you sure about that? Should you be using a different sensor?", type = "warning")
      }
      validation_check$depth <- TRUE
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  })

  # Populate data.frames for calibration data
  send_table$saved <- data.frame("Saved Calibrations" = "Nothing saved yet", check.names = FALSE)

  observeEvent(input$save_basic_info, {
    #Check length of observer, sensor holder, handheld holder
    validation_check$basic <- TRUE
    if (nchar(input$observer) < 2){
      validation_check$basic <- FALSE
      shinyalert::shinyalert(title = "Fill in the calibrator name", type = "error", timer = 2000)
    }
    if (nchar(input$ID_sensor_holder) < 4){
      validation_check$basic <- FALSE
      shinyalert::shinyalert(title = "Fill in the logger/bulkhead serial #", type = "error", timer = 2000)
    }
    if (input$ID_handheld_meter == "NA"){
      shinyalert::shinyalert(title = "Warning: no handheld specified", text = "Handheld only necessary with handheld/bulkhead combos; sondes and loggers are self-contain for calibrations.", type = "warning")
    }

    if (validation_check$basic){
      calibration_data$basic <- data.frame(observation_ID = calibration_data$next_id,
                                           observer = input$observer,
                                           obs_date = input$obs_date,
                                           obs_time = input$obs_time,
                                           ID_sensor_holder = input$ID_sensor_holder,
                                           ID_handheld_meter = input$ID_handheld_meter,
                                           complete = FALSE)
      if (!incomplete$basic){
        googlesheets4::sheet_append(calibrations_id, sheet="observations", data = calibration_data$basic)
      } else {
        googlesheets4::range_write(calibrations_id, data = calibration_data$basic, sheet = "observations", range = paste0("A", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      }

      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "Basic_info"
      } else if (!("Basic_info" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "Basic_info"
      } else if ("Basic_info" %in% send_table$saved[ ,1]){
        shinyalert::shinyalert(title = "Basic info overwritten", type = "success", timer = 2000)
      }
      shinyalert::shinyalert(title = "Basic info saved", type = "success", timer = 2000)
    }
  })

  observeEvent(input$save_cal_pH, {
    if (validation_check$pH){
      calibration_data$pH <- data.frame(observation_ID = calibration_data$next_id,
                                        pH1_std = input$pH1_std,
                                        pH1_pre_val = input$pH1_pre_val,
                                        pH1_pre_mV = input$pH1_pre_mV,
                                        pH1_post_val = input$pH1_post_val,
                                        pH1_post_mV = input$pH1_post_mV,
                                        pH2_std = input$pH2_std,
                                        pH2_pre_val = input$pH2_pre_val,
                                        pH2_pre_mV = input$pH2_pre_mV,
                                        pH2_post_val = input$pH2_post_val,
                                        pH2_post_mV = input$pH2_post_mV,
                                        pH3_std = input$pH3_std,
                                        pH3_pre_val = input$pH3_pre_val,
                                        pH3_pre_mV = input$pH3_pre_mV,
                                        pH3_post_val = input$pH3_post_val,
                                        pH3_post_mV = input$pH3_post_mV)
      if (!incomplete$pH){
        googlesheets4::sheet_append(calibrations_id, sheet="pH", data = calibration_data$pH)
      } else {
        googlesheets4::range_write(calibrations_id, data = calibration_data$pH, sheet = "pH", range = paste0("A", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "pH calibration"
      } else if (!("pH calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "pH calibration"
      }
      shinyalert::shinyalert(title = "pH calibration saved",
                             type = "success", timer = 2000)
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }

  })
  observeEvent(input$save_cal_temp, {
    if (validation_check$temp){
      calibration_data$temp <- data.frame(observation_ID = calibration_data$next_id,
                                          temp_reference_desc = input$temp_reference_desc,
                                          temp_reference = input$temp_reference,
                                          temp_observed = input$temp_observed)
      if (!incomplete$temperature){
        googlesheets4::sheet_append(calibrations_id, sheet="temperature", data = calibration_data$temp)
      } else {
        googlesheets4::range_write(calibrations_id, data = calibration_data$temp, sheet = "temperature", range = paste0("A", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "Temperature calibration"
      } else if (!("Temperature calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "Temperature calibration"
      }
      shinyalert::shinyalert(title = "Temperature calibration saved",
                             type = "success", timer = 2000)
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$save_cal_ORP, {
    if (validation_check$ORP){
      calibration_data$ORP <- data.frame(observation_ID = calibration_data$next_id,
                                         ORP_std = input$ORP_std,
                                         ORP_pre_mV = input$ORP_pre_mV,
                                         ORP_post_mV = input$ORP_post_mV)
      if (!incomplete$ORP){
        googlesheets4::sheet_append(calibrations_id, sheet="ORP", data = calibration_data$ORP)
      } else {
        googlesheets4::range_write(calibrations_id, data = calibration_data$ORP, sheet = "ORP", range = paste0("A", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "ORP calibration"
      } else if (!("ORP calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "ORP calibration"
      }
      shinyalert::shinyalert(title = "ORP calibration saved",
                             type = "success", timer = 2000)
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$save_cal_SpC, {
    if (validation_check$SpC){
      calibration_data$SpC <- data.frame(observation_ID = calibration_data$next_id,
                                         SpC1_std = input$SpC1_std,
                                         SpC1_pre = input$SpC1_pre,
                                         SpC1_post = input$SpC1_post,
                                         SpC2_std = input$SpC2_std,
                                         SpC2_pre = input$SpC2_pre,
                                         SpC2_post = input$SpC2_post)
      if (!incomplete$SpC){
        googlesheets4::sheet_append(calibrations_id, sheet="SpC", data = calibration_data$SpC)
      } else {
        googlesheets4::range_write(calibrations_id, data = calibration_data$SpC, sheet = "SpC", range = paste0("A", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "SpC calibration"
      } else if (!("SpC calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "SpC calibration"
      }
      shinyalert::shinyalert(title = "SpC calibration saved",
                             type = "success", timer = 2000)
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$save_cal_turb, {
    if (validation_check$turb){
      calibration_data$turb <- data.frame(observation_ID = calibration_data$next_id,
                                          turb1_std = input$turb1_std,
                                          turb1_pre = input$turb1_pre,
                                          turb1_post = input$turb1_post,
                                          turb2_std = input$turb2_std,
                                          turb2_pre = input$turb2_pre,
                                          turb2_post = input$turb2_post)
      if (!incomplete$turbidity){
        googlesheets4::sheet_append(calibrations_id, sheet="turbidity", data = calibration_data$turb)
      } else {
        googlesheets4::range_write(calibrations_id, data = calibration_data$turb, sheet = "turbidity", range = paste0("A", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "Turbidity calibration"
      } else if (!("Turbidity calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "Turbidity calibration"
      }
      shinyalert::shinyalert(title = "Turbidity calibration saved",
                             type = "success", timer = 2000)
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$save_cal_DO, {
    if (validation_check$DO){
      calibration_data$DO <- data.frame(observation_ID = calibration_data$next_id,
                                        baro_press_pre = input$baro_press_pre,
                                        baro_press_post = input$baro_press_post,
                                        DO_pre = input$DO_pre,
                                        DO_post = input$DO_post)
      if (!incomplete$DO){
        googlesheets4::sheet_append(calibrations_id, sheet="DO", data = calibration_data$DO)
      } else {
        googlesheets4::range_write(calibrations_id, data = calibration_data$DO, sheet = "DO", range = paste0("A", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "DO calibration"
      } else if (!("DO calibration" %in% send_table$saved[ ,1])){
        send_table$saved[nrow(send_table$saved)+1,1] <- "DO calibration"
      }
      shinyalert::shinyalert(title = "DO calibration saved",
                             type = "success", timer = 2000)
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$save_cal_depth, {
    if (validation_check$depth){
      calibration_data$depth <- data.frame(depth_check_ok = input$depth_check_ok,
                                           depth_changes_ok = input$depth_changes_ok)
      if (!incomplete$depth){
        googlesheets4::sheet_append(calibrations_id, sheet="depth", data = calibration_data$depth)
      } else {
        googlesheets4::range_write(calibrations_id, data = calibration_data$depth, sheet = "depth", range = paste0("A", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "Depth calibration"
      } else if (!("Depth calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "Depth calibration"
      }
      shinyalert::shinyalert(title = "Depth calibration saved",
                             type = "success", timer = 2000)
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })

  # Function to display saved calibrations tables
  output$calibration_table <- renderTable({
    send_table$saved
  })

  observeEvent(input$submit_btn, {
    if (send_table$saved[1,1] == "Nothing saved yet"){
      shinyalert::shinyalert(title = "There is no calibration information to send yet!!!",
                 type = "error")
    } else if (!("Basic_info" %in% send_table$saved[ ,1])){
      shinyalert::shinyalert(title = "There is no basic information yet!!!", text = "Fill in your name, calibration time and date, and required serial numbers.", type = "error")
    } else if (!("Temperature calibration" %in% send_table$saved[ ,1])){
      shinyalert::shinyalert(title = "Temperature calibration is mandatory", text = "If you've filled it in already you probably forgot to save it!", type = "error")
    } else {
      # Send the calibrations and mark it as complete == TRUE after everything is sent

      cals_saved <- paste(send_table$saved[, 1], collapse = " ")
      cals_saved <- gsub(" calibration", "", cals_saved)
      cals_saved <- gsub(" ", ", ", cals_saved)

      #read in observations again as the sheet now has a new row or a row that is being edited from incomplete calibration.
      observations <- googlesheets4::read_sheet(calibrations_id, sheet = "observations")
      googlesheets4::range_write(calibrations_id, data = data.frame(complete = TRUE), sheet = "observations", range = paste0("G", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)

      shinyalert::shinyalert(title = paste0("Calibration data for ", cals_saved, " has been sent"),
                 type = "success", immediate = TRUE)
      Sys.sleep(5)
      stopApp()
    }
  })
}
