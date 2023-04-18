#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  shinyalert::shinyalert("Loading data...", type = "info", timer = 4000)

  # create a few containers
  validation_check <- reactiveValues()
  calibration_data <- reactiveValues()
  instruments_data <- reactiveValues()
  send_table <- reactiveValues()
  messages <- reactiveValues()
  complete <- reactiveValues()
  complete$basic <- FALSE
  complete$temperature <- FALSE
  complete$SpC <- FALSE
  complete$pH <- FALSE
  complete$orp <- FALSE
  complete$turbidity <- FALSE
  complete$DO <- FALSE
  complete$depth <- FALSE

  # Hide a bunch of buttons until they can be used
  shinyjs::hide("delete_pH")
  shinyjs::hide("delete_turb")
  shinyjs::hide("delete_SpC")
  shinyjs::hide("delete_temp")
  shinyjs::hide("delete_orp")
  shinyjs::hide("delete_DO")
  shinyjs::hide("delete_depth")


  #message for the instrument management table
  messages$manage_instruments <- "You can add a new instrument or edit an existing entry. If adding a new instrument you must select 'New record' under 'Search existing serial numbers'. Only serial number and instrument type are mandatory, but please be as thorough as possible."
  messages$instrument_reminder <- "Add your instrument if not listed!"

  # Initiate data.frame to populate with save calibrations later
  send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE) #Title is modified later for clarity if user want to restart a cal
  send_table$restarted_cal <- data.frame("Saved calibrations (recovered session)" = "Basic info", check.names = FALSE)

  #output the messages
  output$manage_text <- renderText({
    messages$manage_instruments
  })
  output$instrument_reminder <- renderText({
    messages$instrument_reminder
  })

  #set-up to connect to google drive using API
  googledrive::drive_auth(cache = "app/secret", email = "wrbcalibrates@gmail.com")
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  calibrations_id <- googledrive::drive_get("calibrations/calibrations")$id
  instruments_id <- googledrive::drive_get("calibrations/instruments")$id
  instruments_sheet <- googlesheets4::read_sheet(instruments_id, sheet = "instruments")
  instruments_data$sheet <- instruments_sheet #assign to a reactive
  instruments_data$handhelds <- instruments_sheet[instruments_sheet$type == "Handheld (connects to bulkheads)" & is.na(instruments_sheet$date_retired) , ]
  instruments_data$others <- instruments_sheet[instruments_sheet$type != "Handheld (connects to bulkheads)" & is.na(instruments_sheet$date_retired) , ]

  # query the observations sheet for increment number and unfinished calibrations
  observations <- googlesheets4::read_sheet(calibrations_id, sheet = "observations")
  calibration_data$next_id <- max(observations$observation_ID) + 1  # find out the new observation ID number

  # find out if any calibrations are labelled as incomplete
  incomplete_observations <- observations[observations$complete == FALSE,]
  if (nrow(incomplete_observations) > 0){
    shinyalert::shinyalert(title = "Incomplete calibrations found!", text = "Go to to the page 'View unfinished calibrations' to restart or delete them.")
    complete$incomplete <- data.frame("Index" = seq(1, nrow(incomplete_observations)),
                                        "Calibrator" = as.vector(incomplete_observations$observer),
                                        "date" = as.character(incomplete_observations$obs_date),
                                        "time" = substr(as.character(incomplete_observations$obs_time), 12, 16),
                                        check.names = FALSE)
  } else {
    # Make a data.frame with no calibrations
    complete$incomplete <- data.frame("Index" = "0",
                                        "Calibrator" = "No unsaved calibrations!",
                                        "date/time" = "No unsaved calibrations!",
                                        check.names = FALSE)
  }

  output$incomplete_table <- renderTable({  #render the incomplete table if the user is on the proper panel
    complete$incomplete
  })
  observeEvent(input$pH1_std, {
    updateNumericInput(session, "pH1_pre_val", label = paste0("pH ", input$pH1_std, " Pre-Cal Value"))
    updateNumericInput(session, "pH1_pre_mV", label = paste0("pH ", input$pH1_std, " Pre-Cal mV"))
    updateNumericInput(session, "pH1_post_val", label = paste0("pH ", input$pH1_std, " Post-Cal Value"))
    updateNumericInput(session, "pH1_post_mV", label = paste0("pH ", input$pH1_std, " Post-Cal mV"))
  })
  observeEvent(input$pH2_std, {
    updateNumericInput(session, "pH2_pre_val", label = paste0("pH ", input$pH2_std, " Pre-Cal Value"))
    updateNumericInput(session, "pH2_pre_mV", label = paste0("pH ", input$pH2_std, " Pre-Cal mV"))
    updateNumericInput(session, "pH2_post_val", label = paste0("pH ", input$pH2_std, " Post-Cal Value"))
    updateNumericInput(session, "pH2_post_mV", label = paste0("pH ", input$pH2_std, " Post-Cal mV"))
  })
  observeEvent(input$pH3_std, {
    updateNumericInput(session, "pH3_pre_val", label = paste0("pH ", input$pH3_std, " Pre-Cal Value"))
    updateNumericInput(session, "pH3_pre_mV", label = paste0("pH ", input$pH3_std, " Pre-Cal mV"))
    updateNumericInput(session, "pH3_post_val", label = paste0("pH ", input$pH3_std, " Post-Cal Value"))
    updateNumericInput(session, "pH3_post_mV", label = paste0("pH ", input$pH3_std, " Post-Cal mV"))
  })

  #observeEvents for when the user selects a particular page
  observeEvent(input$first_selection, { #update selections to incorporate the new record
    if (input$first_selection == "Manage instruments"){
      shinyjs::hide("submit_btn")
      updateSelectInput(session, "existing_serial_no", choices = c("New record", instruments_data$sheet$serial_no))
      updateDateInput(session, "date_retired", value = NA) #Reset the retired date to NA
    } else if (input$first_selection == "Calibrate"){
      shinyjs::show("submit_btn")
      updateSelectInput(session, "ID_sensor_holder", choices = instruments_data$others$serial_no)
      updateSelectInput(session, "ID_handheld_meter", choices = c("NA", instruments_data$handhelds$serial_no))
    } else if (input$first_selection == "View unfinished calibrations"){
      shinyjs::hide("submit_btn")
      updateNumericInput(session, "restart_index", min = 0, max = nrow(incomplete_observations))
    }
  })
  observeEvent(input$existing_serial_no, { #populate fields as required
    if (input$existing_serial_no != "New record"){
      modify_record <- instruments_data$sheet[instruments_data$sheet$serial_no == input$existing_serial_no ,]
      updateTextInput(session, "serial_no", value = modify_record$serial_no)
      updateTextInput(session, "recorder", value = modify_record$observer)
      updateSelectInput(session, "make", selected = modify_record$make)
      updateTextInput(session, "model", value = modify_record$model)
      updateSelectInput(session, "type", selected = modify_record$type)
      updateTextInput(session, "asset_tag", value = modify_record$asset_tag)
      updateDateInput(session, "date_in_service", value = modify_record$date_in_service)
      updateDateInput(session, "date_purchased", value = modify_record$date_purchased)
      updateTextInput(session, "retired_by", value = modify_record$retired_by)
      updateDateInput(session, "date_retired", value = modify_record$date_retired)
    } else if (input$existing_serial_no == "New record"){
      updateTextInput(session, "serial_no", value = "")
      updateTextInput(session, "recorder", value = "")
      updateSelectInput(session, "make", selected = "")
      updateTextInput(session, "model", value = "")
      updateSelectInput(session, "type", selected = "")
      updateTextInput(session, "asset_tag", value = "")
      updateDateInput(session, "date_in_service", value = Sys.Date())
      updateDateInput(session, "date_purchased", value = Sys.Date())
      updateTextInput(session, "retired_by", value = "")
      updateDateInput(session, "date_retired", value = NA)
    }
  })
  observeEvent(input$save_cal_instrument, { #save the new record or the changes to existing record
    new_id <- max(as.numeric(instruments_data$sheet$instrument_ID) + 1)
    if (input$existing_serial_no == "New record"){ #add a new row with the next instrument_ID
      #first check to make sure the serial no does not already exist
      check <- input$serial_no %in% instruments_data$sheet$serial_no
      if (check) {
        shinyalert::shinyalert("Serial number already exists!", text = "You selected 'New record' and then entered an existing serial number.", type = "error")
      } else { #Make a new entry
        instrument.df <- data.frame(instrument_ID = new_id,
                                    observer = input$recorder,
                                    obs_datetime = as.character(Sys.time()),
                                    make = input$make,
                                    model = input$model,
                                    type = input$type,
                                    serial_no = input$serial_no,
                                    asset_tag = input$asset_tag,
                                    date_in_service = input$date_in_service,
                                    date_purchased = input$date_purchased,
                                    retired_by = input$retired_by,
                                    date_retired = input$date_retired)
        googlesheets4::sheet_append(instruments_id, sheet = "instruments", data = instrument.df)
      }
    } else { #Modify an existing entry
      #find the row number for the existing observation
      instruments_data$sheet
      row <- which(instruments_data$sheet$serial_no == input$serial_no)+1
      instrument.df <- data.frame(instrument_ID = new_id,
                                  observer = input$recorder,
                                  obs_datetime = as.character(Sys.time()),
                                  make = input$make,
                                  model = input$model,
                                  type = input$type,
                                  serial_no = input$serial_no,
                                  asset_tag = input$asset_tag,
                                  date_in_service = input$date_in_service,
                                  date_purchased = input$date_purchased,
                                  retired_by = input$retired_by,
                                  date_retired = input$date_retired)
      googlesheets4::range_write(instruments_id, sheet = "instruments", data = instrument.df, range = paste0(row, ":", row) , col_names = FALSE)
    }
    instruments_sheet <- googlesheets4::read_sheet(instruments_id, sheet = "instruments")
    instruments_data$sheet <- instruments_sheet #assign to a reactive
    instruments_data$handhelds <- instruments_sheet[instruments_sheet$type == "Handheld (connects to bulkheads)" & is.na(instruments_sheet$date_retired) , ]
    instruments_data$others <- instruments_sheet[instruments_sheet$type != "Handheld (connects to bulkheads)" & is.na(instruments_sheet$date_retired) , ]
  })

  observeEvent(input$restart_calibration, {
    restart_value <- as.numeric(input$restart_index)
    if (restart_value == 0){
      shinyalert::shinyalert("0 is not a valid selection!", type = "error")
    } else {
      send_table$restarted_cal <- data.frame("Saved calibrations (recovered session)" = "Basic info", check.names = FALSE) #Set/reset here for if the user selects a different calibration in the same session
      complete$basic <- TRUE
      shinyalert::shinyalert("Loading old calibration", text = "Please wait to be taken to the calibration section", type = "info", timer = 5000)
      incomplete_ID <- as.numeric(incomplete_observations[restart_value , 1])
      calibration_data$next_id <- incomplete_ID

      updateTextInput(session, "observer", value = incomplete_observations[restart_value , "observer"]$observer)
      updateDateInput(session, "obs_date", value = as.Date(incomplete_observations[restart_value , "obs_date"]$obs_date))
      shinyTime::updateTimeInput(session, "obs_time", value = as.POSIXct(incomplete_observations[restart_value , "obs_time"]$obs_time))
      updateTextInput(session, "ID_sensor_holder", value = incomplete_observations[restart_value , "ID_sensor_holder"]$ID_sensor_holder)
      updateTextInput(session, "ID_handheld_meter", value = incomplete_observations[restart_value , "ID_handheld_meter"]$ID_handheld_meter)

      # Search for entries in parameter-specific sheets with the same observation_ID and update the fields
      calibration_sheets <- googlesheets4::sheet_names(calibrations_id)
      for (i in calibration_sheets[calibration_sheets != "observations"]) {
        sheet <- googlesheets4::read_sheet(calibrations_id, sheet = i)
        sheet <- sheet[sheet$observation_ID == incomplete_ID , ]
        if (nrow(sheet) == 1){
          if (i == "temperature"){
            output_name <- "Temperature calibration"
            complete$temperature <- TRUE
            updateTextInput(session, "temp_reference_desc", value = sheet$temp_reference_desc)
            updateNumericInput(session, "temp_reference", value = sheet$temp_reference)
            updateNumericInput(session, "temp_observed", value = sheet$temp_reference)
            shinyjs::show("delete_temp")
          } else if (i == "SpC"){
            output_name <- "Conductivity calibration"
            complete$SpC <- TRUE
            updateNumericInput(session, "SpC1_std", value = sheet$SpC1_std)
            updateNumericInput(session, "SpC1_pre", value = sheet$SpC1_pre)
            updateNumericInput(session, "SpC1_post", value = sheet$SpC1_post)
            updateNumericInput(session, "SpC2_std", value = sheet$SpC2_std)
            updateNumericInput(session, "SpC2_pre", value = sheet$SpC2_pre)
            updateNumericInput(session, "SpC2_post", value = sheet$SpC2_post)
            shinyjs::show("delete_SpC")
          } else if (i == "pH"){
            output_name <- "pH calibration"
            complete$pH <- TRUE
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
            shinyjs::show("delete_pH")
          } else if (i == "ORP"){
            output_name <- "ORP calibration"
            complete$orp <- TRUE
            updateNumericInput(session, "orp_std", value = sheet$orp_std)
            updateNumericInput(session, "orp_pre_mV", value = sheet$orp_pre_mV)
            updateNumericInput(session, "orp_post_mV", value = sheet$orp_post_mV)
            shinyjs::show("delete_orp")
          } else if (i == "turbidity"){
            output_name <- "Turbidity calibration"
            complete$turbidity <- TRUE
            updateNumericInput(session, "turb1_std", value = sheet$turb1_std)
            updateNumericInput(session, "turb2_std", value = sheet$turb2_std)
            updateNumericInput(session, "turb1_pre", value = sheet$turb1_pre)
            updateNumericInput(session, "turb2_pre", value = sheet$turb2_pre)
            updateNumericInput(session, "turb1_post", value = sheet$turb1_post)
            updateNumericInput(session, "turb2_post", value = sheet$turb2_post)
            shinyjs::show("delete_turb")
          } else if (i == "DO"){
            output_name <- "DO calibration"
            complete$DO <- TRUE
            updateNumericInput(session, "baro_press_pre", value = sheet$baro_press_pre)
            updateNumericInput(session, "baro_press_post", value = sheet$baro_press_post)
            updateNumericInput(session, "DO_pre", value = sheet$DO_pre)
            updateNumericInput(session, "DO_post", value = sheet$DO_post)
            shinyjs::show("delete_DO")
          } else if (i == "depth"){
            output_name <- "Depth calibration"
            complete$depth <- TRUE
            updateRadioButtons(session, inputId = "depth_check_ok", selected = sheet$depth_check_ok)
            updateRadioButtons(session, inputId = "depth_changes_ok", selected = sheet$depth_changes_ok)
            shinyjs::show("delete_depth")
          }
          send_table$restarted_cal[nrow(send_table$restarted_cal)+1,1] <- output_name
        }
      }
      updateSelectInput(session, "first_selection", selected = "Calibrate") #Changing this selection brings the user right to the calibration page
      colnames(send_table$saved) <- "Saved calibrations (this session)" #Update the name for clarity since we're restarting a calibration
      output$restart_table <- renderTable({ # Display remotely saved calibrations tables
        send_table$restarted_cal
      })
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
  validation_check$orp <- FALSE
  observeEvent(input$validate_orp, { #Deal with the temp page
    tryCatch({
      orp_std <- input$orp_std
      orp_post <- input$orp_post_mV
      orp_diff <- abs(orp_std - orp_post)
      if (orp_diff > 5){
        shinyjs::js$backgroundCol("orp_std", "red")
        shinyjs::js$backgroundCol("orp_post_mV", "red")
      } else if (orp_diff > 5){
        shinyjs::js$backgroundCol("orp_std", "lemonchiffon")
        shinyjs::js$backgroundCol("orp_post_mV", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("orp_std", "white")
        shinyjs::js$backgroundCol("orp_post_mV", "white")
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      }
      validation_check$orp <- TRUE
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

  ### Save basic info
  observeEvent(input$save_basic_info, {
    #Check length of observer, sensor holder, handheld holder
    validation_check$basic <- TRUE
    if (nchar(input$observer) < 2){
      validation_check$basic <- FALSE
      shinyalert::shinyalert(title = "Fill in the calibrator name", type = "error", timer = 2000)
    }
    if (nchar(input$ID_sensor_holder) < 1){
      validation_check$basic <- FALSE
      shinyalert::shinyalert(title = "Fill in the logger/bulkhead serial #", type = "error", timer = 2000)
    }
    if (input$ID_handheld_meter == "NA"){
      shinyalert::shinyalert(title = "Warning: no handheld specified", text = "Handheld only necessary with handheld/bulkhead combos; sondes and loggers are self-contain for calibrations.", type = "warning")
    }

    if (validation_check$basic){
      shinyalert::shinyalert(title = "Saving...", type = "info")
      calibration_data$basic <- data.frame(observation_ID = calibration_data$next_id,
                                           observer = input$observer,
                                           obs_date = input$obs_date,
                                           obs_time = input$obs_time,
                                           ID_sensor_holder = input$ID_sensor_holder,
                                           ID_handheld_meter = input$ID_handheld_meter,
                                           complete = FALSE)
      if (!complete$basic){
        googlesheets4::sheet_append(calibrations_id, sheet="observations", data = calibration_data$basic)
        complete$basic <- TRUE
      } else {
        googlesheets4::range_write(calibrations_id, data = calibration_data$basic, sheet = "observations", range = paste0("A", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      }
      if ("Basic info" %in% send_table$saved[ ,1] | "Basic info" %in% send_table$restarted_cal[ ,1]){
        shinyalert::shinyalert(title = "Basic info overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "Basic info saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "Basic info"
      } else if (!("Basic info" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "Basic info"
      }
    }
  })

  ### Save/Delete pH
  observeEvent(input$save_cal_pH, {
    if (validation_check$pH){
      shinyalert::shinyalert(title = "Saving...", type = "info")
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
      if (!complete$pH){
        googlesheets4::sheet_append(calibrations_id, sheet="pH", data = calibration_data$pH)
        complete$pH <- TRUE
        shinyjs::show("delete_pH")
      } else {
        pH_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "pH")
        row <- which(pH_sheet$observation_ID == calibration_data$next_id)+1
        googlesheets4::range_write(calibrations_id, data = calibration_data$pH, sheet = "pH", range = paste0("A", row), col_names = FALSE)
      }
      if (!complete$basic){
        if (!("Basic info" %in% send_table$saved)){
          calibration_data$basic <- data.frame(observation_ID = calibration_data$next_id,
                                               observer = "NOT SET",
                                               obs_date = as.character(Sys.Date()),
                                               obs_time = Sys.time(),
                                               ID_sensor_holder = "NOT SET",
                                               ID_handheld_meter = "NOT SET",
                                               complete = FALSE)
          googlesheets4::sheet_append(calibrations_id, sheet="observations", data = calibration_data$basic)
          complete$basic <- TRUE
        }
      }
      if ("pH calibration" %in% send_table$saved[ ,1] | "pH calibration" %in% send_table$restarted_cal[ ,1]){
        shinyalert::shinyalert(title = "pH calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "pH calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "pH calibration"
      } else if (!("pH calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "pH calibration"
      }
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$delete_pH, {
    shinyalert::shinyalert("Deleting...", type = "info")
    #delete on remote sheet
    pH_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "pH")
    row <- which(pH_sheet$observation_ID == calibration_data$next_id)+1
    googlesheets4::range_clear(calibrations_id, sheet = "pH", range = paste0(row, ":", row))
    #reset the fields
    updateNumericInput(session, "pH1_std", label = "Low pH solution value", value = "4")
    updateNumericInput(session, "pH2_std", label = "Neutral pH solution value", value = "7")
    updateNumericInput(session, "pH3_std", label = "High pH solution value", value = "10")
    updateNumericInput(session, "pH1_pre_val", label = "pH 4 Pre-Cal Value", value = "")
    updateNumericInput(session, "pH1_pre_mV", label = "pH 4 Pre-Cal mV", value = "")
    updateNumericInput(session, "pH2_pre_val", label = "pH 7 Pre-Cal Value", value = "")
    updateNumericInput(session, "pH2_pre_mV", label = "pH 7 Pre-Cal mV", value = "")
    updateNumericInput(session, "pH3_pre_val", label = "pH 10 Pre-Cal Value", value = "")
    updateNumericInput(session, "pH3_pre_mV", label = "pH 10 Pre-Cal mV", value = "")
    updateNumericInput(session, "pH1_post_val", label = "pH 4 Post-Cal Value", value = "")
    updateNumericInput(session, "pH1_post_mV", label = "pH 4 Post-Cal mV", value = "")
    updateNumericInput(session, "pH2_post_val", label = "pH 7 Post-Cal Value", value = "")
    updateNumericInput(session, "pH2_post_mV", label = "pH 7 Post-Cal mV", value = "")
    updateNumericInput(session, "pH3_post_val", label = "pH 10 Post-Cal Value", value = "")
    updateNumericInput(session, "pH3_post_mV", label = "pH 10 Post-Cal mV", value = "")
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved != "pH calibration"]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$saved != "pH calibration"]
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
  })

  ### Save/Delete temperature
  observeEvent(input$save_cal_temp, {
    if (validation_check$temp){
      shinyalert::shinyalert(title = "Saving...", type = "info")
      calibration_data$temp <- data.frame(observation_ID = calibration_data$next_id,
                                          temp_reference_desc = input$temp_reference_desc,
                                          temp_reference = input$temp_reference,
                                          temp_observed = input$temp_observed)
      if (!complete$temperature){
        googlesheets4::sheet_append(calibrations_id, sheet="temperature", data = calibration_data$temp)
        shinyjs::show("delete_temp")
      } else {
        temp_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "temperature")
        row <- which(temp_sheet$observation_ID == calibration_data$next_id)+1
        googlesheets4::range_write(calibrations_id, data = calibration_data$temp, sheet = "temperature", range = paste0("A", row), col_names = FALSE)
      }
      if (!complete$basic){
        if (!("Basic info" %in% send_table$saved)){
          calibration_data$basic <- data.frame(observation_ID = calibration_data$next_id,
                                               observer = "NOT SET",
                                               obs_date = as.character(Sys.Date()),
                                               obs_time = Sys.time(),
                                               ID_sensor_holder = "NOT SET",
                                               ID_handheld_meter = "NOT SET",
                                               complete = FALSE)
          googlesheets4::sheet_append(calibrations_id, sheet="observations", data = calibration_data$basic)
          complete$basic <- TRUE
        }
      }
      if ("Temperature calibration" %in% send_table$saved[ ,1] | "Temperature calibration" %in% send_table$restarted_cal[ ,1]){
        shinyalert::shinyalert(title = "Temperature calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "Temperature calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "Temperature calibration"
      } else if (!("Temperature calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "Temperature calibration"
      }
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$delete_temp, {
    shinyalert::shinyalert("Deleting...", type = "info")
    temp_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "temperature")
    row <- which(temp_sheet$observation_ID == calibration_data$next_id)+1
    googlesheets4::range_clear(calibrations_id, sheet = "temperature", range = paste0(row, ":", row))
    #reset the fields
    updateTextInput(session, "temp_reference_desc", label = "Temp Reference Type", value = "Lab thermometer")
    updateNumericInput(session, "temp_reference", label = "Reference Temp", value = "")
    updateNumericInput(session, "temp_observed", label = "Sensor Temp", value = "")
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved != "Temperature calibration"]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$saved != "Temperature calibration"]
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
  })

  ### Save/Delete ORP
  observeEvent(input$save_cal_orp, {
    if (validation_check$orp){
      shinyalert::shinyalert(title = "Saving...", type = "info")
      calibration_data$orp <- data.frame(observation_ID = calibration_data$next_id,
                                         orp_std = input$orp_std,
                                         orp_pre_mV = input$orp_pre_mV,
                                         orp_post_mV = input$orp_post_mV)
      if (!complete$orp){
        googlesheets4::sheet_append(calibrations_id, sheet="ORP", data = calibration_data$orp)
        complete$orp <- TRUE
        shinyjs::show("delete_orp")
      } else {
        orp_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "ORP")
        row <- which(orp_sheet$observation_ID == calibration_data$next_id)+1
        googlesheets4::range_write(calibrations_id, data = calibration_data$orp, sheet = "ORP", range = paste0("A", row), col_names = FALSE)
      }
      if (!complete$basic){
        if (!("Basic info" %in% send_table$saved)){
          calibration_data$basic <- data.frame(observation_ID = calibration_data$next_id,
                                               observer = "NOT SET",
                                               obs_date = as.character(Sys.Date()),
                                               obs_time = Sys.time(),
                                               ID_sensor_holder = "NOT SET",
                                               ID_handheld_meter = "NOT SET",
                                               complete = FALSE)
          googlesheets4::sheet_append(calibrations_id, sheet="observations", data = calibration_data$basic)
          complete$basic <- TRUE
        }
      }
      if ("ORP calibration" %in% send_table$saved[ ,1] | "ORP calibration" %in% send_table$restarted_cal[ ,1]){
        shinyalert::shinyalert(title = "ORP calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "ORP calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "ORP calibration"
      } else if (!("ORP calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "ORP calibration"
      }
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$delete_orp, {
    shinyalert::shinyalert("Deleting...", type = "info")
    orp_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "orp")
    row <- which(orp_sheet$observation_ID == calibration_data$next_id)+1
    googlesheets4::range_clear(calibrations_id, sheet = "orp", range = paste0(row, ":", row))
    #reset the fields
    updateNumericInput(session, "orp_std", label = "ORP Standard solution mV", value = "")
    updateNumericInput(session, "orp_pre_mV", label = "ORP mV Pre-Cal Value", value = "")
    updateNumericInput(session, "orp_post_mV", label = "ORP mV Post-Cal Value", value = "")
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved != "ORP calibration"]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$saved != "ORP calibration"]
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
  })

  ### Save/Delete SpC
  observeEvent(input$save_cal_SpC, {
    if (validation_check$SpC){
      shinyalert::shinyalert(title = "Saving...", type = "info")
      calibration_data$SpC <- data.frame(observation_ID = calibration_data$next_id,
                                         SpC1_std = input$SpC1_std,
                                         SpC1_pre = input$SpC1_pre,
                                         SpC1_post = input$SpC1_post,
                                         SpC2_std = input$SpC2_std,
                                         SpC2_pre = input$SpC2_pre,
                                         SpC2_post = input$SpC2_post)
      if (!complete$SpC){
        googlesheets4::sheet_append(calibrations_id, sheet="SpC", data = calibration_data$SpC)
        complete$SpC <- TRUE
        shinyjs::show("delete_SpC")
      } else {
        SpC_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "SpC")
        row <- which(SpC_sheet$observation_ID == calibration_data$next_id)+1
        googlesheets4::range_write(calibrations_id, data = calibration_data$SpC, sheet = "SpC", range = paste0("A", row), col_names = FALSE)
      }
      if (!complete$basic){
        if (!("Basic info" %in% send_table$saved)){
          calibration_data$basic <- data.frame(observation_ID = calibration_data$next_id,
                                               observer = "NOT SET",
                                               obs_date = as.character(Sys.Date()),
                                               obs_time = Sys.time(),
                                               ID_sensor_holder = "NOT SET",
                                               ID_handheld_meter = "NOT SET",
                                               complete = FALSE)
          googlesheets4::sheet_append(calibrations_id, sheet="observations", data = calibration_data$basic)
          complete$basic <- TRUE
        }
      }
      if ("Conductivity calibration" %in% send_table$saved[ ,1] | "Conductivity calibration" %in% send_table$restarted_cal[ ,1]){
        shinyalert::shinyalert(title = "Conductivity calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "Conductivity calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "Conductivity calibration"
      } else if (!("Conductivity calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "Conductivity calibration"
      }
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$delete_SpC, {
    shinyalert::shinyalert("Deleting...", type = "info")
    spc_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "SpC")
    row <- which(spc_sheet$observation_ID == calibration_data$next_id)+1
    googlesheets4::range_clear(calibrations_id, sheet = "SpC", range = paste0(row, ":", row))
    #reset the fields
    updateNumericInput(session, "SpC1_std", label = "SpC Low-Range Standard", value = "0")
    updateNumericInput(session, "SpC1_pre", label = "SpC Low-Range Pre-Cal Value", value = "")
    updateNumericInput(session, "SpC1_post", label = "SpC Low-Range Post-Cal Value", value = "")
    updateNumericInput(session, "SpC2_std", label = "SpC High-Range Standard", value = "1413")
    updateNumericInput(session, "SpC2_pre", label = "SpC High-Range Pre-Cal Value", value = "")
    updateNumericInput(session, "SpC2_post", label = "SpC High-Range Post-Cal Value", value = "")
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved != "Conductivity calibration"]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$saved != "Conductivity calibration"]
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
  })

  ### Save/Delete turbidity
  observeEvent(input$save_cal_turb, {
    if (validation_check$turb){
      shinyalert::shinyalert(title = "Saving...", type = "info")
      calibration_data$turb <- data.frame(observation_ID = calibration_data$next_id,
                                          turb1_std = input$turb1_std,
                                          turb1_pre = input$turb1_pre,
                                          turb1_post = input$turb1_post,
                                          turb2_std = input$turb2_std,
                                          turb2_pre = input$turb2_pre,
                                          turb2_post = input$turb2_post)
      if (!complete$turbidity){
        googlesheets4::sheet_append(calibrations_id, sheet="turbidity", data = calibration_data$turb)
        complete$turbidity <- TRUE
        shinyjs::show("delete_turb")
      } else {
        turbidity_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "turbidity")
        row <- which(turbidity_sheet$observation_ID == calibration_data$next_id)+1
        googlesheets4::range_write(calibrations_id, data = calibration_data$turb, sheet = "turbidity", range = paste0("A", row), col_names = FALSE)
      }
      if (!complete$basic){
        if (!("Basic info" %in% send_table$saved)){
          calibration_data$basic <- data.frame(observation_ID = calibration_data$next_id,
                                               observer = "NOT SET",
                                               obs_date = as.character(Sys.Date()),
                                               obs_time = Sys.time(),
                                               ID_sensor_holder = "NOT SET",
                                               ID_handheld_meter = "NOT SET",
                                               complete = FALSE)
          googlesheets4::sheet_append(calibrations_id, sheet="observations", data = calibration_data$basic)
          complete$basic <- TRUE
        }
      }
      if ("Turbidity calibration" %in% send_table$saved[ ,1] | "Turbidity calibration" %in% send_table$restarted_cal[ ,1]){
        shinyalert::shinyalert(title = "Turbidity calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "Turbidity calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "Turbidity calibration"
      } else if (!("Turbidity calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "Turbidity calibration"
      }
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$delete_turb, {
    shinyalert::shinyalert("Deleting...", type = "info")
    turb_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "turbidity")
    row <- which(turb_sheet$observation_ID == calibration_data$next_id)+1
    googlesheets4::range_clear(calibrations_id, sheet = "turbidity", range = paste0(row, ":", row))
    #reset the fields
    updateNumericInput(session, "turb1_std", label = "Low Turb Standard Value", value = "0")
    updateNumericInput(session, "turb2_std", label = "High Turb Standard Value", value = "124")
    updateNumericInput(session, "turb1_pre", label = "Low Turb Pre-cal Value", value = "")
    updateNumericInput(session, "turb2_pre", label = "High Turb Pre-cal Value", value = "")
    updateNumericInput(session, "turb1_post", label = "Low Turb Post-cal Value", value = "")
    updateNumericInput(session, "turb2_post", label = "High Turb Post-cal Value", value = "")
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved != "Turbidity calibration"]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$saved != "Turbidity calibration"]
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
  })

  ### Save/Delete DO
  observeEvent(input$save_cal_DO, {
    if (validation_check$DO){
      shinyalert::shinyalert(title = "Saving...", type = "info")
      calibration_data$DO <- data.frame(observation_ID = calibration_data$next_id,
                                        baro_press_pre = input$baro_press_pre,
                                        baro_press_post = input$baro_press_post,
                                        DO_pre = input$DO_pre,
                                        DO_post = input$DO_post)
      if (!complete$DO){
        googlesheets4::sheet_append(calibrations_id, sheet="DO", data = calibration_data$DO)
        complete$DO <- TRUE
        shinyjs::show("delete_DO")
      } else {
        DO_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "DO")
        row <- which(DO_sheet$observation_ID == calibration_data$next_id)+1
        googlesheets4::range_write(calibrations_id, data = calibration_data$DO, sheet = "DO", range = paste0("A", row), col_names = FALSE)
      }
      if (!complete$basic){
        if (!("Basic info" %in% send_table$saved)){
          calibration_data$basic <- data.frame(observation_ID = calibration_data$next_id,
                                               observer = "NOT SET",
                                               obs_date = as.character(Sys.Date()),
                                               obs_time = Sys.time(),
                                               ID_sensor_holder = "NOT SET",
                                               ID_handheld_meter = "NOT SET",
                                               complete = FALSE)
          googlesheets4::sheet_append(calibrations_id, sheet="observations", data = calibration_data$basic)
          complete$basic <- TRUE
        }
      }
      if ("DO calibration" %in% send_table$saved[ ,1] | "DO calibration" %in% send_table$restarted_cal[ ,1]){
        shinyalert::shinyalert(title = "DO calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "DO calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "DO calibration"
      } else if (!("DO calibration" %in% send_table$saved[ ,1])){
        send_table$saved[nrow(send_table$saved)+1,1] <- "DO calibration"
      }
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$delete_DO, {
    shinyalert::shinyalert("Deleting...", type = "info")
    DO_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "DO")
    row <- which(DO_sheet$observation_ID == calibration_data$next_id)+1
    googlesheets4::range_clear(calibrations_id, sheet = "DO", range = paste0(row, ":", row))
    #reset the fields
    updateNumericInput(session, "baro_press_pre", label = "Baro Pressure Pre-Cal", value = "")
    updateNumericInput(session, "baro_press_post", label = "Baro Pressure Post-Cal", value = "")
    updateNumericInput(session, "DO_pre", label = "DO Pre-Cal mg/L", value = "")
    updateNumericInput(session, "DO_post", label = "DO Post-Cal mg/L", value = "")
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved != "DO calibration"]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$saved != "DO calibration"]
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
  })

  ### Save/Delete depth
  observeEvent(input$save_cal_depth, {
    if (validation_check$depth){
      shinyalert::shinyalert(title = "Saving...", type = "info")
      calibration_data$depth <- data.frame(observation_ID = calibration_data$next_id,
                                           depth_check_ok = input$depth_check_ok,
                                           depth_changes_ok = input$depth_changes_ok)
      if (!complete$depth){
        googlesheets4::sheet_append(calibrations_id, sheet="depth", data = calibration_data$depth)
        complete$depth <- TRUE
        shinyjs::show("delete_depth")
      } else {
        depth_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "depth")
        row <- which(depth_sheet$observation_ID == calibration_data$next_id)+1
        googlesheets4::range_write(calibrations_id, data = calibration_data$depth, sheet = "depth", range = paste0("A", row), col_names = FALSE)
      }
      if (!complete$basic){
        if (!("Basic info" %in% send_table$saved)){
          calibration_data$basic <- data.frame(observation_ID = calibration_data$next_id,
                                               observer = "NOT SET",
                                               obs_date = as.character(Sys.Date()),
                                               obs_time = Sys.time(),
                                               ID_sensor_holder = "NOT SET",
                                               ID_handheld_meter = "NOT SET",
                                               complete = FALSE)
          googlesheets4::sheet_append(calibrations_id, sheet="observations", data = calibration_data$basic)
          complete$basic <- TRUE
        }
      }
      if ("Depth calibration" %in% send_table$saved[ ,1] | "Depth calibration" %in% send_table$restarted_cal[ ,1]){
        shinyalert::shinyalert(title = "Depth calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "Depth calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet"){
        send_table$saved[1,1] <- "Depth calibration"
      } else if (!("Depth calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "Depth calibration"
      }
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  })
  observeEvent(input$delete_depth, {
    shinyalert::shinyalert("Deleting...", type = "info")
    depth_sheet <- googlesheets4::read_sheet(calibrations_id, sheet = "depth")
    row <- which(depth_sheet$observation_ID == calibration_data$next_id)+1
    googlesheets4::range_clear(calibrations_id, sheet = "depth", range = paste0(row, ":", row))
    #reset the fields
    updateRadioButtons(session, inputId = "depth_check_ok", selected = "FALSE")
    updateRadioButtons(session, inputId = "depth_changes_ok", selected = "FALSE")
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved != "Depth calibration"]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$saved != "Depth calibration"]
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
  })

  # Function to display saved calibrations tables
  output$calibration_table <- renderTable({
    send_table$saved
  })

  observeEvent({
    input$submit_btn_basic
    input$submit_btn_ph
    input$submit_btn_orp
    input$submit_btn_turb
    input$submit_btn_temp
    input$submit_btn_do
    input$submit_btn_depth
    input$submit_btn_spc
  }, {
    if (!("Basic info" %in% send_table$saved[ ,1] | "Basic info" %in% send_table$restarted_cal[ ,1])){
      shinyalert::shinyalert(title = "There is no basic information yet!!!", text = "Fill in your name, calibration time and date, and required serial numbers.", type = "error")
    } else if (!("Temperature calibration" %in% send_table$saved[ ,1] | "Temperature calibration" %in% send_table$restarted_cal[ ,1])){
      shinyalert::shinyalert(title = "Temperature calibration is mandatory", text = "If you've filled it in already you probably forgot to save it!", type = "error")
    } else {
      shinyalert::shinyalert(title = paste0("Finalizing..."), type = "info")
      # Send the calibrations and mark it as complete == TRUE after everything is sent
      cals_saved <- paste(send_table$saved[, 1], collapse = " ")
      cals_saved <- gsub(" calibration", "", cals_saved)
      cals_saved <- gsub(" ", ", ", cals_saved)
      #read in observations again as the sheet now has a new row or a row that is being edited from incomplete calibration.
      observations <- googlesheets4::read_sheet(calibrations_id, sheet = "observations")
      googlesheets4::range_write(calibrations_id, data = data.frame(complete = TRUE), sheet = "observations", range = paste0("G", which(observations$observation_ID == calibration_data$next_id)+1), col_names = FALSE)
      shinyalert::shinyalert(title = paste0("Calibration marked as complete."),
                 type = "success", immediate = TRUE)
      Sys.sleep(3)
      stopApp()
    }
  })
}
