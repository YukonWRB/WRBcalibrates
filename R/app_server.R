#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # set up for emailing
  smtp_username="wrbcalibrates@gmail.com"
  smtp_password="vxwjkeimzlsvdyol" #app password associated with this email
  smtp_server="smtp.gmail.com"
  smtp_port=587

  # httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
  smtp <- emayili::server(
    host = smtp_server,
    port = smtp_port,
    username = smtp_username,
    password = smtp_password,
    max_times = 5
  )
  #TODO: make a first page with option to restart unfinished calibration (if calibration_data$complete == FALSE)
  #
  #TODO: make a second page to enter instrument data:
  # calibration_data$basic <- data.frame(obs_datetime = input$obs_datetime,
  #                                      observer = input$observer,
  #                                      ID_sensor_holder = input$ID_sensor_holder,
  #                                      ID_meter = input$ID_meter)
  #
  #TODO: Auto-save to a .rdata file each time a new value is added. Might need to flag each unsaved calibrations as "incomplete". This would use observe(), sitting outside of observeEvent().
  #This would likely require fileInput triggered from an observeEvent, with the user picking the right file.
  # !Will likely need to communicate with Google Drive for persistent data storage. On first open, Drive will be checked for unfinished files and these displayed to the user for selection.
  #
  #TODO: make the pH standards update if the default is changed
 #https://shiny.rstudio.com/reference/shiny/0.14/updatetextinput

  validation_check <- reactiveValues()
  calibration_data <- reactiveValues()
  calibration_data$complete <- FALSE #FALSE means survey has not been sent and can be restarted.
  send_table <- reactiveValues()

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
      if(warn_mv_post){
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
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  })
  validation_check$depth <- FALSE
  observeEvent(input$validate_depth, { #Deal with depth
    tryCatch({
      depth_check <- input$depth_check_ok
      shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  })

  # Populate data.frames for calibration data
  send_table$saved <- data.frame("Saved Calibrations" = "Nothing saved yet", check.names = FALSE)

  observeEvent(input$save_cal_pH, {
    if (validation_check$pH){
      calibration_data$pH <- data.frame(pH1_std = input$pH1_std,
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
      calibration_data$temp <- data.frame(temp_reference_desc = input$temp_reference_desc,
                                          temp_reference = input$temp_reference,
                                          temp_observed = input$temp_observed)
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
      calibration_data$ORP <- data.frame(ORP_std = input$ORP_std,
                                         ORP_pre_mV = input$ORP_pre_mV,
                                         ORP_post_mV = input$ORP_post_mV)
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
      calibration_data$SpC <- data.frame(SpC1_std = input$SpC1_std,
                                         SpC1_pre = input$SpC1_pre,
                                         SpC1_post = input$SpC1_post,
                                         SpC2_std = input$SpC2_std,
                                         SpC2_pre = input$SpC2_pre,
                                         SpC2_post = input$SpC2_post)
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
      calibration_data$turb <- data.frame(turb1_std = input$turb1_std,
                                          turb1_pre = input$turb1_pre,
                                          turb1_post = input$turb1_post,
                                          turb2_std = input$turb2_std,
                                          turb2_pre_val = input$turb2_pre,
                                          turb2_post_val = input$turb2_post)
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
      calibration_data$DO <- data.frame(baro_press_pre = input$baro_press_pre,
                                        baro_press_post = input$baro_press_post,
                                        DO_pre = input$DO_pre,
                                        DO_post = input$DO_post)
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
      calibration_data$depth <- data.frame(depth_check_ok = input$depth_check_ok)
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

  # Function to display saved calibrations table
  output$calibration_table <- renderTable({
    send_table$saved
  })

  observeEvent(input$submit_btn, {
    if (send_table$saved[1,1] == "Nothing saved yet"){
      shinyalert::shinyalert(title = "There is no calibration information to send yet!!!",
                 type = "error")
    } else {
      # Submit the calibration.
      shinyalert::shinyalert(title = "Standby... sending data",type = "info")
      calibration <- reactiveValuesToList(calibration_data)
      tempfile <- tempfile(fileext = ".rdata")
      save(calibration, file = tempfile)

      message("Sending message to ", smtp_username, ".")
      emayili::envelope(
        to = smtp_username,
        from = smtp_username,
        subject = strftime(Sys.time(), "WRBcalibrates [%F %X]")
      ) %>%
        emayili::attachment(path = tempfile, name = "calibration.rdata") %>%
        emayili::text("This message was sent from WRBcalibrates Shiny app using {emayili}.") %>%
        smtp()

      cals_saved <- paste(send_table$saved[, 1], collapse = " ")
      cals_saved <- gsub(" calibration", "", cals_saved)
      cals_saved <- gsub(" ", ", ", cals_saved)
      shinyalert::shinyalert(title = paste0("Calibration data for ", cals_saved, " has been sent"),
                 type = "success", immediate = TRUE)

      #if send was successful, calibration_data$complete changes to TRUE. This way the past survey is not listed as "incomplete".
      calibration_data$complete <- TRUE
      #save calibration_data locally again
      #TODO: finish this part
    }
  })
}
