#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  table_reset <- '
var colors = ["blue", "green"];
var stack = [];
table.on("click", "tr", function() {
  var $rows = $("#calibration_instruments_table tbody tr"); // change the name of the table here
  var $row = $(this);
  var idx = $row.index();
  if($row.hasClass("selected")) {
    stack.push(idx);
    for(var i = 0; i < stack.length; i++) {
      $rows.eq(stack[i]).find("td").css(
        "box-shadow", "inset 0 0 0 9999px " + colors[i]
      );
    }
  } else {
    var i0 = stack.indexOf(idx);
    $rows.eq(stack[i0]).find("td").css(
      "box-shadow", ""
    );
    stack.splice(i0, 1);
    for(var i = 0; i < stack.length; i++) {
      $rows.eq(stack[i]).find("td").css(
        "box-shadow", "inset 0 0 0 9999px " + colors[i]
      );
    }
  }
});
'

  # Logic to save and fetch field entries from the user's browser ################################################
  # observeEvent(input$observer, {
  #   js$setCalibratorName(input$observer)
  # })
  #
  # observe({
  #   session$sendCustomMessage(type = 'getCalibratorName', message = '')
  # })



  # Initial show/hide and creation of containers ################################################
  # create a few containers
  validation_check <- reactiveValues()
  calibration_data <- reactiveValues()
  calibration_data$restarted_id <- 0
  restarted <- reactiveValues()
  restarted$initialized <- FALSE
  restarted$restarted <- FALSE
  instruments_data <- reactiveValues()
  select_data <- reactiveValues()  # Holds data to populate select menus
  sensors_data <- reactiveValues()
  sensors_data$datetime <- as.character(.POSIXct(Sys.time(), tz = "UTC")) #get the time here so that multiple maintenance events are on same line
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
  reset_check <- reactiveValues(sensors = FALSE)
  initial_instr_table <- reactiveValues(value = TRUE)

  ### Hide a bunch of buttons until they can be used
  # Delete buttons to remove a calibration sheet
  shinyjs::hide("delete_pH")
  shinyjs::hide("delete_turb")
  shinyjs::hide("delete_SpC")
  shinyjs::hide("delete_temp")
  shinyjs::hide("delete_orp")
  shinyjs::hide("delete_DO")
  shinyjs::hide("delete_depth")
  # Buttons to show sensor information
  shinyjs::hide("sensor1_show")
  shinyjs::addClass("sensor1_show", "hidden")
  shinyjs::hide("sensor2_show")
  shinyjs::addClass("sensor2_show", "hidden")
  shinyjs::hide("sensor3_show")
  shinyjs::addClass("sensor3_show", "hidden")
  shinyjs::hide("sensor4_show")
  shinyjs::addClass("sensor4_show", "hidden")
  shinyjs::hide("sensor5_show")
  shinyjs::addClass("sensor5_show", "hidden")
  shinyjs::hide("sensor6_show")
  shinyjs::addClass("sensor6_show", "hidden")
  shinyjs::hide("sensor7_show")
  shinyjs::addClass("sensor7_show", "hidden")
  shinyjs::hide("sensor8_show")
  shinyjs::addClass("sensor8_show", "hidden")
  shinyjs::hide("add_sensor")
  shinyjs::hide("new_sensor_serial")
  shinyjs::hide("add_sensor_note")
  shinyjs::hide("add_sensor_dropdown")
  shinyjs::hide("add_sensor_name")
  shinyjs::hide("load_sensors")
  shinyjs::hide("sensor1_details")
  shinyjs::hide("sensor2_details")
  shinyjs::hide("sensor3_details")
  shinyjs::hide("sensor4_details")
  shinyjs::hide("sensor5_details")
  shinyjs::hide("sensor6_details")
  shinyjs::hide("sensor7_details")
  shinyjs::hide("sensor8_details")
  shinyjs::hide("add_sensor_serial")
  shinyjs::hide("change_sensor")
  shinyjs::hide("add_comment")
  shinyjs::hide("add.change_sensor.comment_name")
  shinyjs::hide("add.change_sensor.comment")
  shinyjs::hide("restart_table")

  # Hide the pH, ORP, and turbidity post-cal fields. They're still in the UI and in the code below in case ever needed, but hidden from view. Their values reflect the standard solution values, though a user could modify the fields if they become visible
  shinyjs::hide("pH1_post_val")
  shinyjs::hide("pH2_post_val")
  shinyjs::hide("pH3_post_val")
  shinyjs::hide("orp_post_mV")
  shinyjs::hide("turb1_post")
  shinyjs::hide("turb2_post")
  shinyjs::hide("SpC1_post")
  shinyjs::hide("SpC2_post")

  # Get the data from the database, make initial tables, populate UI elements ########################################
  instruments_sheet <- DBI::dbGetQuery(pool, "SELECT i.instrument_id, i.obs_datetime, CONCAT(observers.observer_first, ' ', observers.observer_last) AS observer, i.holds_replaceable_sensors, i.serial_no, i.asset_tag, i.date_in_service, i.date_purchased, i.retired_by, i.date_retired, instrument_make.make, instrument_model.model, instrument_type.type FROM instruments AS i LEFT JOIN instrument_make ON i.make = instrument_make.make_id LEFT JOIN instrument_model ON i.model = instrument_model.model_id LEFT JOIN instrument_type ON i.type = instrument_type.type_id LEFT JOIN observers ON i.observer = observers.observer_id ORDER BY i.instrument_id")

  calibrations <- reactiveValues()
  calibrations$calibrations <- DBI::dbGetQuery(pool, "SELECT * FROM calibrations")  # This will be used to check if there are any incomplete calibrations

  instruments_data$sheet <- instruments_sheet
  instruments_data$observers <- DBI::dbGetQuery(pool, "SELECT * FROM observers")
  instruments_data$makes <- DBI::dbGetQuery(pool, "SELECT * FROM instrument_make")
  instruments_data$models <- DBI::dbGetQuery(pool, "SELECT * FROM instrument_model")
  instruments_data$types <- DBI::dbGetQuery(pool, "SELECT * FROM instrument_type")
  instruments_data$handhelds <- instruments_sheet[instruments_sheet$type == "Handheld" & is.na(instruments_sheet$date_retired) , ]
  instruments_data$others <- instruments_sheet[instruments_sheet$type != "Handheld" & is.na(instruments_sheet$date_retired) , ]
  instruments_data$maintainable <- instruments_sheet[instruments_sheet$type %in% c("Sonde", "Bulkhead") , ]

  # Create initial tables for managing instruments
  initial_manage_instruments_table <- instruments_sheet[is.na(instruments_sheet$date_retired), !colnames(instruments_sheet) %in% c("instrument_id", "observer", "obs_datetime", "holds_replaceable_sensors", "retired_by", "date_retired")]
  output$manage_instruments_table <- DT::renderDataTable(initial_manage_instruments_table, rownames = FALSE, selection = "single")
  output$calibration_instruments_table <- DT::renderDataTable({
    DT::datatable(
      initial_manage_instruments_table,
      rownames = FALSE,
      selection = "multiple",
      callback = htmlwidgets::JS(table_reset)
    )
  }, server = TRUE)

  observe({
    instruments_data$observers$observer_string <- paste0(instruments_data$observers$observer_first, " ", instruments_data$observers$observer_last, " (", instruments_data$observers$organization, ")")
    select_data$recorder <- setNames(c(instruments_data$observers$observer_id, "new"), c(instruments_data$observers$observer_string, "Add new observer"))
    select_data$makes <- setNames(c(instruments_data$makes$make_id, "new"), c(instruments_data$makes$make, "Add new make"))
    select_data$models <- setNames(c(instruments_data$models$model_id, "new"), c(instruments_data$models$model, "Add new model"))
    select_data$types <- setNames(c(instruments_data$types$type_id, "new"), c(instruments_data$types$type, "Add new type"))
    output$observer <- renderUI({
      selectInput("observer", label = "Calibrator name", choices = select_data$recorder)
    })
    output$ID_sensor_holder <- renderUI({
      div(
        selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = c("", instruments_data$others$serial_no)),
        style = "color: white; background-color: blue;"
      )
    })
    output$ID_handheld_meter <- renderUI({
      div(
        selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = c("NA", instruments_data$handhelds$serial_no)),
        style = "color: white; background-color: green;"
      )
    })
    output$add.change_sensor.comment_name <- renderUI({
      selectInput("add.change_sensor.comment_name", label = "Observer name", choices = select_data$recorder)
    })

    if (!restarted$initialized) {
      # Create initial tables for managing incomplete calibrations
      incomplete_calibrations <- calibrations$calibrations[calibrations$calibrations$complete == FALSE, ] # find out if any calibrations are labelled as incomplete
      calibrations$incomplete_calibrations <- incomplete_calibrations
      if (nrow(incomplete_calibrations) > 0) {
        shinyalert::shinyalert(title = "Incomplete calibrations found!", text = "Go to to the page 'View unfinished calibrations' to restart or delete them.")
        incomplete_calibrations <- dplyr::left_join(incomplete_calibrations, instruments_data$observers[, c("observer_id", "observer_string")], by = dplyr::join_by(observer == observer_id))
        complete$incomplete <- data.frame("Index" = seq(1, nrow(incomplete_calibrations)),
                                          "Calibrator" = as.vector(incomplete_calibrations$observer_string),
                                          "Date/time UTC" = incomplete_calibrations$obs_datetime,
                                          check.names = FALSE)
      } else {
        # Make a data.frame with no calibrations
        complete$incomplete <- data.frame("Index" = 0,
                                          "Calibrator" = "No unsaved calibrations!",
                                          "Date/Time UTC" = "No unsaved calibrations!",
                                          check.names = FALSE)
      }
      restarted$initialized <- TRUE
    }

  })




  # Modals and observers to add new observers, makes, models, types to the DB ########################################
  observeEvent(input$observer, {
    if (input$observer == "new") {
      # Add a new observer using a modal dialog
      showModal(modalDialog(title = "Add new observer",
                            textInput("new_observer_first", "First name"),
                            textInput("new_observer_last", "Last name"),
                            textInput("new_observer_org", "Organization"),
                            actionButton("add_new_observer", "Add new observer")
                            ))
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$add.change_sensor.comment_name, {
    if (input$add.change_sensor.comment_name == "new") {
      # Add a new observer using a modal dialog
      showModal(modalDialog(title = "Add new observer",
                            textInput("new_observer_first", "First name"),
                            textInput("new_observer_last", "Last name"),
                            textInput("new_observer_org", "Organization"),
                            actionButton("add_new_observer", "Add new observer")
      ))
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  observeEvent(input$add_new_observer, {
    # Ensure that a first and last name are entered
    if (input$new_observer_first == "" | input$new_observer_last == "") {
      shinyalert::shinyalert(title = "Error", text = "Please enter both a first and last name.")
      return()
    }
    DBI::dbExecute(pool, paste0("INSERT INTO observers (observer_first, observer_last, organization) VALUES ('", input$new_observer_first, "', '", input$new_observer_last, "', '", input$new_observer_org, "')"))
    instruments_data$observers <- DBI::dbGetQuery(pool, "SELECT * FROM observers")
    select_data$recorder <- setNames(c(instruments_data$observers$observer_id, "new"), c(instruments_data$observers$observer_string, "Add new observer"))
    output$observer <- renderUI({
      selectInput("observer", label = "Calibrator name", choices = select_data$recorder)
    })
    removeModal()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$make, {
    if (input$make == "new") {
      # Add a new make using a modal dialog
      showModal(modalDialog(title = "Add new make",
                            textInput("new_make", "Make"),
                            textInput("new_make_desc", "Description"),
                            actionButton("add_new_make", "Add new make")
      ))
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
 observeEvent(input$add_new_make, {
   # Ensure that a make and description are entered
    if (input$new_make == "") {
      shinyalert::shinyalert(title = "Error", text = "Please enter a make, description optional.")
      return()
    }
    DBI::dbExecute(pool, paste0("INSERT INTO instrument_make (make, description) VALUES ('", input$new_make, "', '", input$new_make_desc, "')"))
    instruments_data$makes <- DBI::dbGetQuery(pool, "SELECT * FROM instrument_make")
    select_data$makes <- setNames(c(instruments_data$makes$make_id, "new"), c(instruments_data$makes$make, "Add new make"))
    updateSelectInput(session, "make", choices = select_data$makes)
    removeModal()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

 observeEvent(input$model, {
    if (input$model == "new") {
      # Add a new model using a modal dialog
      showModal(modalDialog(title = "Add new model",
                            textInput("new_model", "Model"),
                            textInput("new_model_desc", "Description"),
                            actionButton("add_new_model", "Add new model")
      ))
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
 observeEvent(input$add_new_model, {
    # Ensure that a model and description are entered
    if (input$new_model == "") {
      shinyalert::shinyalert(title = "Error", text = "Please enter a model, description optional.")
      return()
    }
    DBI::dbExecute(pool, paste0("INSERT INTO instrument_model (model, description) VALUES ('", input$new_model, "', '", input$new_model_desc, "')"))
    instruments_data$models <- DBI::dbGetQuery(pool, "SELECT * FROM instrument_model")
    select_data$models <- setNames(c(instruments_data$models$model_id, "new"), c(instruments_data$models$model, "Add new model"))
    updateSelectInput(session, "model", choices = select_data$models)
    removeModal()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

 observeEvent(input$type, {
    if (input$type == "new") {
      # Add a new type using a modal dialog
      showModal(modalDialog(title = "Add new type",
                            textInput("new_type", "Type"),
                            textInput("new_type_desc", "Description"),
                            actionButton("add_new_type", "Add new type")
      ))
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
 observeEvent(input$add_new_type, {
    # Ensure that a type and description are entered
    if (input$new_type == "") {
      shinyalert::shinyalert(title = "Error", text = "Please enter a type, description optional.")
      return()
    }
    DBI::dbExecute(pool, paste0("INSERT INTO instrument_type (type, description) VALUES ('", input$new_type, "', '", input$new_type_desc, "')"))
    instruments_data$types <- DBI::dbGetQuery(pool, "SELECT * FROM instrument_type")
    select_data$types <- setNames(c(instruments_data$types$type_id, "new"), c(instruments_data$types$type, "Add new type"))
    updateSelectInput(session, "type", choices = select_data$types)
    removeModal()
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Show/hide post-cal fields for each calibration type ################################################
  observeEvent(input$show_post_pH, {
    if ((input$show_post_pH %% 2) == 0) {
      shinyjs::hide("pH1_post_val")
      shinyjs::hide("pH2_post_val")
      shinyjs::hide("pH3_post_val")
      updateActionButton(session, "show_post_pH", label = "Show post-cal fields")
    } else {
      shinyjs::show("pH1_post_val")
      shinyjs::show("pH2_post_val")
      shinyjs::show("pH3_post_val")
      updateActionButton(session, "show_post_pH", label = "Hide post-cal fields")
    }
  }, ignoreInit = TRUE)
  observeEvent(input$show_post_orp, {
    if ((input$show_post_orp %% 2) == 0) {
      shinyjs::hide("orp_post_mV")
      updateActionButton(session, "show_post_orp", label = "Show post-cal fields")
    } else {
      shinyjs::show("orp_post_mV")
      updateActionButton(session, "show_post_orp", label = "Hide post-cal fields")
    }
  }, ignoreInit = TRUE)
  observeEvent(input$show_post_turb, {
    if ((input$show_post_turb %% 2) == 0) {
      shinyjs::hide("turb1_post")
      shinyjs::hide("turb2_post")
      updateActionButton(session, "show_post_turb", label = "Show post-cal fields")
    } else {
      shinyjs::show("turb1_post")
      shinyjs::show("turb2_post")
      updateActionButton(session, "show_post_turb", label = "Hide post-cal fields")
    }
  }, ignoreInit = TRUE)
  observeEvent(input$show_post_SpC, {
    if ((input$show_post_SpC %% 2) == 0) {
      shinyjs::hide("SpC1_post")
      shinyjs::hide("SpC2_post")
      updateActionButton(session, "show_post_SpC", label = "Show post-cal fields")
    } else {
      shinyjs::show("SpC1_post")
      shinyjs::show("SpC2_post")
      updateActionButton(session, "show_post_SpC", label = "Hide post-cal fields")
    }
  }, ignoreInit = TRUE)

  # Render messages and notes ################################################
  messages$instrument_reminder <- "Add your instrument if not listed!"
  messages$add_sensor_note <- "Caution: adds new sensor slot. To change sensor type, click on it for new options."

  # Initiate data.frame to populate with saved calibrations later
  send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE) #Title is modified later for clarity if user want to restart a cal
  send_table$restarted_cal <- data.frame("Saved calibrations (recovered session)" = "", check.names = FALSE)
  # Initiate data.frame for instrument details when maintaining
  sensors_data$instrument_table <- data.frame("Select an instrument first" = NA, check.names = FALSE)
  sensors_data$sensor1_details <- data.frame("Nothing to show yet!" = NA, check.names = FALSE)
  output$sensor1_details <- renderTable({
    sensors_data$sensor1_details
  })

  output$instrument_reminder <- renderText({
    messages$instrument_reminder
  })
  output$sensors_reminder <- renderText({
    messages$instrument_reminder
  })
  output$add_sensor_note <- renderText({
    messages$add_sensor_note
  })
  messages$pH_mV_note <- "<b><br><br>pH mV standards:&nbsp;&nbsp;&nbsp;pH7 = +- 50;&nbsp;&nbsp;&nbsp;pH4 = pH 7 value + 165 to 180;&nbsp;&nbsp;&nbsp;pH 10 = pH 7 value - 165 to 180</b>"
  output$pH_mV_note <- renderUI({
    HTML(messages$pH_mV_note)
  })
  messages$ORP_molarity_note <- "<b><br><br>If using combination pH/ORP electrode adjust pH first.<br><br>Use proper standard scale: YSI Pro Series use 3.5M KCl scale, YSI sondes use 4M KCl scale.</b>"
  output$ORP_molarity_note <- renderUI({
    HTML(messages$ORP_molarity_note)
  })

  #Create reset functions for each calibration type ################################################
  reset_basic <- function(keep_observer = FALSE) {
    if (!keep_observer) {
      output$observer <- renderUI({
        selectInput("observer", label = "Calibrator name", choices = select_data$recorder)
      })
    }
    shinyWidgets::updateAirDateInput(session, "obs_datetime", value = as.character(.POSIXct(Sys.time(), tz = "MST")))
    output$ID_sensor_holder <- renderUI({
      div(
        selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = c("", instruments_data$others$serial_no)),
        style = "color: white; background-color: blue;"
      )
    })
    output$ID_handheld_meter <- renderUI({
      div(
        selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = c("NA", instruments_data$handhelds$serial_no)),
        style = "color: white; background-color: green;"
      )
    })
  }
  reset_ph <- function() {
    updateNumericInput(session, "pH1_std", label = "Low pH solution value", value = "4")
    updateNumericInput(session, "pH2_std", label = "Neutral pH solution value", value = "7")
    updateNumericInput(session, "pH3_std", label = "High pH solution value", value = "10")
    updateNumericInput(session, "pH1_pre_val", label = "pH 4 Pre-Cal Value", value = "")
    updateNumericInput(session, "pH1_pre_mV", label = "pH 4 mV", value = "")
    updateNumericInput(session, "pH2_pre_val", label = "pH 7 Pre-Cal Value", value = "")
    updateNumericInput(session, "pH2_pre_mV", label = "pH 7 mV", value = "")
    updateNumericInput(session, "pH3_pre_val", label = "pH 10 Pre-Cal Value", value = "")
    updateNumericInput(session, "pH3_pre_mV", label = "pH 10 mV", value = "")
    updateNumericInput(session, "pH1_post_val", label = "pH 4 Post-Cal Value", value = "")
    updateNumericInput(session, "pH1_post_mV", label = "pH 4 Post-Cal mV", value = "")
    updateNumericInput(session, "pH2_post_val", label = "pH 7 Post-Cal Value", value = "")
    updateNumericInput(session, "pH2_post_mV", label = "pH 7 Post-Cal mV", value = "")
    updateNumericInput(session, "pH3_post_val", label = "pH 10 Post-Cal Value", value = "")
    updateNumericInput(session, "pH3_post_mV", label = "pH 10 Post-Cal mV", value = "")
    shinyjs::hide("delete_pH")
  }
  reset_temp <- function() {
    updateTextInput(session, "temp_reference_desc", label = "Temp Reference Type", value = "Lab thermometer")
    updateNumericInput(session, "temp_reference", label = "Reference Temp", value = "")
    updateNumericInput(session, "temp_observed", label = "Sensor Temp", value = "")
    shinyjs::hide("delete_temp")
  }
  reset_orp <- function() {
    updateNumericInput(session, "orp_std", label = "ORP Standard solution mV", value = "")
    updateNumericInput(session, "orp_pre_mV", label = "ORP mV Pre-Cal Value", value = "")
    updateNumericInput(session, "orp_post_mV", label = "ORP mV Post-Cal Value", value = "")
    shinyjs::hide("delete_orp")
  }
  reset_spc <- function() {
    updateNumericInput(session, "SpC1_std", label = "SpC Low-Range Standard", value = "0")
    updateNumericInput(session, "SpC1_pre", label = "SpC Low-Range Pre-Cal Value", value = "")
    updateNumericInput(session, "SpC1_post", label = "SpC Low-Range Post-Cal Value", value = "")
    updateNumericInput(session, "SpC2_std", label = "SpC High-Range Standard", value = "1413")
    updateNumericInput(session, "SpC2_pre", label = "SpC High-Range Pre-Cal Value", value = "")
    updateNumericInput(session, "SpC2_post", label = "SpC High-Range Post-Cal Value", value = "")
    shinyjs::hide("delete_SpC")
  }
  reset_turb <- function() {
    updateNumericInput(session, "turb1_std", label = "Low Turb Standard Value", value = "0")
    updateNumericInput(session, "turb2_std", label = "High Turb Standard Value", value = "124")
    updateNumericInput(session, "turb1_pre", label = "Low Turb Pre-cal Value", value = "")
    updateNumericInput(session, "turb2_pre", label = "High Turb Pre-cal Value", value = "")
    updateNumericInput(session, "turb1_post", label = "Low Turb Post-cal Value", value = "")
    updateNumericInput(session, "turb2_post", label = "High Turb Post-cal Value", value = "")
    shinyjs::hide("delete_turb")
  }
  reset_do <- function() {
    updateNumericInput(session, "baro_press_pre", label = "Baro Pressure Pre-Cal", value = "")
    updateNumericInput(session, "baro_press_post", label = "Baro Pressure Post-Cal", value = "")
    updateNumericInput(session, "DO_pre_prct", label = "DO Pre-Cal %", value = "")
    updateNumericInput(session, "DO_post_prct", label = "DO Post-Cal %", value = "")
    updateNumericInput(session, "DO_pre", label = "DO Pre-Cal mg/L", value = "")
    updateNumericInput(session, "DO_post", label = "DO Post-Cal mg/L", value = "")
    shinyjs::hide("delete_DO")
  }
  reset_depth <- function() {
    updateRadioButtons(session, inputId = "depth_check_ok", selected = "FALSE")
    updateRadioButtons(session, inputId = "depth_changes_ok", selected = "FALSE")
    shinyjs::hide("delete_depth")
  }


  ### observeEvents to translate rows clicked into updated inputs, applies to several tables. ############################
  # the tricky one: the table where sensor holder and handheld are selected. Depends on table_reset js code defined at tope of server.
  click_count <- reactiveValues(value = 0)
  observeEvent(input$calibration_instruments_table_rows_selected, {
    output$ID_sensor_holder <- renderUI({
      div(
        selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = c("", instruments_data$others$serial_no)),
        style = "color: white; background-color: blue;"
      )
    })
    output$ID_handheld_meter <- renderUI({
      div(
        selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = c("NA", instruments_data$handhelds$serial_no)),
        style = "color: white; background-color: green;"
      )
    })
    if (click_count$value <= 2) {
      if (initial_instr_table$value) {
        output$ID_sensor_holder <- renderUI({
          div(
            selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = c("", instruments_data$others$serial_no), selected = initial_manage_instruments_table[input$calibration_instruments_table_rows_selected[1], "serial_no"]),
            style = "color: white; background-color: blue;"
          )
        })
        output$ID_handheld_meter <- renderUI({
          div(
            selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = c("NA", instruments_data$handhelds$serial_no), selected = initial_manage_instruments_table[input$calibration_instruments_table_rows_selected[2], "serial_no"]),
            style = "color: white; background-color: green;"
          )
        })
      } else {
        output$ID_sensor_holder <- renderUI({
          div(
            selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = c("", instruments_data$others$serial_no), selected = instruments_data$manage_instruments[input$calibration_instruments_table_rows_selected[1], "serial_no"]),
            style = "color: white; background-color: blue;"
          )
        })
        output$ID_handheld_meter <- renderUI({
          div(
            selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = c("NA", instruments_data$handhelds$serial_no), selected = instruments_data$manage_instruments[input$calibration_instruments_table_rows_selected[2], "serial_no"]),
            style = "color: white; background-color: green;"
          )
        })
      }
      click_count$value <- click_count$value + 1
    } else {
      if (initial_instr_table$value) {
        output$ID_sensor_holder <- renderUI({
          div(
            selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = c("", instruments_data$others$serial_no), selected = initial_manage_instruments_table[input$calibration_instruments_table_rows_selected[1], "serial_no"]),
            style = "color: white; background-color: blue;"
          )
        })
        output$ID_handheld_meter <- renderUI({
          div(
            selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = c("NA", instruments_data$handhelds$serial_no), selected = initial_manage_instruments_table[input$calibration_instruments_table_rows_selected[2], "serial_no"]),
            style = "color: white; background-color: green;"
          )
        })
      } else {
        output$ID_sensor_holder <- renderUI({
          div(
            selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = c("", instruments_data$others$serial_no), selected = instruments_data$manage_instruments[input$calibration_instruments_table_rows_selected[1], "serial_no"]),
            style = "color: white; background-color: blue;"
          )
        })
        output$ID_handheld_meter <- renderUI({
          div(
            selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = c("NA", instruments_data$handhelds$serial_no), selected = instruments_data$manage_instruments[input$calibration_instruments_table_rows_selected[2], "serial_no"]),
            style = "color: white; background-color: green;"
          )
        })
      }
    }
    if (length(input$calibration_instruments_table_rows_selected[input$calibration_instruments_table_rows_selected != 0]) > 2) {
      selection <- NULL
      proxy <- DT::dataTableProxy("my_table")
      DT::selectRows(proxy, selection, selected = FALSE)
      if (initial_instr_table$value) {
        output$calibration_instruments_table <- DT::renderDT({
          DT::datatable(
            initial_manage_instruments_table,
            rownames = FALSE,
            selection = "multiple",
            callback = htmlwidgets::JS(table_reset)
          )
        }, server = TRUE)
      } else {
        output$calibration_instruments_table <- DT::renderDT({
          DT::datatable(
            instruments_data$manage_instruments,
            rownames = FALSE,
            selection = "multiple",
            callback = htmlwidgets::JS(table_reset)
          )
        }, server = TRUE)
      }

    }
  })

  observeEvent(input$incomplete_table_rows_selected, {
    reset_value <- complete$incomplete[input$incomplete_table_rows_selected[1], "Index"]
    updateNumericInput(session, "restart_index", value = reset_value)
  })

  observeEvent(input$manage_instruments_table_rows_selected, {
    updateSelectInput(session, "existing_serial_no", selected = instruments_data$manage_instruments[input$manage_instruments_table_rows_selected[1], "serial_no"])
  })

  observeEvent(input$manage_sensors_table_rows_selected, {
    updateSelectInput(session, "maintain_serial", selected = instruments_data$maintainable[input$manage_sensors_table_rows_selected[1], "serial_no"])
  })


  # Update pH and ORP fields based on standard solution selected ###################################
  observeEvent(input$pH1_std, {
    updateNumericInput(session, "pH1_pre_val", label = paste0("pH ", input$pH1_std, " Pre-Cal Value"))
    updateNumericInput(session, "pH1_mV", label = paste0("pH ", input$pH1_std, " mV"))
    updateNumericInput(session, "pH1_post_val", label = paste0("pH ", input$pH1_std, " Post-Cal Value"), value = input$pH1_std)
  }, ignoreInit = TRUE)
  observeEvent(input$pH2_std, {
    updateNumericInput(session, "pH2_pre_val", label = paste0("pH ", input$pH2_std, " Pre-Cal Value"))
    updateNumericInput(session, "pH2_mV", label = paste0("pH ", input$pH2_std, " mV"))
    updateNumericInput(session, "pH2_post_val", label = paste0("pH ", input$pH2_std, " Post-Cal Value"), value = input$pH2_std)
  }, ignoreInit = TRUE)
  observeEvent(input$pH3_std, {
    updateNumericInput(session, "pH3_pre_val", label = paste0("pH ", input$pH3_std, " Pre-Cal Value"))
    updateNumericInput(session, "pH3_mV", label = paste0("pH ", input$pH3_std, " mV"))
    updateNumericInput(session, "pH3_post_val", label = paste0("pH ", input$pH3_std, " Post-Cal Value"), value = input$pH3_std)
  }, ignoreInit = TRUE)
  observeEvent(input$orp_std, {
    updateNumericInput(session, "orp_post_mV", value = input$orp_std)
  }, ignoreInit = TRUE)
  observeEvent(input$turb1_std, {
    updateNumericInput(session, "turb1_post", value = input$turb1_std)
  }, ignoreInit = TRUE)
  observeEvent(input$turb2_std, {
    updateNumericInput(session, "turb2_post", value = input$turb2_std)
  }, ignoreInit = TRUE)
  observeEvent(input$SpC1_std, {
    updateNumericInput(session, "SpC1_post", value = input$SpC1_std)
  }, ignoreInit = TRUE)
  observeEvent(input$SpC2_std, {
    updateNumericInput(session, "SpC2_post", value = input$SpC2_std)
  }, ignoreInit = TRUE)

  #observeEvents for when the user selects a particular page ########################################
  observeEvent(input$first_selection, {
    if (input$first_selection == "Manage instruments") {
      shinyjs::hide("submit_btn")
      shinyjs::hide("sensor1_show")
      shinyjs::addClass("sensor1_show", "hidden")
      shinyjs::hide("sensor2_show")
      shinyjs::addClass("sensor2_show", "hidden")
      shinyjs::hide("sensor3_show")
      shinyjs::addClass("sensor3_show", "hidden")
      shinyjs::hide("sensor4_show")
      shinyjs::addClass("sensor4_show", "hidden")
      shinyjs::hide("sensor5_show")
      shinyjs::addClass("sensor5_show", "hidden")
      shinyjs::hide("sensor6_show")
      shinyjs::addClass("sensor6_show", "hidden")
      shinyjs::hide("sensor7_show")
      shinyjs::addClass("sensor7_show", "hidden")
      shinyjs::hide("sensor8_show")
      shinyjs::addClass("sensor8_show", "hidden")
      shinyjs::hide("add_sensor")
      shinyjs::hide("add_sensor_note")
      shinyjs::hide("add_sensor_dropdown")
      shinyjs::hide("add_sensor_name")
      shinyjs::hide("sensor1_details")
      shinyjs::hide("sensor2_details")
      shinyjs::hide("sensor3_details")
      shinyjs::hide("sensor4_details")
      shinyjs::hide("sensor5_details")
      shinyjs::hide("sensor6_details")
      shinyjs::hide("sensor7_details")
      shinyjs::hide("sensor8_details")
      shinyjs::hide("change_sensor")
      shinyjs::hide("add_sensor_serial")
      shinyjs::hide("new_sensor_serial")
      shinyjs::hide("add_comment")
      shinyjs::hide("add.change_sensor.comment_name")
      shinyjs::hide("add.change_sensor.comment")
      shinyjs::hide("date_retired")
      shinyjs::hide("retired_by")
      updateSelectInput(session, "existing_serial_no", choices = c("New record", instruments_data$sheet$serial_no))
      updateSelectInput(session, "recorder", choices = select_data$recorder)
      updateSelectInput(session, "make", choices = select_data$makes)
      updateSelectInput(session, "model", choices = select_data$models)
      updateSelectInput(session, "type", choices = select_data$types)
      updateDateInput(session, "date_retired", value = NA) #Reset the retired date to NA
      updateDateInput(session, "date_in_service", value = NA)
      updateDateInput(session, "date_purchased", value = NA)
      instruments_data$manage_instruments <- instruments_data$sheet[ , !colnames(instruments_data$sheet) %in% c("instrument_id", "observer", "obs_datetime", "holds_replaceable_sensors")]
      output$manage_instruments_table <- DT::renderDataTable(instruments_data$manage_instruments, rownames = FALSE, selection = "single")
    } else if (input$first_selection == "Manage sensors and log maintenance") {
      #reload instruments_data$sheet to mitigate conflicts
      instruments_data$sheet <- DBI::dbGetQuery(pool, "SELECT i.instrument_id, i.obs_datetime, CONCAT(observers.observer_first, ' ', observers.observer_last) AS observer, i.holds_replaceable_sensors, i.serial_no, i.asset_tag, i.date_in_service, i.date_purchased, i.retired_by, i.date_retired, instrument_make.make, instrument_model.model, instrument_type.type FROM instruments AS i LEFT JOIN instrument_make ON i.make = instrument_make.make_id LEFT JOIN instrument_model ON i.model = instrument_model.model_id LEFT JOIN instrument_type ON i.type = instrument_type.type_id LEFT JOIN observers ON i.observer = observers.observer_id ORDER BY i.instrument_id")
      instruments_data$maintainable <- instruments_data$sheet[instruments_data$sheet$type %in% c("Sonde", "Bulkhead") , ]
      temp_table <- instruments_data$maintainable[, c("make", "model", "type", "serial_no")]
      output$manage_sensors_table <- DT::renderDataTable(temp_table, rownames = FALSE, selection = "single")
      updateSelectInput(session, "maintain_serial", choices = c("", instruments_data$maintainable$serial_no))
      shinyjs::hide("submit_btn")
      shinyjs::hide("add_sensor")
      shinyjs::hide("add_sensor_name")
      shinyjs::hide("add_sensor_note")
      shinyjs::hide("new_sensor_serial")
      shinyjs::hide("add.change_sensor.comment")
      shinyjs::show("load_sensors")
      shinyjs::show("manage_sensors_table")
      updateSelectInput(session, "add_sensor_name", choices = select_data$observer)
    } else if (input$first_selection == "Calibrate") {
      shinyjs::show("calibration_instruments_table")
      shinyjs::show("submit_btn")
      shinyjs::hide("load_sensors")
      shinyjs::hide("sensor1_show")
      shinyjs::addClass("sensor1_show", "hidden")
      shinyjs::hide("sensor2_show")
      shinyjs::addClass("sensor2_show", "hidden")
      shinyjs::hide("sensor3_show")
      shinyjs::addClass("sensor3_show", "hidden")
      shinyjs::hide("sensor4_show")
      shinyjs::addClass("sensor4_show", "hidden")
      shinyjs::hide("sensor5_show")
      shinyjs::addClass("sensor5_show", "hidden")
      shinyjs::hide("sensor6_show")
      shinyjs::addClass("sensor6_show", "hidden")
      shinyjs::hide("sensor7_show")
      shinyjs::addClass("sensor7_show", "hidden")
      shinyjs::hide("sensor8_show")
      shinyjs::addClass("sensor8_show", "hidden")
      shinyjs::hide("add_sensor")
      shinyjs::hide("add_sensor_note")
      shinyjs::hide("add_sensor_dropdown")
      shinyjs::hide("add_sensor_name")
      shinyjs::hide("sensor1_details")
      shinyjs::hide("sensor2_details")
      shinyjs::hide("sensor3_details")
      shinyjs::hide("sensor4_details")
      shinyjs::hide("sensor5_details")
      shinyjs::hide("sensor6_details")
      shinyjs::hide("sensor7_details")
      shinyjs::hide("sensor8_details")
      shinyjs::hide("change_sensor")
      shinyjs::hide("add_sensor_serial")
      shinyjs::hide("new_sensor_serial")
      shinyjs::hide("add_comment")
      shinyjs::hide("add.change_sensor.comment_name")
      shinyjs::hide("add.change_sensor.comment")
      output$observer <- renderUI({
        selectInput("observer", label = "Calibrator name", choices = select_data$recorder)
      })
    } else if (input$first_selection == "View unfinished calibrations") {
      output$incomplete_table <- DT::renderDataTable(complete$incomplete, rownames = FALSE, selection = "single")
      shinyjs::hide("submit_btn")
      shinyjs::hide("load_sensors")
      shinyjs::hide("sensor1_show")
      shinyjs::addClass("sensor1_show", "hidden")
      shinyjs::hide("sensor2_show")
      shinyjs::addClass("sensor2_show", "hidden")
      shinyjs::hide("sensor3_show")
      shinyjs::addClass("sensor3_show", "hidden")
      shinyjs::hide("sensor4_show")
      shinyjs::addClass("sensor4_show", "hidden")
      shinyjs::hide("sensor5_show")
      shinyjs::addClass("sensor5_show", "hidden")
      shinyjs::hide("sensor6_show")
      shinyjs::addClass("sensor6_show", "hidden")
      shinyjs::hide("sensor7_show")
      shinyjs::addClass("sensor7_show", "hidden")
      shinyjs::hide("sensor8_show")
      shinyjs::addClass("sensor8_show", "hidden")
      shinyjs::hide("add_sensor")
      shinyjs::hide("add_sensor_note")
      shinyjs::hide("add_sensor_dropdown")
      shinyjs::hide("add_sensor_name")
      shinyjs::hide("sensor1_details")
      shinyjs::hide("sensor2_details")
      shinyjs::hide("sensor3_details")
      shinyjs::hide("sensor4_details")
      shinyjs::hide("sensor5_details")
      shinyjs::hide("sensor6_details")
      shinyjs::hide("sensor7_details")
      shinyjs::hide("sensor8_details")
      shinyjs::hide("change_sensor")
      shinyjs::hide("add_sensor_serial")
      shinyjs::hide("new_sensor_serial")
      shinyjs::hide("add_comment")
      shinyjs::hide("add.change_sensor.comment_name")
      shinyjs::hide("add.change_sensor.comment")
      updateNumericInput(session, "restart_index", min = 0, max = nrow(calibrations$incomplete_calibrations))
    }
  })

  ### observeEvents related to maintenance of instruments ################################################
  observeEvent(input$maintain_serial, {
    shinyjs::show("load_sensors")
    shinyjs::hide("sensor1_show")
    shinyjs::addClass("sensor1_show", "hidden")
    shinyjs::hide("sensor2_show")
    shinyjs::addClass("sensor2_show", "hidden")
    shinyjs::hide("sensor3_show")
    shinyjs::addClass("sensor3_show", "hidden")
    shinyjs::hide("sensor4_show")
    shinyjs::addClass("sensor4_show", "hidden")
    shinyjs::hide("sensor5_show")
    shinyjs::addClass("sensor5_show", "hidden")
    shinyjs::hide("sensor6_show")
    shinyjs::addClass("sensor6_show", "hidden")
    shinyjs::hide("sensor7_show")
    shinyjs::addClass("sensor7_show", "hidden")
    shinyjs::hide("sensor8_show")
    shinyjs::addClass("sensor8_show", "hidden")
    shinyjs::hide("add_sensor")
    shinyjs::hide("add_sensor_note")
    shinyjs::hide("add_sensor_dropdown")
    shinyjs::hide("add_sensor_note")
    shinyjs::hide("add_sensor_name")
    shinyjs::hide("sensor1_details")
    shinyjs::hide("sensor2_details")
    shinyjs::hide("sensor3_details")
    shinyjs::hide("sensor4_details")
    shinyjs::hide("sensor5_details")
    shinyjs::hide("sensor6_details")
    shinyjs::hide("sensor7_details")
    shinyjs::hide("sensor8_details")
    shinyjs::hide("change_sensor")
    shinyjs::hide("add_sensor_serial")
    shinyjs::hide("new_sensor_serial")
    shinyjs::hide("add_comment")
    shinyjs::hide("add.change_sensor.comment_name")
    shinyjs::hide("add.change_sensor.comment")
  }, ignoreInit = TRUE)

  observeEvent(input$load_sensors, {
    if ((input$load_sensors %% 2) == 0) { # this part runs on second and subsequent even numbered clicks
      shinyjs::show("manage_sensors_table")
      #Hide the extra buttons
      shinyjs::hide("sensor1_show")
      shinyjs::addClass("sensor1_show", "hidden")
      shinyjs::hide("sensor2_show")
      shinyjs::addClass("sensor2_show", "hidden")
      shinyjs::hide("sensor3_show")
      shinyjs::addClass("sensor3_show", "hidden")
      shinyjs::hide("sensor4_show")
      shinyjs::addClass("sensor4_show", "hidden")
      shinyjs::hide("sensor5_show")
      shinyjs::addClass("sensor5_show", "hidden")
      shinyjs::hide("sensor6_show")
      shinyjs::addClass("sensor6_show", "hidden")
      shinyjs::hide("sensor7_show")
      shinyjs::addClass("sensor7_show", "hidden")
      shinyjs::hide("sensor8_show")
      shinyjs::addClass("sensor8_show", "hidden")
      shinyjs::hide("add_sensor")
      shinyjs::hide("add_sensor_note")
      shinyjs::hide("add_sensor_dropdown")
      shinyjs::hide("add_sensor_note")
      shinyjs::hide("add_sensor_name")
      #Hide the sensor-specific tables in case any were openned
      shinyjs::hide("sensor1_details")
      shinyjs::hide("sensor2_details")
      shinyjs::hide("sensor3_details")
      shinyjs::hide("sensor4_details")
      shinyjs::hide("sensor5_details")
      shinyjs::hide("sensor6_details")
      shinyjs::hide("sensor7_details")
      shinyjs::hide("sensor8_details")
      #Hide the extra buttons related to sensor changes/maintenance
      shinyjs::hide("change_sensor")
      shinyjs::hide("add_sensor_serial")
      shinyjs::hide("new_sensor_serial")
      shinyjs::hide("add_comment")
      shinyjs::hide("add.change_sensor.comment_name")
      shinyjs::hide("add.change_sensor.comment")
      updateActionButton(session, "load_sensors", label = "Show sensors")
    } else { # This part runs on first and subsequent odd numbered clicks
      if (input$maintain_serial != "loading choices...") {
        shinyjs::hide("manage_sensors_table")
        # Reset the button colors
        shinyjs::runjs('document.getElementById("sensor1_show").style.color = "#000000";')
        shinyjs::runjs('document.getElementById("sensor2_show").style.color = "#000000";')
        shinyjs::runjs('document.getElementById("sensor3_show").style.color = "#000000";')
        shinyjs::runjs('document.getElementById("sensor4_show").style.color = "#000000";')
        shinyjs::runjs('document.getElementById("sensor5_show").style.color = "#000000";')
        shinyjs::runjs('document.getElementById("sensor6_show").style.color = "#000000";')
        shinyjs::runjs('document.getElementById("sensor7_show").style.color = "#000000";')
        shinyjs::runjs('document.getElementById("sensor8_show").style.color = "#000000";')
        updateActionButton(session, "load_sensors", label = "Show instruments table again")

        #Find the instrument_id associated with this instrument
        sensors_data$instrument_id <-  instruments_data$maintainable[instruments_data$maintainable$serial_no == input$maintain_serial, "instrument_id"]

        #Load the sensors table
        sensors_data$sensor <- DBI::dbGetQuery(pool, paste0("SELECT * FROM array_maintenance_changes WHERE instrument_id = ", sensors_data$instrument_id))
        if (nrow(sensors_data$sensor) == 0) {
          sensors_data$sensor <- data.frame("instrument_id" = NA,
                                            "obs_datetime" = NA)
        }
        shinyjs::show("add_sensor_dropdown")
        #Find out the max number of sensors ever assigned to the instrument and what they currently are
        sensors_data$number <- length(grep("sensor[1-8]_type", colnames(sensors_data$sensor)))
        if (sensors_data$number > 0) {
          for (i in 1:sensors_data$number) { #show the right number of sensors
            shinyjs::show(paste0("sensor", i, "_show"))
            shinyjs::removeClass(paste0("sensor", i, "_show"), "hidden")
            updateActionButton(session, paste0("sensor", i, "_show"), label = HTML(paste0("Slot ", i, "<br>", sensors_data$sensor[nrow(sensors_data$sensor), paste0("sensor", i, "_type")])))
            if (i == 8) {
              shinyjs::hide("add_sensor_dropdown")
            }
          }
        }
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$add_sensor_dropdown, {
    shinyjs::show("add_sensor")
    shinyjs::show("new_sensor_serial")
    shinyjs::show("add_sensor_note")
    shinyjs::show("add_sensor_name")
  }, ignoreInit = TRUE)

  observeEvent(input$add_sensor, { # edit the data.table, making a new entry
    comment <- paste0("Added a new sensor: ", input$add_sensor_dropdown, " added")
    #find out if an entry already has the timestamp sensors_data$datetime. if not, new line.
    run_else <- TRUE
    if (sensors_data$datetime_exists) { #adding to an existing line
      if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
        col_type <- paste0("sensor", sensors_data$number + 1, "_type")
        col_comment <- paste0("sensor", sensors_data$number + 1, "_notes")
        col_serial <- paste0("sensor", sensors_data$number + 1, "_serial")
        DBI::dbExecute(pool, paste0("UPDATE sensors SET ", col_type, " = '", input$add_sensor_dropdown, "', ", col_comment, " = '", comment, "', ", col_serial, " = '", as.character(input$new_sensor_serial), "' WHERE obs_datetime = '", sensors_data$datetime, "'"))

        #Append to the internal df to not have to read-in again
        type_col <- paste0("sensor", sensors_data$number + 1, "_type")
        notes_col <- paste0("sensor", sensors_data$number + 1, "_notes")
        serial_col <- paste0("sensor", sensors_data$number + 1, "_serial")
        sensors_data$sensor$type_col <- NA #Create the new columns since new sensors
        sensors_data$sensor$notes_col <- NA
        sensors_data$sensor$serial_col <- NA
        sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, type_col] <- input$add_sensor_dropdown
        sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, notes_col] <- comment
        sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, serial_col] <- as.character(input$new_sensor_serial)
        sensors_data$sensors[sensors_data$sensors$obs_datetime == sensors_data$datetime, type_col] <- input$add_sensor_dropdown
        sensors_data$sensors[sensors_data$sensors$obs_datetime == sensors_data$datetime, notes_col] <- comment
        sensors_data$sensors[sensors_data$sensors$obs_datetime == sensors_data$datetime, serial_col] <- as.character(input$add_sensor_serial)
        run_else <- FALSE
      }
    }
    if (run_else) { #append to sensors with new row
      df <- data.frame("instrument_id" = sensors_data$instrument_id,
                       "observer" = input$add_sensor_name,
                       "obs_datetime" = sensors_data$datetime,
                       "sensor1_type" = if ("sensor1_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor1_type"] else NA,
                       "sensor1_serial" = if("sensor1_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor1_serial"] else NA,
                       "sensor2_type" = if ("sensor2_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor2_type"] else NA,
                       "sensor2_serial" = if("sensor2_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor2_serial"] else NA,
                       "sensor3_type" = if ("sensor3_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor3_type"] else NA,
                       "sensor3_serial" = if("sensor3_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor3_serial"] else NA,
                       "sensor4_type" = if ("sensor4_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor4_type"] else NA,
                       "sensor4_serial" = if("sensor4_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor4_serial"] else NA,
                       "sensor5_type" = if ("sensor5_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor5_type"] else NA,
                       "sensor5_serial" = if("sensor5_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor5_serial"] else NA,
                       "sensor6_type" = if ("sensor6_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor6_type"] else NA,
                       "sensor6_serial" = if("sensor6_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor6_serial"] else NA,
                       "sensor7_type" = if ("sensor7_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor7_type"] else NA,
                       "sensor7_serial" = if("sensor7_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor7_serial"] else NA,
                       "sensor8_type" = if ("sensor8_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor8_type"] else NA,
                       "sensor8_serial" = if("sensor8_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor8_serial"] else NA,
                       "sensor1_notes" = NA,
                       "sensor2_notes" = NA,
                       "sensor3_notes" = NA,
                       "sensor4_notes" = NA,
                       "sensor5_notes" = NA,
                       "sensor6_notes" = NA,
                       "sensor7_notes" = NA,
                       "sensor8_notes" = NA)
      df[1, paste0("sensor", sensors_data$number + 1, "_type")] <- input$add_sensor_dropdown
      df[1, paste0("sensor", sensors_data$number + 1, "_notes")] <- comment
      df[1, paste0("sensor", sensors_data$number + 1, "_serial")] <- as.character(input$new_sensor_serial)
      DBI::dbAppendTable(pool, "sensors", df)

      #Append to the internal dfs to not have to read-in again
      type_col <- paste0("sensor", sensors_data$number + 1, "_type")
      notes_col <- paste0("sensor", sensors_data$number + 1, "_notes")
      serial_col <- paste0("sensor", sensors_data$number + 1, "_serial")
      sensors_data$sensor[[type_col]] <- NA
      sensors_data$sensor[[notes_col]] <- NA
      sensors_data$sensor[[serial_col]] <- NA
      sensors_data$sensor <- merge(sensors_data$sensor, df, all = TRUE, sort = FALSE)
      sensors_data$sensors <- merge(sensors_data$sensors, df, all = TRUE, sort = FALSE)
      sensors_data$sensor <- sensors_data$sensor[!is.na(sensors_data$sensor$instrument_id) & !is.na(sensors_data$sensor$obs_datetime) , ] #remove the row with NAs from the initial df
    }
    shinyjs::show(paste0("sensor", sensors_data$number + 1, "_show")) # show the new sensor button
    shinyjs::removeClass(paste0("sensor", sensors_data$number + 1, "_show"), "hidden")
    shinyjs::hide("add_sensor_name")
    shinyjs::hide("add_sensor")
    shinyjs::hide("new_sensor_serial")
    shinyjs::hide("add_sensor_note")
    lab <- paste0("Slot ", sensors_data$number + 1, "<br>", sensors_data$sensor[nrow(sensors_data$sensor), type_col])
    updateActionButton(session, paste0("sensor", sensors_data$number + 1, "_show"), label = HTML(lab))
    sensors_data$number <- sensors_data$number + 1
    sensors_data$datetime_exists <- TRUE
  }, ignoreInit = TRUE)

  observeEvent(input$sensor1_show, {
    sensors_data$selected <- "sensor1"
    sensors_data$sensor1_details <- data.frame("Time/date" = substr(sensors_data$sensor$obs_datetime, 1, 16),
                                               "Type" = sensors_data$sensor$sensor1_type,
                                               "Serial" = sensors_data$sensor$sensor1_serial,
                                               "Notes" = sensors_data$sensor$sensor1_notes,
                                               check.names = FALSE)
    sensors_data$sensor1_details <- sensors_data$sensor1_details[!is.na(sensors_data$sensor1_details$Notes), ]
    output$sensor1_details <- DT::renderDataTable(sensors_data$sensor1_details, rownames = FALSE)
    updateSelectInput(session, "change_sensor", selected = sensors_data$sensor[nrow(sensors_data$sensor), "sensor1_type"]) #reflects the type currently on
    updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor1_serial"])
    if (sensors_data$datetime_exists) {
      if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
        if (!is.na(sensors_data$sensor[nrow(sensors_data$sensor), "sensor1_notes"])) {
          updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor1_serial"])
          updateTextAreaInput(session, "add_comment", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor1_notes"]) #reflects the note associated with this session. User may have added a sensor and auto-generated a note, or is actually wanting to edit a note.
          output$add.change_sensor.comment_name <- renderUI({
            selectInput("add.change_sensor.comment_name", label = "Observer name", choices = select_data$recorder, selected = sensors_data$sensor[nrow(sensors_data$sensor), "observer"])
          })
          updateActionButton(session, "add.change_sensor.comment", label = "Save edits")
          shinyjs::show("add.change_sensor.comment")
        }
      }
    }
    shinyjs::hide("manage_sensors_table")
    shinyjs::show("sensor1_details")
    shinyjs::hide("sensor2_details")
    shinyjs::hide("sensor3_details")
    shinyjs::hide("sensor4_details")
    shinyjs::hide("sensor5_details")
    shinyjs::hide("sensor6_details")
    shinyjs::hide("sensor7_details")
    shinyjs::hide("sensor8_details")
    shinyjs::show("change_sensor")
    shinyjs::show("add_comment")
    shinyjs::show("add.change_sensor.comment_name")
    shinyjs::show("add_sensor_serial")
    shinyjs::runjs('document.getElementById("sensor1_show").style.color = "#00BFFF";')
    shinyjs::runjs('document.getElementById("sensor2_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor3_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor4_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor5_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor6_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor7_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor8_show").style.color = "#000000";')
  }, ignoreInit = TRUE)
  observeEvent(input$sensor2_show, {
    sensors_data$selected <- "sensor2"
    sensors_data$sensor2_details <- data.frame("Time/date" = substr(sensors_data$sensor$obs_datetime, 1, 16),
                                               "Type" = sensors_data$sensor$sensor2_type,
                                               "Serial" = sensors_data$sensor$sensor2_serial,
                                               "Notes" = sensors_data$sensor$sensor2_notes,
                                               check.names = FALSE)
    sensors_data$sensor2_details <- sensors_data$sensor2_details[!is.na(sensors_data$sensor2_details$Notes), ]
    output$sensor2_details <- DT::renderDataTable(sensors_data$sensor2_details, rownames = FALSE)
    updateSelectInput(session, "change_sensor", selected = sensors_data$sensor[nrow(sensors_data$sensor), "sensor2_type"]) #reflects the type currently on
    updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor2_serial"])
    if (sensors_data$datetime_exists) {
      if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
        if (!is.na(sensors_data$sensor[nrow(sensors_data$sensor), "sensor2_notes"])) {
          updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor2_serial"])
          updateTextAreaInput(session, "add_comment", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor2_notes"]) #reflects the note associated with this session. User may have added a sensor and auto-generated a note, or is actually wanting to re-edit a note.
          output$add.change_sensor.comment_name <- renderUI({
            selectInput("add.change_sensor.comment_name", label = "Observer name", choices = select_data$recorder, selected = sensors_data$sensor[nrow(sensors_data$sensor), "observer"])
          })
          updateActionButton(session, "add.change_sensor.comment", label = "Save edits")
          shinyjs::show("add.change_sensor.comment")
        }
      }
    }
    shinyjs::hide("manage_sensors_table")
    shinyjs::hide("sensor1_details")
    shinyjs::show("sensor2_details")
    shinyjs::hide("sensor3_details")
    shinyjs::hide("sensor4_details")
    shinyjs::hide("sensor5_details")
    shinyjs::hide("sensor6_details")
    shinyjs::hide("sensor7_details")
    shinyjs::hide("sensor8_details")
    shinyjs::show("change_sensor")
    shinyjs::show("add_comment")
    shinyjs::show("add.change_sensor.comment_name")
    shinyjs::show("add_sensor_serial")
    shinyjs::runjs('document.getElementById("sensor1_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor2_show").style.color = "#00BFFF";')
    shinyjs::runjs('document.getElementById("sensor3_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor4_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor5_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor6_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor7_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor8_show").style.color = "#000000";')
  }, ignoreInit = TRUE)
  observeEvent(input$sensor3_show, {
    sensors_data$selected <- "sensor3"
    sensors_data$sensor3_details <- data.frame("Time/date" = substr(sensors_data$sensor$obs_datetime, 1, 16),
                                               "Type" = sensors_data$sensor$sensor3_type,
                                               "Serial" = sensors_data$sensor$sensor3_serial,
                                               "Notes" = sensors_data$sensor$sensor3_notes,
                                               check.names = FALSE)
    sensors_data$sensor3_details <- sensors_data$sensor3_details[!is.na(sensors_data$sensor3_details$Notes), ]
    output$sensor3_details <- DT::renderDataTable(sensors_data$sensor3_details, rownames = FALSE)
    updateSelectInput(session, "change_sensor", selected = sensors_data$sensor[nrow(sensors_data$sensor), "sensor3_type"]) #reflects the type currently on
    updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor3_serial"])
    if (sensors_data$datetime_exists) {
      if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
        if (!is.na(sensors_data$sensor[nrow(sensors_data$sensor), "sensor3_notes"])) {
          updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor3_serial"])
          updateTextAreaInput(session, "add_comment", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor3_notes"]) #reflects the note associated with this session. User may have added a sensor and auto-generated a note, or is actually wanting to re-edit a note.
          output$add.change_sensor.comment_name <- renderUI({
            selectInput("add.change_sensor.comment_name", label = "Observer name", choices = select_data$recorder, selected = sensors_data$sensor[nrow(sensors_data$sensor), "observer"])
          })
          updateActionButton(session, "add.change_sensor.comment", label = "Save edits")
          shinyjs::show("add.change_sensor.comment")
        }
      }
    }
    shinyjs::hide("manage_sensors_table")
    shinyjs::hide("sensor1_details")
    shinyjs::hide("sensor2_details")
    shinyjs::show("sensor3_details")
    shinyjs::hide("sensor4_details")
    shinyjs::hide("sensor5_details")
    shinyjs::hide("sensor6_details")
    shinyjs::hide("sensor7_details")
    shinyjs::hide("sensor8_details")
    shinyjs::show("change_sensor")
    shinyjs::show("add_comment")
    shinyjs::show("add.change_sensor.comment_name")
    shinyjs::show("add_sensor_serial")
    shinyjs::runjs('document.getElementById("sensor1_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor2_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor3_show").style.color = "#00BFFF";')
    shinyjs::runjs('document.getElementById("sensor4_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor5_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor6_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor7_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor8_show").style.color = "#000000";')
  }, ignoreInit = TRUE)
  observeEvent(input$sensor4_show, {
    sensors_data$selected <- "sensor4"
    sensors_data$sensor4_details <- data.frame("Time/date" = substr(sensors_data$sensor$obs_datetime, 1, 16),
                                               "Type" = sensors_data$sensor$sensor4_type,
                                               "Serial" = sensors_data$sensor$sensor4_serial,
                                               "Notes" = sensors_data$sensor$sensor4_notes,
                                               check.names = FALSE)
    sensors_data$sensor4_details <- sensors_data$sensor4_details[!is.na(sensors_data$sensor4_details$Notes), ]
    output$sensor4_details <- DT::renderDataTable(sensors_data$sensor4_details, rownames = FALSE)
    updateSelectInput(session, "change_sensor", selected = sensors_data$sensor[nrow(sensors_data$sensor), "sensor4_type"]) #reflects the type currently on
    updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor4_serial"])
    if (sensors_data$datetime_exists) {
      if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
        if (!is.na(sensors_data$sensor[nrow(sensors_data$sensor), "sensor4_notes"])) {
          updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor4_serial"])
          updateTextAreaInput(session, "add_comment", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor4_notes"]) #reflects the note associated with this session. User may have added a sensor and auto-generated a note, or is actually wanting to edit a note.
          output$add.change_sensor.comment_name <- renderUI({
            selectInput("add.change_sensor.comment_name", label = "Observer name", choices = select_data$recorder, selected = sensors_data$sensor[nrow(sensors_data$sensor), "observer"])
          })
          updateActionButton(session, "add.change_sensor.comment", label = "Save edits")
          shinyjs::show("add.change_sensor.comment")
        }
      }
    }
    shinyjs::hide("manage_sensors_table")
    shinyjs::hide("sensor1_details")
    shinyjs::hide("sensor2_details")
    shinyjs::hide("sensor3_details")
    shinyjs::show("sensor4_details")
    shinyjs::hide("sensor5_details")
    shinyjs::hide("sensor6_details")
    shinyjs::hide("sensor7_details")
    shinyjs::hide("sensor8_details")
    shinyjs::show("change_sensor")
    shinyjs::show("add_comment")
    shinyjs::show("add.change_sensor.comment_name")
    shinyjs::show("add_sensor_serial")
    shinyjs::runjs('document.getElementById("sensor1_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor2_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor3_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor4_show").style.color = "#00BFFF";')
    shinyjs::runjs('document.getElementById("sensor5_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor6_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor7_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor8_show").style.color = "#000000";')
  }, ignoreInit = TRUE)
  observeEvent(input$sensor5_show, {
    sensors_data$selected <- "sensor5"
    sensors_data$sensor5_details <- data.frame("Time/date" = substr(sensors_data$sensor$obs_datetime, 1, 16),
                                               "Type" = sensors_data$sensor$sensor5_type,
                                               "Serial" = sensors_data$sensor$sensor5_serial,
                                               "Notes" = sensors_data$sensor$sensor5_notes,
                                               check.names = FALSE)
    sensors_data$sensor5_details <- sensors_data$sensor5_details[!is.na(sensors_data$sensor5_details$Notes), ]
    output$sensor5_details <- DT::renderDataTable(sensors_data$sensor5_details, rownames = FALSE)
    updateSelectInput(session, "change_sensor", selected = sensors_data$sensor[nrow(sensors_data$sensor), "sensor5_type"]) #reflects the type currently on
    updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor5_serial"])
    if (sensors_data$datetime_exists) {
      if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
        if (!is.na(sensors_data$sensor[nrow(sensors_data$sensor), "sensor5_notes"])) {
          updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor5_serial"])
          updateTextAreaInput(session, "add_comment", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor5_notes"]) #reflects the note associated with this session. User may have added a sensor and auto-generated a note, or is actually wanting to edit a note.
          output$add.change_sensor.comment_name <- renderUI({
            selectInput("add.change_sensor.comment_name", label = "Observer name", choices = select_data$recorder, selected = sensors_data$sensor[nrow(sensors_data$sensor), "observer"])
          })
          updateActionButton(session, "add.change_sensor.comment", label = "Save edits")
          shinyjs::show("add.change_sensor.comment")
        }
      }
    }
    shinyjs::hide("manage_sensors_table")
    shinyjs::hide("sensor1_details")
    shinyjs::hide("sensor2_details")
    shinyjs::hide("sensor3_details")
    shinyjs::hide("sensor4_details")
    shinyjs::show("sensor5_details")
    shinyjs::hide("sensor6_details")
    shinyjs::hide("sensor7_details")
    shinyjs::hide("sensor8_details")
    shinyjs::show("change_sensor")
    shinyjs::show("add_comment")
    shinyjs::show("add.change_sensor.comment_name")
    shinyjs::show("add_sensor_serial")
    shinyjs::runjs('document.getElementById("sensor1_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor2_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor3_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor4_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor5_show").style.color = "#00BFFF";')
    shinyjs::runjs('document.getElementById("sensor6_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor7_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor8_show").style.color = "#000000";')
  }, ignoreInit = TRUE)
  observeEvent(input$sensor6_show, {
    sensors_data$selected <- "sensor6"
    sensors_data$sensor6_details <- data.frame("Time/date" = substr(sensors_data$sensor$obs_datetime, 1, 16),
                                               "Type" = sensors_data$sensor$sensor6_type,
                                               "Serial" = sensors_data$sensor$sensor6_serial,
                                               "Notes" = sensors_data$sensor$sensor6_notes,
                                               check.names = FALSE)
    sensors_data$sensor6_details <- sensors_data$sensor6_details[!is.na(sensors_data$sensor6_details$Notes), ]
    output$sensor6_details <- DT::renderDataTable(sensors_data$sensor6_details, rownames = FALSE)
    updateSelectInput(session, "change_sensor", selected = sensors_data$sensor[nrow(sensors_data$sensor), "sensor6_type"]) #reflects the type currently on
    updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor6_serial"])
    if (sensors_data$datetime_exists) {
      if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
        if (!is.na(sensors_data$sensor[nrow(sensors_data$sensor), "sensor6_notes"])) {
          updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor6_serial"])
          updateTextAreaInput(session, "add_comment", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor6_notes"]) #reflects the note associated with this session. User may have added a sensor and auto-generated a note, or is actually wanting to edit a note.
          output$add.change_sensor.comment_name <- renderUI({
            selectInput("add.change_sensor.comment_name", label = "Observer name", choices = select_data$recorder, selected = sensors_data$sensor[nrow(sensors_data$sensor), "observer"])
          })
          updateActionButton(session, "add.change_sensor.comment", label = "Save edits")
          shinyjs::show("add.change_sensor.comment")
        }
      }
    }
    shinyjs::hide("manage_sensors_table")
    shinyjs::hide("sensor1_details")
    shinyjs::hide("sensor2_details")
    shinyjs::hide("sensor3_details")
    shinyjs::hide("sensor4_details")
    shinyjs::hide("sensor5_details")
    shinyjs::show("sensor6_details")
    shinyjs::hide("sensor7_details")
    shinyjs::hide("sensor8_details")
    shinyjs::show("change_sensor")
    shinyjs::show("add_comment")
    shinyjs::show("add.change_sensor.comment_name")
    shinyjs::show("add_sensor_serial")
    shinyjs::runjs('document.getElementById("sensor1_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor2_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor3_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor4_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor5_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor6_show").style.color = "#00BFFF";')
    shinyjs::runjs('document.getElementById("sensor7_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor8_show").style.color = "#000000";')
  }, ignoreInit = TRUE)
  observeEvent(input$sensor7_show, {
    sensors_data$selected <- "sensor7"
    sensors_data$sensor7_details <- data.frame("Time/date" = substr(sensors_data$sensor$obs_datetime, 1, 16),
                                               "Type" = sensors_data$sensor$sensor7_type,
                                               "Serial" = sensors_data$sensor$sensor7_serial,
                                               "Notes" = sensors_data$sensor$sensor7_notes,
                                               check.names = FALSE)
    sensors_data$sensor7_details <- sensors_data$sensor7_details[!is.na(sensors_data$sensor7_details$Notes), ]
    output$sensor7_details <- DT::renderDataTable(sensors_data$sensor7_details, rownames = FALSE)
    updateSelectInput(session, "change_sensor", selected = sensors_data$sensor[nrow(sensors_data$sensor), "sensor7_type"]) #reflects the type currently on
    updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor7_serial"])
    if (sensors_data$datetime_exists) {
      if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
        if (!is.na(sensors_data$sensor[nrow(sensors_data$sensor), "sensor7_notes"])) {
          updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor7_serial"])
          updateTextAreaInput(session, "add_comment", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor7_notes"]) #reflects the note associated with this session. User may have added a sensor and auto-generated a note, or is actually wanting to edit a note.
          output$add.change_sensor.comment_name <- renderUI({
            selectInput("add.change_sensor.comment_name", label = "Observer name", choices = select_data$recorder, selected = sensors_data$sensor[nrow(sensors_data$sensor), "observer"])
          })
          updateActionButton(session, "add.change_sensor.comment", label = "Save edits")
          shinyjs::show("add.change_sensor.comment")
        }
      }
    }
    shinyjs::hide("manage_sensors_table")
    shinyjs::hide("sensor1_details")
    shinyjs::hide("sensor2_details")
    shinyjs::hide("sensor3_details")
    shinyjs::hide("sensor4_details")
    shinyjs::hide("sensor5_details")
    shinyjs::hide("sensor6_details")
    shinyjs::show("sensor7_details")
    shinyjs::hide("sensor8_details")
    shinyjs::show("change_sensor")
    shinyjs::show("add_comment")
    shinyjs::show("add.change_sensor.comment_name")
    shinyjs::show("add_sensor_serial")
    shinyjs::runjs('document.getElementById("sensor1_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor2_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor3_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor4_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor5_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor6_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor7_show").style.color = "#00BFFF";')
    shinyjs::runjs('document.getElementById("sensor8_show").style.color = "#000000";')
  }, ignoreInit = TRUE)
  observeEvent(input$sensor8_show, {
    sensors_data$selected <- "sensor8"
    sensors_data$sensor8_details <- data.frame("Time/date" = substr(sensors_data$sensor$obs_datetime, 1, 16),
                                               "Type" = sensors_data$sensor$sensor8_type,
                                               "Serial" = sensors_data$sensor$sensor8_serial,
                                               "Notes" = sensors_data$sensor$sensor8_notes,
                                               check.names = FALSE)
    sensors_data$sensor8_details <- sensors_data$sensor8_details[!is.na(sensors_data$sensor8_details$Notes), ]
    output$sensor8_details <- DT::renderDataTable(sensors_data$sensor8_details, rownames = FALSE)
    updateSelectInput(session, "change_sensor", selected = sensors_data$sensor[nrow(sensors_data$sensor), "sensor8_type"]) #reflects the type currently on
    updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor8_serial"])
    if (sensors_data$datetime_exists) {
      if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
        if (!is.na(sensors_data$sensor[nrow(sensors_data$sensor), "sensor8_notes"])) {
          updateTextInput(session, "add_sensor_serial", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor8_serial"])
          updateTextAreaInput(session, "add_comment", value = sensors_data$sensor[nrow(sensors_data$sensor), "sensor8_notes"]) #reflects the note associated with this session. User may have added a sensor and auto-generated a note, or is actually wanting to edit a note.
          output$add.change_sensor.comment_name <- renderUI({
            selectInput("add.change_sensor.comment_name", label = "Observer name", choices = select_data$recorder, selected = sensors_data$sensor[nrow(sensors_data$sensor), "observer"])
          })
          updateActionButton(session, "add.change_sensor.comment", label = "Save edits")
          shinyjs::show("add.change_sensor.comment")
        }
      }
    }
    shinyjs::hide("manage_sensors_table")
    shinyjs::hide("sensor1_details")
    shinyjs::hide("sensor2_details")
    shinyjs::hide("sensor3_details")
    shinyjs::hide("sensor4_details")
    shinyjs::hide("sensor5_details")
    shinyjs::hide("sensor6_details")
    shinyjs::hide("sensor7_details")
    shinyjs::show("sensor8_details")
    shinyjs::show("change_sensor")
    shinyjs::show("add_comment")
    shinyjs::show("add.change_sensor.comment_name")
    shinyjs::show("add_sensor_serial")
    shinyjs::runjs('document.getElementById("sensor1_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor2_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor3_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor4_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor5_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor6_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor7_show").style.color = "#000000";')
    shinyjs::runjs('document.getElementById("sensor8_show").style.color = "#00BFFF";')
  }, ignoreInit = TRUE)

  observeEvent(input$add.change_sensor.comment_name, {
    shinyjs::show("add.change_sensor.comment") #Now you can save
  }, ignoreInit = TRUE)

  observeEvent(input$add.change_sensor.comment, {
    if (nchar(input$add.change_sensor.comment_name) <= 1 & nchar(input$add_comment) < 5 & nchar(input$add_sensor_serial) < 2) {
      shinyalert::shinyalert("Please fill in all fields!", "You might need a few more characters if you've already written something. Come on, make us a useful note!", type =  "error", timer = 3000)
    } else { #add the data
      run_else <- TRUE
      if (sensors_data$datetime_exists) {
        if (sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, "instrument_id"] == sensors_data$instrument_id) {
          col_type <- paste0(sensors_data$selected, "_type")
          col_comment <- paste0(sensors_data$selected, "_notes")
          col_serial <- paste0(sensors_data$selected, "_serial")
          DBI::dbExecute(pool, paste0("UPDATE sensors SET ", col_type, " = '", input$change_sensor, "', ", col_comment, " = '", input$add_comment, "', ", col_serial, " = '", as.character(input$add_sensor_serial), "' WHERE obs_datetime = '", sensors_data$datetime, "'"))

          #Append to the internal df to not have to read-in again
          type_col <- paste0(sensors_data$selected, "_type")
          notes_col <- paste0(sensors_data$selected, "_notes")
          serial_col <- paste0(sensors_data$selected, "_serial")
          sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, type_col] <- input$change_sensor
          sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, notes_col] <- input$add_comment
          sensors_data$sensor[sensors_data$sensor$obs_datetime == sensors_data$datetime, serial_col] <- as.character(input$add_sensor_serial)
          sensors_data$sensors[sensors_data$sensors$obs_datetime == sensors_data$datetime, type_col] <- input$change_sensor
          sensors_data$sensors[sensors_data$sensors$obs_datetime == sensors_data$datetime, notes_col] <- input$add_comment
          sensors_data$sensors[sensors_data$sensors$obs_datetime == sensors_data$datetime, serial_col] <- as.character(input$add_sensor_serial)
          run_else <- FALSE
        }
      }
      if (run_else) { #append to sensors with a new row
        df <- data.frame("instrument_id" = sensors_data$instrument_id,
                         "observer" = input$add.change_sensor.comment_name,
                         "obs_datetime" = sensors_data$datetime,
                         "sensor1_type" = if ("sensor1_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor1_type"] else NA,
                         "sensor1_serial" = if ("sensor1_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor1_serial"] else NA,
                         "sensor2_type" = if ("sensor2_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor2_type"] else NA,
                         "sensor2_serial" = if ("sensor2_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor2_serial"] else NA,
                         "sensor3_type" = if ("sensor3_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor3_type"] else NA,
                         "sensor3_serial" = if ("sensor3_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor3_serial"] else NA,
                         "sensor4_type" = if ("sensor4_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor4_type"] else NA,
                         "sensor4_serial" = if ("sensor4_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor4_serial"] else NA,
                         "sensor5_type" = if ("sensor5_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor5_type"] else NA,
                         "sensor5_serial" = if ("sensor5_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor5_serial"] else NA,
                         "sensor6_type" = if ("sensor6_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor6_type"] else NA,
                         "sensor6_serial" = if ("sensor6_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor6_serial"] else NA,
                         "sensor7_type" = if ("sensor7_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor7_type"] else NA,
                         "sensor7_serial" = if ("sensor7_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor7_serial"] else NA,
                         "sensor8_type" = if ("sensor8_type" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor8_type"] else NA,
                         "sensor8_serial" = if ("sensor8_serial" %in% colnames(sensors_data$sensor)) sensors_data$sensor[nrow(sensors_data$sensor), "sensor8_serial"] else NA,
                         "sensor1_notes" = NA,
                         "sensor2_notes" = NA,
                         "sensor3_notes" = NA,
                         "sensor4_notes" = NA,
                         "sensor5_notes" = NA,
                         "sensor6_notes" = NA,
                         "sensor7_notes" = NA,
                         "sensor8_notes" = NA)
        df[1, paste0(sensors_data$selected, "_type")] <- input$change_sensor
        df[1, paste0(sensors_data$selected, "_notes")] <- input$add_comment
        df[1, paste0(sensors_data$selected, "_serial")] <- as.character(input$add_sensor_serial)
        DBI::dbAppendTable(pool, "sensors", df)

        #Append to the internal df to not have to read-in again
        sensors_data$sensor <- merge(sensors_data$sensor, df, all = TRUE, sort = FALSE)
        sensors_data$sensors <- merge(sensors_data$sensors, df, all = TRUE, sort = FALSE)
      }
      #render table again and update buttons
      table_name <- paste0(sensors_data$selected, "_details")
      type_col <- paste0(sensors_data$selected, "_type")
      notes_col <- paste0(sensors_data$selected, "_notes")
      serial_col <- paste0(sensors_data$selected, "_serial")
      sensors_data[[table_name]] <- data.frame("Time/date" = substr(sensors_data$sensor$obs_datetime, 1, 16),
                                               "Type" = sensors_data$sensor[[type_col]],
                                               "Serial" = as.character(sensors_data$sensor[[serial_col]]),
                                               "Notes" = sensors_data$sensor[[notes_col]],
                                               check.names = FALSE)
      sensors_data[[table_name]] <- sensors_data[[table_name]][!is.na(sensors_data[[table_name]]$Notes), ]
      output[[table_name]] <- DT::renderDataTable(sensors_data[[table_name]], rownames = FALSE)
      lab <- paste0("Slot ", gsub("\\D", "", sensors_data$selected), "<br>", sensors_data$sensor[nrow(sensors_data$sensor), type_col])
      updateActionButton(session, paste0("sensor", gsub("\\D", "", sensors_data$selected), "_show"), label = HTML(lab))
      sensors_data$datetime_exists <- TRUE
      updateActionButton(session, "add.change_sensor.comment", label = "Save edits")
    }
  }, ignoreInit = TRUE)


  # Make a new instrument record or modify an existing one ##############################################
  observeEvent(input$existing_serial_no, { #populate fields as required
    if (input$existing_serial_no != "New record") {
      modify_record <- instruments_data$sheet[instruments_data$sheet$serial_no == input$existing_serial_no ,]
      updateTextInput(session, "serial_no", value = modify_record$serial_no)
      output$observer <- renderUI({
        selectInput("observer", label = "Calibrator name", choices = select_data$recorder)
      })
      updateSelectInput(session, "make", selected = modify_record$make)
      updateTextInput(session, "model", value = modify_record$model)
      updateSelectInput(session, "type", selected = modify_record$type)
      updateTextInput(session, "asset_tag", value = modify_record$asset_tag)
      updateDateInput(session, "date_in_service", value = modify_record$date_in_service)
      updateDateInput(session, "date_purchased", value = modify_record$date_purchased)
      updateTextInput(session, "retired_by", value = modify_record$retired_by)
      updateDateInput(session, "date_retired", value = modify_record$date_retired)
      updateActionButton(session, "save_cal_instrument", "Save edits")
      shinyjs::hide("serial_no")
      shinyjs::show("date_retired")
      shinyjs::show("retired_by")
    } else if (input$existing_serial_no == "New record") {
      updateTextInput(session, "serial_no", value = "")
      updateTextInput(session, "recorder", value = "")
      updateSelectInput(session, "make", selected = "")
      updateTextInput(session, "model", value = "")
      updateSelectInput(session, "type", selected = "")
      updateTextInput(session, "asset_tag", value = "")
      updateTextInput(session, "retired_by", value = "")
      updateDateInput(session, "date_retired", value = NA)
      updateActionButton(session, "save_cal_instrument", "Save new instrument")
      shinyjs::show("serial_no")
      shinyjs::hide("date_retired")
      shinyjs::hide("retired_by")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$save_cal_instrument, { #save the new record or the changes to existing record
    #reload instruments_data$sheet to mitigate conflicts
    instruments_data$sheet <- DBI::dbGetQuery(pool, "SELECT i.instrument_id, i.obs_datetime, CONCAT(observers.observer_first, ' ', observers.observer_last) AS observer, i.holds_replaceable_sensors, i.serial_no, i.asset_tag, i.date_in_service, i.date_purchased, i.retired_by, i.date_retired, instrument_make.make, instrument_model.model, instrument_type.type FROM instruments AS i LEFT JOIN instrument_make ON i.make = instrument_make.make_id LEFT JOIN instrument_model ON i.model = instrument_model.model_id LEFT JOIN instrument_type ON i.type = instrument_type.type_id LEFT JOIN observers ON i.observer = observers.observer_id ORDER BY i.instrument_id")

    if (nrow(instruments_data$sheet) == 0) { #if there are no instruments listed yet...
      check <- FALSE #definitely does not exist yet
    } else {
      check <- input$serial_no %in% instruments_data$sheet$serial_no #check to make sure the serial no does not already exist
    }
    if (input$existing_serial_no == "New record") { #add a new row with the next instrument_id
      if (check) {
        shinyalert::shinyalert("Serial number already exists!", text = "You selected 'New record' and then entered an existing serial number.", type = "error")
        new_entry <- FALSE
      } else if (nchar(input$recorder) < 1 | nchar(input$make) < 1 | nchar(input$model) < 1 | nchar(input$type) < 1 | (nchar(input$serial_no) < 4 | (grepl("Search", input$serial_no, ignore.case = TRUE)))) { #long statement to check if all entries are satisfactorily filled in.
        shinyalert::shinyalert("Unfilled mandatory entries!", text = "You need to provide your name, instrument make, model, type, and serial number of minimum 2 characters.", type = "error")
        new_entry <- FALSE
      } else { #Make a new entry

        instrument.df <- data.frame(observer = input$recorder,
                                    obs_datetime = as.character(.POSIXct(Sys.time(), tz = "MST")),
                                    make = input$make,
                                    model = input$model,
                                    type = input$type,
                                    holds_replaceable_sensors = input$replaceableSensors,
                                    serial_no = gsub("[^[:alnum:]]", "", input$serial_no),
                                    asset_tag = gsub("[^[:alnum:]]", "", input$asset_tag),
                                    date_in_service = if (length(input$date_in_service) < 1) NA else as.character(input$date_in_service),
                                    date_purchased = if (length(input$date_purchased) < 1) NA else as.character(input$date_purchased),
                                    retired_by = input$retired_by,
                                    date_retired = if (length(input$date_retired) < 1) NA else as.character(input$date_retired))
        DBI::dbAppendTable(pool, "instruments", instrument.df)
        instrument.df$instrument_id <- DBI::dbGetQuery(pool, "SELECT MAX(instrument_id) FROM instruments")[[1]]

        shinyalert::shinyalert(paste0("Serial number ", input$serial_no, " added"), type = "success", timer = 2000)
        new_entry <- TRUE
      }
    } else { #Modify an existing entry
      if (nchar(input$recorder) < 1 | nchar(input$make) < 1 | nchar(input$model) < 1 | nchar(input$type) < 1 | (nchar(input$serial_no) < 4 | (grepl("Search", input$serial_no, ignore.case = TRUE)))) { #long statement to check if all entries are satisfactorily filled in.
        shinyalert::shinyalert("Unfilled mandatory entries!", text = "You need to provide your name, instrument make, model, type, and serial number of minimum 2 characters.", type = "error")
        new_entry <- FALSE
      } else {
        exist_id <- DBI::dbGetQuery(pool, "SELECT instrument_id FROM instruments WHERE serial_no = '", input$existing_serial_no, "'")[[1]]
        instrument.df <- data.frame(instrument_id = exist_id,
                                    observer = input$recorder,
                                    obs_datetime = as.character(.POSIXct(Sys.time(), tz = "MST")),
                                    make = input$make,
                                    model = input$model,
                                    type = input$type,
                                    holds_replaceable_sensors = input$replaceableSensors,
                                    serial_no = gsub("[^[:alnum:]]", "", input$existing_serial_no),
                                    asset_tag = gsub("[^[:alnum:]]", "", input$asset_tag),
                                    date_in_service = if (length(input$date_in_service) < 1) NA else as.character(input$date_in_service),
                                    date_purchased = if (length(input$date_purchased) < 1) NA else as.character(input$date_purchased),
                                    retired_by = input$retired_by,
                                    date_retired = if (length(input$date_retired) < 1) NA else as.character(input$date_retired))

        DBI::dbExecute(pool, paste0("UPDATE instruments SET observer = ", input$recorder,
                                    ", obs_datetime = '", as.character(.POSIXct(Sys.time(), tz = "MST")),
                                    "', make = ", make,
                                    ", model = '", model,
                                    ", type = '", type,
                                    ", holds_replaceable_sensors = ", input$replaceableSensors,
                                    ", asset_tag = '", gsub("[^[:alnum:]]", "", input$asset_tag),
                                    "', date_in_service = '", if (length(input$date_in_service) < 1) NA else as.character(input$date_in_service),
                                    "', date_purchased = '", if (length(input$date_purchased) < 1) NA else as.character(input$date_purchased),
                                    "', retired_by = '", input$retired_by,
                                    "', date_retired = '", if (length(input$date_retired) < 1) NA else as.character(input$date_retired),
                                    "' WHERE serial_no = '", input$existing_serial_no, "'"))
        shinyalert::shinyalert(paste0("Serial number ", input$serial_no, " modified"), type = "success", timer = 2000)
        new_entry <- TRUE
      }
    }
    if (new_entry) {
      #Reload the instruments sheet to integrate modifications
      #TODO: This step could be avoided by simply adding the row or modifying a row in instruments_data$sheet and not re-reading.

      instruments_sheet <- DBI::dbReadTable(pool, "instruments")
      instruments_data$sheet <- instruments_sheet #assign to a reactive

      instruments_data$handhelds <- instruments_sheet[instruments_sheet$type == "Handheld (connects to bulkheads)" & is.na(instruments_sheet$date_retired) , ]
      instruments_data$others <- instruments_sheet[instruments_sheet$type != "Handheld (connects to bulkheads)" & is.na(instruments_sheet$date_retired) , ]
      instruments_data$maintainable <- instruments_sheet[instruments_sheet$type %in% c("Sonde", "Bulkhead") , ]
      #Reset some fields, show/hide others
      updateSelectInput(session, "existing_serial_no", choices = c("New record", instruments_data$sheet$serial_no), selected = instrument.df$serial_no)
      updateTextInput(session, "serial_no", value = "")
      output$observer <- renderUI({
        selectInput("observer", label = "Calibrator name", choices = select_data$recorder)
      })
      output$ID_sensor_holder <- renderUI({
        div(
          selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = c("", instruments_data$others$serial_no)),
          style = "color: white; background-color: blue;"
        )
      })
      output$ID_handheld_meter <- renderUI({
        div(
          selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = c("NA", instruments_data$handhelds$serial_no)),
          style = "color: white; background-color: green;"
        )
      })
      shinyjs::hide("serial_no")
      shinyjs::show("date_retired")
      shinyjs::show("retired_by")
      updateActionButton(session, "save_cal_instrument", "Save edits")
      #Re-render the tables
      instruments_data$manage_instruments <- instruments_data$sheet[ , !colnames(instruments_data$sheet) %in% c("instrument_id", "observer", "obs_datetime", "holds_replaceable_sensors")]
      output$manage_instruments_table <- DT::renderDataTable(instruments_data$manage_instruments, rownames = FALSE, selection = "single")
      temp_table <- instruments_data$maintainable[, c("make", "model", "type", "serial_no")]
      temp_table$type <- gsub(" .*", "", temp_table$type)
      output$manage_sensors_table <- DT::renderDataTable(temp_table, rownames = FALSE, selection = "single")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$restart_calibration, {
    restart_value <- as.numeric(input$restart_index)
    if (restart_value == 0) {
      shinyalert::shinyalert("0 is not a valid selection!", type = "error")
    } else if (!(restart_value %in% complete$incomplete$Index)) {
      shinyalert::shinyalert("The number you entered does not correspond to a row/index number", type = "error")
    } else {
      shinyjs::show("restart_table")
      send_table$restarted_cal <- data.frame("Saved calibrations (recovered session)" = "Basic info", check.names = FALSE) #Set/reset here for if the user selects a different calibration in the same session
      complete$basic <- TRUE
      incomplete_ID <- as.numeric(calibrations$incomplete_calibrations[restart_value , "calibration_id"])
      calibration_data$next_id <- incomplete_ID
      calibration_data$restarted_id <- incomplete_ID

      # Search for entries in parameter-specific sheets with the same calibration_id and update the fields
      all_sheets <- DBI::dbListTables(pool)
      calibration_sheets <- all_sheets[grepl("^calibrate_", all_sheets)]
      for (i in calibration_sheets) {
        sheet <- DBI::dbGetQuery(pool, paste0("SELECT * FROM ", i, " WHERE calibration_id = ", incomplete_ID))
        if (nrow(sheet) == 1) {
          if (i == "calibrate_temperature") {
            output_name <- "Temperature calibration"
            complete$temperature <- TRUE
            updateTextInput(session, "temp_reference_desc", value = sheet$temp_reference_desc)
            updateNumericInput(session, "temp_reference", value = sheet$temp_reference)
            updateNumericInput(session, "temp_observed", value = sheet$temp_reference)
            shinyjs::show("delete_temp")
          } else if (i == "calibrate_specific_conductance") {
            output_name <- "Conductivity calibration"
            complete$SpC <- TRUE
            updateNumericInput(session, "SpC1_std", value = sheet$SpC1_std)
            updateNumericInput(session, "SpC1_pre", value = sheet$SpC1_pre)
            updateNumericInput(session, "SpC1_post", value = sheet$SpC1_post)
            updateNumericInput(session, "SpC2_std", value = sheet$SpC2_std)
            updateNumericInput(session, "SpC2_pre", value = sheet$SpC2_pre)
            updateNumericInput(session, "SpC2_post", value = sheet$SpC2_post)
            shinyjs::show("delete_SpC")
          } else if (i == "calibrate_ph") {
            output_name <- "pH calibration"
            complete$pH <- TRUE
            updateNumericInput(session, "pH1_std", value = sheet$pH1_std)
            updateNumericInput(session, "pH2_std", value = sheet$pH2_std)
            updateNumericInput(session, "pH3_std", value = sheet$pH3_std)
            updateNumericInput(session, "pH1_pre_val", label = paste0("pH ", sheet$pH1_std, " Pre-Cal Value"), value =  sheet$pH1_pre_val)
            updateNumericInput(session, "pH1_mV", label = paste0("pH ", sheet$pH1_std, " mV"), value = sheet$pH1_mV)
            updateNumericInput(session, "pH2_pre_val", label = paste0("pH ", sheet$pH2_std, " Pre-Cal Value"), value = sheet$pH2_pre_val)
            updateNumericInput(session, "pH2_mV", label = paste0("pH ", sheet$pH2_std, " mV"), value = sheet$pH2_mV)
            updateNumericInput(session, "pH3_pre_val", label = paste0("pH ", sheet$pH3_std, " Pre-Cal Value"), value = sheet$pH3_pre_val)
            updateNumericInput(session, "pH3_mV", label = paste0("pH ", sheet$pH3_std, " mV"), value = sheet$pH3_mV)
            updateNumericInput(session, "pH1_post_val", label = paste0("pH ", sheet$pH1_std, " Post-Cal Value"), value = sheet$pH1_post_val)
            updateNumericInput(session, "pH2_post_val", label = paste0("pH ", sheet$pH2_std, " Post-Cal Value"), value = sheet$pH2_post_val)
            updateNumericInput(session, "pH3_post_val", label = paste0("pH ", sheet$pH3_std, " Post-Cal Value"), value = sheet$pH3_post_val)
            shinyjs::show("delete_pH")
          } else if (i == "calibrate_orp") {
            output_name <- "ORP calibration"
            complete$orp <- TRUE
            updateNumericInput(session, "orp_std", value = sheet$orp_std)
            updateNumericInput(session, "orp_pre_mV", value = sheet$orp_pre_mV)
            updateNumericInput(session, "orp_post_mV", value = sheet$orp_post_mV)
            shinyjs::show("delete_orp")
          } else if (i == "calibrate_turbidity") {
            output_name <- "Turbidity calibration"
            complete$turbidity <- TRUE
            updateNumericInput(session, "turb1_std", value = sheet$turb1_std)
            updateNumericInput(session, "turb2_std", value = sheet$turb2_std)
            updateNumericInput(session, "turb1_pre", value = sheet$turb1_pre)
            updateNumericInput(session, "turb2_pre", value = sheet$turb2_pre)
            updateNumericInput(session, "turb1_post", value = sheet$turb1_post)
            updateNumericInput(session, "turb2_post", value = sheet$turb2_post)
            shinyjs::show("delete_turb")
          } else if (i == "calibrate_dissolved_oxygen") {
            output_name <- "DO calibration"
            complete$DO <- TRUE
            updateNumericInput(session, "baro_press_pre", value = sheet$baro_press_pre)
            updateNumericInput(session, "baro_press_post", value = sheet$baro_press_post)
            updateNumericInput(session, "DO_pre", value = sheet$DO_pre_mgl)
            updateNumericInput(session, "DO_post", value = sheet$DO_post_mgl)
            shinyjs::show("delete_DO")
          } else if (i == "calibrate_depth") {
            output_name <- "Depth calibration"
            complete$depth <- TRUE
            updateRadioButtons(session, inputId = "depth_check_ok", selected = sheet$depth_check_ok)
            updateRadioButtons(session, inputId = "depth_changes_ok", selected = sheet$depth_changes_ok)
            shinyjs::show("delete_depth")
          }
          send_table$restarted_cal[nrow(send_table$restarted_cal) + 1,1] <- output_name
        }
      }
      # Reset the basic fields according to recovered info
      output$observer <- renderUI({
        selectInput("observer", label = "Calibrator name", choices = select_data$recorder, selected = select_data$recorder[calibrations$incomplete_calibrations[restart_value, "observer"]])
      })
      shinyWidgets::updateAirDateInput(session, "obs_datetime", value = calibrations$incomplete_calibrations[restart_value, "obs_datetime"])
      output$ID_sensor_holder <- renderUI({
        div(
          selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = c("", instruments_data$others$serial_no),
                         selected =
                           if (!is.na(calibrations$incomplete_calibrations[calibrations$incomplete_calibrations$calibration_id == incomplete_ID , "id_sensor_holder"]))
                             instruments_data$others[instruments_data$others$instrument_id == calibrations$incomplete_calibrations[calibrations$incomplete_calibrations$calibration_id == incomplete_ID, "id_sensor_holder"], "serial_no"]
                         else "NA"),
          style = "color: white; background-color: blue;"
        )
      })
      output$ID_handheld_meter <- renderUI({
        div(
          selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = c("NA", instruments_data$handhelds$serial_no),
                         selected =
                           if (!is.na(calibrations$incomplete_calibrations[calibrations$incomplete_calibrations$calibration_id == incomplete_ID , "id_handheld_meter"]))
                             instruments_data$others[instruments_data$others$instrument_id == calibrations$incomplete_calibrations[calibrations$incomplete_calibrations$calibration_id == incomplete_ID, "id_handheld_meter"], "serial_no"]
                         else "NA"),
          style = "color: white; background-color: green;"
        )
      })
      colnames(send_table$saved) <- "Saved calibrations (this session)" #Update the name for clarity since we're restarting a calibration
      output$saved <- renderTable({ # Display local calibrations tables with new name
        send_table$saved
      })
      output$restart_table <- renderTable({ # Display remotely saved calibrations tables
        send_table$restarted_cal
      })
      restarted$restarted <- TRUE
      updateCheckboxInput(session, "spc_or_not", value = FALSE) #Reset to FALSE since values are stored as SpC; this makes it clear to the user.
      updateSelectInput(session, "first_selection", selected = "Calibrate") #Changing this selection brings the user right to the calibration page
    }
  }, ignoreInit = TRUE)

  observeEvent(input$delete_calibration, {
    delete_value <- as.numeric(input$restart_index)
    if (delete_value == 0) {
      shinyalert::shinyalert("0 is not a valid selection!", type = "error")
    } else if (!(delete_value %in% complete$incomplete$Index)) {
      shinyalert::shinyalert("The number you entered does not correspond to a row/index number", type = "error")
    } else {
      delete_ID <- as.numeric(calibrations$incomplete_calibrations[delete_value , "calibration_id"])
      shinyalert::shinyalert("Deleting old calibration", type = "info")
      all_sheets <- DBI::dbListTables(pool)
      calibration_sheets <- all_sheets[grepl("^calibrate_", all_sheets)]

      DBI::dbExecute(pool, paste0("DELETE FROM calibrations WHERE calibration_id = ", delete_ID))  # Cascades to all other sheets where the id is referenced

      complete$incomplete <- complete$incomplete[!complete$incomplete$Index == delete_value ,]
      if (nrow(complete$incomplete) == 0) {
        complete$incomplete <- data.frame("Index" = 0,
                                          "Calibrator" = "No unsaved calibrations!",
                                          "Date/time UTC" = "No unsaved calibrations!",
                                          check.names = FALSE)
      }
      output$incomplete_table <- DT::renderDataTable(complete$incomplete, rownames = FALSE, selection = "single")
      calibrations$incomplete_calibrations <- calibrations$incomplete_calibrations[!calibrations$incomplete_calibrations$calibration_id == delete_ID ,]
      #reset internal markers of completeness
      complete$basic <- FALSE
      complete$temperature <- FALSE
      complete$SpC <- FALSE
      complete$pH <- FALSE
      complete$orp <- FALSE
      complete$turbidity <- FALSE
      complete$DO <- FALSE
      complete$depth <- FALSE
      # reset fields previously loaded if loaded calibration is the one being deleted
      if (delete_ID == calibration_data$restarted_id) {
        reset_basic()
        reset_ph()
        reset_temp()
        reset_orp()
        reset_spc()
        reset_turb()
        reset_do()
        reset_depth()
      }
      updateNumericInput(session, "restart_index", value = 0)
      shinyalert::shinyalert("Deleted", type = "success", immediate = TRUE, timer = 2000)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$spc_or_not, {
    if (input$spc_or_not) {
      updateNumericInput(session, "SpC1_pre", label = "Conducvitity Low-Range Pre-Cal Value")
      updateNumericInput(session, "SpC1_post", label = "Conducvitity Low-Range Post-Cal Value")
      updateNumericInput(session, "SpC2_pre", label = "Conducvitity High-Range Pre-Cal Value")
      updateNumericInput(session, "SpC2_post", label = "Conducvitity High-Range Post-Cal Value")
    } else {
      updateNumericInput(session, "SpC1_pre", label = "SpC Low-Range Pre-Cal Value")
      updateNumericInput(session, "SpC1_post", label = "SpC Low-Range Post-Cal Value")
      updateNumericInput(session, "SpC2_pre", label = "SpC High-Range Pre-Cal Value")
      updateNumericInput(session, "SpC2_post", label = "SpC High-Range Post-Cal Value")
    }
  }, ignoreInit = TRUE)

  observe( #Updates the SPC or non-spc values based on reference temperature input$spc_or_not changing
    if (input$spc_or_not) {
      post_condy_val <-input$SpC2_std/(1+0.02*(calibration_data$temp$temp_reference - 25))
      updateNumericInput(session, "SpC2_post", value = round(post_condy_val, 0))
    } else {
      updateNumericInput(session, "SpC2_post", value = input$SpC2_std)
    }
  )

  #Function to simplify DO calculated fields later on
  DO_calc <- function(pre_post, prct_abs, messages = TRUE) {
    trigger_name <- if (pre_post == "pre" & prct_abs == "prct") "DO_pre_prct" else if (pre_post == "pre" & prct_abs == "abs") "DO_pre" else if (pre_post == "post" & prct_abs == "prct") "DO_post_prct" else if (pre_post == "post" & prct_abs == "abs") "DO_post"
    update_name <- if (pre_post == "pre" & prct_abs == "prct") "DO_pre" else if (pre_post == "pre" & prct_abs == "abs") "DO_pre_prct" else if (pre_post == "post" & prct_abs == "prct") "DO_post" else if (pre_post == "post" & prct_abs == "abs") "DO_post_prct"
    baro_press <- if(pre_post == "pre") input$baro_press_pre else input$baro_press_post
    meas <- input[[trigger_name]]
    temp <- input$temp_observed
    go_baro <- FALSE
    go_temp <- FALSE

    if (!is.na(baro_press)) {
      if (baro_press < 600 | baro_press > 850) {
        if (pre_post == "pre") {
          shinyalert::shinyalert("Pre-calibration baro pressure out of range", "Enter pressure in mmHg only", type = "error", timer = 3000)
        } else {
          shinyalert::shinyalert("Post-calibration baro pressure out of range", "Enter pressure in mmHg only", type = "error", timer = 3000)
        }
      } else {
        go_baro <- TRUE
      }
    } else {
      if (pre_post == "pre") {
        shinyalert::shinyalert("Enter pre-cal baro pressures in mmHg first!", type = "error", timer = 3000)
      } else {
        shinyalert::shinyalert("Enter post-cal baro pressures in mmHg first!", type = "error", timer = 3000)
      }
    }
    if (!is.na(temp)) {
      if (temp < 0 | temp > 30) {
        if (messages) {
          shinyalert::shinyalert("Temperature out of range", "Temp should be between 0 and 30 degrees C; review temperature calibration", type = "error", timer = 3000)
        }
      } else {
        go_temp <- TRUE
      }
    } else {
      if (messages) {
        shinyalert::shinyalert("Enter temperature data first!", "Go to the temperature calibration", type = "error", timer = 3000)
      }
    }
    if (!is.na(meas) & meas > 0 & go_baro & go_temp) {
      res <- suppressWarnings(respR::convert_DO(meas, from = if (grepl("prct", trigger_name)) "%Air" else "mg/l", to = if (grepl("prct", trigger_name)) "mg/l" else "%Air", S = 0, t = temp, P = baro_press / 750.06156130264))
      updateNumericInput(session, update_name, value = round(res, 2))
      if (messages) {
        shinyalert::shinyalert("You MUST recalculate if updating baro pressures or temperature", "Cannot auto-update without knowing which value (mg/l or %) to update", timer = 4000)
      }
      updateActionButton(session, "calc_abs_DO", "Recalc mg/l values")
      updateActionButton(session, "calc_prct_DO", "Recalc % values")
    }
  }

  observeEvent(input$calc_abs_DO, {
    DO_calc(pre_post = "pre", prct_abs = "prct")
    DO_calc(pre_post = "post", prct_abs = "prct", messages = FALSE)
  }, ignoreInit = T)
  observeEvent(input$calc_prct_DO, {
    DO_calc(pre_post = "pre", prct_abs = "abs")
    DO_calc(pre_post = "post", prct_abs = "abs", messages = FALSE)
  }, ignoreInit = T)


  observeEvent(input$ID_sensor_holder, {
    if (nrow(instruments_data$others[instruments_data$others$serial_no == input$ID_sensor_holder, ]) > 0) {
      if (instruments_data$others[instruments_data$others$serial_no == input$ID_sensor_holder, "make"] == "Solinst") {
        updateCheckboxInput(session, "spc_or_not", value = TRUE)
      } else {
        updateCheckboxInput(session, "spc_or_not", value = FALSE)
      }
    }
  }, ignoreInit = TRUE)

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
      if (std1 != 4) {
        shinyjs::js$backgroundCol("pH1_std", "lemonchiffon")
        warn_ph_std <- TRUE
      } else {
        shinyjs::js$backgroundCol("pH1_std", "white")
        warn_ph_std <- FALSE
      }
      if (std2 != 7) {
        shinyjs::js$backgroundCol("pH2_std", "lemonchiffon")
        warn_ph_std <- TRUE
      } else {
        shinyjs::js$backgroundCol("pH2_std", "white")
        warn_ph_std <- FALSE
      }
      if (std3 != 10) {
        shinyjs::js$backgroundCol("pH3_std", "lemonchiffon")
        warn_ph_std <- TRUE
      } else {
        shinyjs::js$backgroundCol("pH3_std", "white")
        warn_ph_std <- FALSE
      }
      #Validate the pH measurements vs the standards
      value1 <- as.numeric(input$pH1_post_val)
      if (value1 < (std1 - 0.1) | value1 > (std1 + 0.1) | is.null(value1)) { #tolerance of 0.1 pH units from the stated calibration standard value
        shinyjs::js$backgroundCol("pH1_post_val", "red")
        warn_ph_post <- TRUE
      } else {
        shinyjs::js$backgroundCol("pH1_post_val", "white")
      }
      value2 <- as.numeric(input$pH2_post_val)
      if (value2 < (std2 - 0.1) | value2 > (std2 + 0.1) | is.null(value2)) { #tolerance of 0.1 pH units from the stated calibration standard value
        shinyjs::js$backgroundCol("pH2_post_val", "red")
        warn_ph_post <- TRUE
      } else {
        shinyjs::js$backgroundCol("pH2_post_val", "white")
      }
      value3 <- as.numeric(input$pH3_post_val)
      if (value3 < (std3 - 0.1) | value3 > (std3 + 0.1) | is.null(value3)) { #tolerance of 0.1 pH units from the stated calibration standard value
        shinyjs::js$backgroundCol("pH3_post_val", "red")
        warn_ph_post <- TRUE
      } else {
        shinyjs::js$backgroundCol("pH3_post_val", "white")
      }
      # Validate the mV readings
      pH1_mV <- as.numeric(input$pH1_mV)
      pH2_mV <- as.numeric(input$pH2_mV)
      pH3_mV <- as.numeric(input$pH3_mV)
      if ((pH1_mV < (165 + pH2_mV)) | (pH1_mV > (180 + pH2_mV)) & (std1 > 3.9 & std1 < 4.1)) {
        shinyjs::js$backgroundCol("pH1__mV", "red")
        warn_mv_post <- TRUE
      } else if (std1 != 4) {
        shinyjs::js$backgroundCol("pH1_mV", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("pH1_mV", "white")
      }
      if ((pH2_mV > 50 | pH2_mV < -50) & (std2 > 6.9 & std2 < 7.1)) {
        shinyjs::js$backgroundCol("pH2_mV", "red")
        warn_mv_post <- TRUE
      } else if (std2 != 7) {
        shinyjs::js$backgroundCol("pH2_mV", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("pH2_mV", "white")
      }
      if ((pH3_mV > (pH2_mV - 165)) | (pH3_mV < (pH2_mV - 180)) & (std3 > 9.9 & std3 < 10.1)) {
        shinyjs::js$backgroundCol("pH3_mV", "red")
        warn_mv_post <- TRUE
      } else if (std3 != 10) {
        shinyjs::js$backgroundCol("pH3_mV", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("pH3_mV", "white")
      }
      #TODO: chain these shinyalerts together
      if (warn_ph_std) {
        shinyalert::shinyalert(title = "Are you sure your standards are correct? If yes, checks on mV outputs will be invalid; use your judgement.", type = "warning")
      }
      if (warn_ph_post) {
        shinyalert::shinyalert(title = "Some of your post calibration pH values are > 0.1 units from their standards! Check your inputs.", type = "warning")
      }
      if (warn_mv_post) {
        shinyalert::shinyalert(title = "Some of your post calibration mV values are outside of the valid range!", text = "Re-check your measurements, and if the problem persists consider replacing the electrode (step 1) or entire sensor (last resort). ", type = "warning")
      }
      validation_check$pH <- TRUE
      if (!warn_ph_std & !warn_ph_post & !warn_mv_post) {
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      }
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", text = "If doing a 2-point calibration enter 0 for the third solution values to pass this check.", type = "error")
    })
  }, ignoreInit = TRUE)

  validation_check$orp <- FALSE
  observeEvent(input$validate_orp, { #Deal with the temp page
    tryCatch({
      orp_std <- input$orp_std
      orp_post <- input$orp_post_mV
      orp_diff <- abs(orp_std - orp_post)
      if (orp_diff > 5) {
        shinyjs::js$backgroundCol("orp_std", "red")
        shinyjs::js$backgroundCol("orp_post_mV", "red")
      } else if (orp_diff > 5) {
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
  }, ignoreInit = TRUE)

  validation_check$temp <- FALSE
  observeEvent(input$validate_temp, { #Deal with the temp page
    tryCatch({
      temp_re_type <- input$temp_reference_desc
      temp_ref <- input$temp_reference
      temp_meas <- input$temp_observed
      temp_diff <- abs(temp_ref - temp_meas)
      if (temp_diff > 0.2) {
        shinyjs::js$backgroundCol("temp_observed", "red")
        shinyjs::js$backgroundCol("temp_reference", "red")
        shinyalert::shinyalert(title = "Warning: double check your temperature, consider replacing this sensor!", type = "warning", timer = 2000)
      } else if (temp_diff > 0.1) {
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
  }, ignoreInit = TRUE)

  validation_check$SpC <- FALSE
  observeEvent(input$validate_SpC, { #Deal with SpC
    if (input$spc_or_not & is.null(calibration_data$temp)) {
      shinyalert::shinyalert("Calibrate temperature first!", "You must calibrate temperature first if entering non-specific conductivity")
    } else {
      tryCatch({
        SpC1_ref <- input$SpC1_std
        SpC2_ref <- input$SpC2_std
        SpC1_post <- if (input$spc_or_not) input$SpC1_post/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC1_post
        SpC2_post <- if (input$spc_or_not) input$SpC2_post/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC2_post
        SpC1_diff <- abs(SpC1_ref - SpC1_post)
        SpC2_diff <- abs(SpC2_ref - SpC2_post)
        gtg1 <- FALSE
        gtg2 <- FALSE
        if (SpC1_diff > 10) {
          shinyjs::js$backgroundCol("SpC1_std", "red")
          shinyjs::js$backgroundCol("SpC1_post", "red")
          if (input$spc_or_not) {
            shinyalert::shinyalert("Warning: double check your values", paste0("Your low-range input converts to an SpC of ", round(SpC1_post, 0), " versus the expected ", SpC1_ref))
          } else {
            shinyalert::shinyalert(title = "Warning: double check your values", type = "warning", timer = 2000)
          }
        } else if (SpC1_diff > 5) {
          shinyjs::js$backgroundCol("SpC1_std", "lemonchiffon")
          shinyjs::js$backgroundCol("SpC1_post", "lemonchiffon")
          if (input$spc_or_not) {
            shinyalert::shinyalert("Warning: double check your values", paste0("Your low_range input converts to an SpC of ", round(SpC1_post, 0), " versus the expected ", SpC1_ref))
          } else {
            shinyalert::shinyalert(title = "Warning: double check your values", type = "warning", timer = 2000)
          }
        } else {
          gtg1 <- TRUE
          shinyjs::js$backgroundCol("SpC1_std", "white")
          shinyjs::js$backgroundCol("SpC1_post", "white")
        }
        if (SpC2_diff > 10) {
          shinyjs::js$backgroundCol("SpC2_std", "red")
          shinyjs::js$backgroundCol("SpC2_post", "red")
          if (input$spc_or_not) {
            shinyalert::shinyalert("Warning: double check your values", paste0("Your high-range input converts to an SpC of ", round(SpC2_post, 0), " versus the expected ", SpC2_ref))
          } else {
            shinyalert::shinyalert(title = "Warning: double check your values", type = "warning", timer = 2000)
          }
        } else if (SpC2_diff > 5) {
          shinyjs::js$backgroundCol("SpC2_std", "lemonchiffon")
          shinyjs::js$backgroundCol("SpC2_post", "lemonchiffon")
          shinyalert::shinyalert(title = "Warning: double check your values", type = "warning", timer = 2000)
          if (input$spc_or_not) {
            shinyalert::shinyalert("Warning: double check your values", paste0("Your high-range input converts to an SpC of ", round(SpC2_post, 0), " versus the expected ", SpC2_ref))
          } else {
            shinyalert::shinyalert(title = "Warning: double check your values", type = "warning", timer = 2000)
          }
        } else {
          gtg2 <- TRUE
          shinyjs::js$backgroundCol("SpC2_std", "white")
          shinyjs::js$backgroundCol("SpC2_post", "white")
        }
        if (gtg1 & gtg2) {
          shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
        }
        validation_check$SpC <- TRUE
      }, error = function(e) {
        shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
      })
    }
  }, ignoreInit = TRUE)

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
      if (turb1_diff > 10) {
        shinyjs::js$backgroundCol("turb1_std", "red")
        shinyjs::js$backgroundCol("turb1_post", "red")
      } else if (turb1_diff > 5) {
        shinyjs::js$backgroundCol("turb1_std", "lemonchiffon")
        shinyjs::js$backgroundCol("turb1_post", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("turb1_std", "white")
        shinyjs::js$backgroundCol("turb1_post", "white")
        gtg1 <- TRUE
      }
      if (turb2_diff > 10) {
        shinyjs::js$backgroundCol("turb2_std", "red")
        shinyjs::js$backgroundCol("turb2_post", "red")
      } else if (turb2_diff > 5) {
        shinyjs::js$backgroundCol("turb2_std", "lemonchiffon")
        shinyjs::js$backgroundCol("turb2_post", "lemonchiffon")
      } else {
        shinyjs::js$backgroundCol("turb2_std", "white")
        shinyjs::js$backgroundCol("turb2_post", "white")
        gtg2 <- TRUE
      }
      if (gtg1 & gtg2) {
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      } else {
        shinyalert::shinyalert(title = "Your post-cal values are a bit off from the standard. Please check you entries before moving on.", type = "warning", timer = 2000)
      }
      validation_check$turb <- TRUE
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  }, ignoreInit = TRUE)

  validation_check$DO <- FALSE
  observeEvent(input$validate_DO, { #Deal with DO
    NFG <- FALSE
    tryCatch({
      baro_post <- input$baro_press_post
      DO_post <- input$DO_post
      if (baro_post < 600 | baro_post > 800) {
        shinyalert::shinyalert(title = "Baro pressures are not in range", "You must enter baro pressure in mmHg", type = "error", timer = 2000)
        validation_check$DO <- FALSE
        NFG <- TRUE
      }
      if (DO_post < 1 | DO_post > 15) {
        shinyalert::shinyalert(title = "DO post-calibration values are out of range", "Are you sure you entered % and mg/l in the right boxes? Only mg/l is saved, use the Fill/recalculate button.", type = "error", timer = 5000)
        validation_check$DO <- FALSE
        NFG <- TRUE
      }
      if (!NFG) {
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
        validation_check$DO <- TRUE
      }
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", "Baro pressure and DO in mg/l are mandatory", type = "error", timer = 4000)
    })
  }, ignoreInit = TRUE)

  validation_check$depth <- FALSE
  observeEvent(input$validate_depth, { #Deal with depth
    tryCatch({
      depth_check <- input$depth_check_ok
      depth_change <- input$depth_changes_ok
      if (depth_check == "TRUE" & depth_change == "TRUE") {
        shinyalert::shinyalert(title = "Good to go!", type = "success", timer = 2000)
      } else {
        shinyalert::shinyalert(title = "You indicated FALSE or Not Checked for one of the values. Are you sure about that? Should you be using a different sensor?", type = "warning")
      }
      validation_check$depth <- TRUE
    }, error = function(e) {
      shinyalert::shinyalert(title = "You have unfilled mandatory entries", type = "error", timer = 2000)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$selection, {
    if (input$selection == "Basic calibration info") {
      instruments_data$manage_instruments <- instruments_data$sheet[ , !colnames(instruments_data$sheet) %in% c("instrument_ID", "observer", "obs_datetime", "holds_replaceable_sensors", "retired_by", "date_retired")]
      output$calibration_instruments_table <- DT::renderDT({
        DT::datatable(
          instruments_data$manage_instruments,
          rownames = FALSE,
          selection = "multiple",
          callback = htmlwidgets::JS(table_reset)
        )
      }, server = TRUE)
      shinyjs::show("calibration_instruments_table")
    } else {
      shinyjs::hide("calibration_instruments_table")
    }
    if (input$selection == "pH calibration" & input$first_selection == "Calibrate") {
      shinyjs::show("pH_mV_note")
    } else {
      shinyjs::hide("pH_mV_note")
    }
    if (input$selection == "ORP calibration" & input$first_selection == "Calibrate") {
      shinyjs::show("ORP_molarity_note")
    } else {
      shinyjs::hide("ORP_molarity_note")
    }
  })

  # Calibration data saving #########################################################

  # Chastize the user if they try to move on without saving basic info
  observeEvent(input$selection, {
    if (input$selection != "Basic calibration info" & !complete$basic) {
      shinyalert::shinyalert(title = "Stop! You must save basic info first or load an incomplete calibration!", type = "error", timer = 2000)
      updateSelectInput(session, "selection", selected = "Basic calibration info")
    }
  })

  ### Save basic info
  observeEvent(input$save_basic_info, {
    if (nchar(input$ID_sensor_holder) < 1) {
      shinyalert::shinyalert(title = "Fill in the logger/bulkhead serial #", type = "error", timer = 2000)
      return()
    }
    if (input$ID_handheld_meter == "NA" & instruments_data$others[instruments_data$others$serial_no == input$ID_sensor_holder, "type"] == "Bulkhead") {
      shinyalert::shinyalert(title = "Error: no handheld specified", text = "Handheld unit is integral to bulkhead calibrations and must be entered.", type = "error")
      return()
    }
    if (input$ID_handheld_meter != "NA" & !(instruments_data$others[instruments_data$others$serial_no == input$ID_sensor_holder, "type"] %in% c("Bulkhead", "Sonde"))) {
      shinyalert::shinyalert(title = "Error: You specified a handheld meter but your sensor holder is not a bulkhead or sonde.", text = "Only bulkheads and sondes should have associated handheld units.", type = "error")
      return()
    }
    if (nchar(input$observer) < 1) {
      shinyalert::shinyalert(title = "Fill in the observer name", type = "error", timer = 2000)
      return()
    }


      dt <- input$obs_datetime
      id_sensor_holder <- instruments_data$sheet[instruments_data$sheet$serial_no == input$ID_sensor_holder, "instrument_id"]
      id_handheld_meter <- if (input$ID_handheld_meter == "NA") NA else instruments_data$sheet[instruments_data$sheet$serial_no == input$ID_handheld_meter, "instrument_id"]

      calibration_data$basic <- data.frame(observer = input$observer,
                                           obs_datetime = dt,
                                           id_sensor_holder = id_sensor_holder,
                                           id_handheld_meter = id_handheld_meter,
                                           complete = FALSE)
      if (!complete$basic) {
        DBI::dbAppendTable(pool, "calibrations", calibration_data$basic)
        calibration_data$next_id <- DBI::dbGetQuery(pool, "SELECT MAX(calibration_id) FROM calibrations")[[1]]
        complete$basic <- TRUE
      } else {
        if (is.na(id_handheld_meter)) {
          DBI::dbExecute(pool, paste0("UPDATE calibrations SET observer = '", input$observer, "', obs_datetime = '", dt, "', id_sensor_holder = ", id_sensor_holder, " WHERE calibration_id = ", calibration_data$next_id, ";"))
        } else {
          DBI::dbExecute(pool, paste0("UPDATE calibrations SET observer = '", input$observer, "', obs_datetime = '", dt, "', id_sensor_holder = ", id_sensor_holder,  ", id_handheld_meter = ", id_handheld_meter, " WHERE calibration_id = ", calibration_data$next_id, ";"))
        }
      }
      if ("Basic info" %in% send_table$saved[ ,1] | "Basic info" %in% send_table$restarted_cal[ ,1]) {
        shinyalert::shinyalert(title = "Basic info overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "Basic info saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet") {
        send_table$saved[1,1] <- "Basic info"
      } else if (!("Basic info" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved) + 1,1] <- "Basic info"
      }
      output$saved <- renderTable({ # Display local calibrations table
        send_table$saved
      })
  }, ignoreInit = TRUE)

  ### Save/Delete pH
  observeEvent(input$save_cal_pH, {
    if (validation_check$pH) {

      calibration_data$pH <- data.frame(calibration_id = calibration_data$next_id,
                                        ph1_std = input$pH1_std,
                                        ph2_std = input$pH2_std,
                                        ph3_std = input$pH3_std,
                                        ph1_pre_val = input$pH1_pre_val,
                                        ph2_pre_val = input$pH2_pre_val,
                                        ph3_pre_val = input$pH3_pre_val,
                                        ph1_mv = input$pH1_mV,
                                        ph2_mv = input$pH2_mV,
                                        ph3_mv = input$pH3_mV,
                                        ph1_post_val = input$pH1_post_val,
                                        ph2_post_val = input$pH2_post_val,
                                        ph3_post_val = input$pH3_post_val)
      if (!complete$pH) {
        DBI::dbAppendTable(pool, "calibrate_ph", calibration_data$pH)

        complete$pH <- TRUE
        shinyjs::show("delete_pH")
      } else {
        DBI::dbExecute(pool, paste0("UPDATE calibrate_ph SET ph1_std = ", input$pH1_std,
                                    ", ph2_std = ", input$pH2_std,
                                    ", ph3_std = ", input$pH3_std,
                                    ", ph1_pre_val = ", input$pH1_pre_val,
                                    ", ph2_pre_val = ", input$pH2_pre_val,
                                    ", ph3_pre_val = ", input$pH3_pre_val,
                                    ", ph1_mv = ", input$pH1_mV,
                                    ", ph2_mv = ", input$pH2_mV,
                                    ", ph3_mv = ", input$pH3_mV,
                                    ", ph1_post_val = ", input$pH1_post_val,
                                    ", ph2_post_val = ", input$pH2_post_val,
                                    ", ph3_post_val = ", input$pH3_post_val,
                                    " WHERE calibration_id = ", calibration_data$next_id))
      }
      if ("pH calibration" %in% send_table$saved[ ,1] | "pH calibration" %in% send_table$restarted_cal[ ,1]) {
        shinyalert::shinyalert(title = "pH calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "pH calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet") {
        send_table$saved[1,1] <- "pH calibration"
      } else if (!("pH calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved) + 1,1] <- "pH calibration"
      }
      output$saved <- renderTable({ # Display local calibrations table
        send_table$saved
      })
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$delete_pH, {
    shinyalert::shinyalert("Deleting...", type = "info")
    #delete on remote sheet
    DBI::dbExecute(pool, paste0("DELETE FROM calibrate_ph WHERE calibration_id = ", calibration_data$next_id))
    #reset the fields
    reset_ph()
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved[1] != "pH calibration" , , drop = FALSE]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$restarted_cal[1] != "pH calibration" , , drop = FALSE]
    if (nrow(send_table$saved) == 0) {
      if (!restarted$restarted) {
        send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE)
      } else {
        send_table$saved <- data.frame("Saved calibrations (this session)" = "Nothing saved yet", check.names = FALSE)
      }
    }
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
    output$saved <- renderTable({ # Display local calibrations table
      send_table$saved
    })
    output$restart_table <- renderTable({ # Display remotely saved calibrations tables
      send_table$restarted_cal
    })
    complete$pH <- FALSE
  }, ignoreInit = TRUE)

  ### Save/Delete temperature
  observeEvent(input$save_cal_temp, {
    if (validation_check$temp) {

      calibration_data$temp <- data.frame(calibration_id = calibration_data$next_id,
                                          temp_reference_desc = input$temp_reference_desc,
                                          temp_reference = input$temp_reference,
                                          temp_observed = input$temp_observed)
      if (!complete$temperature) {
        DBI::dbAppendTable(pool, "calibrate_temperature", calibration_data$temp)
        complete$temperature <- TRUE
        shinyjs::show("delete_temp")
      } else {
        DBI::dbExecute(pool, paste0("UPDATE calibrate_temperature SET temp_reference_desc = '", input$temp_reference_desc,
                                    "', temp_reference = ", input$temp_reference,
                                    ", temp_observed = ", input$temp_observed,
                                    " WHERE calibration_id = ", calibration_data$next_id))
      }
      if ("Temperature calibration" %in% send_table$saved[ ,1] | "Temperature calibration" %in% send_table$restarted_cal[ ,1]) {
        shinyalert::shinyalert(title = "Temperature calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "Temperature calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet") {
        send_table$saved[1,1] <- "Temperature calibration"
      } else if (!("Temperature calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved) + 1,1] <- "Temperature calibration"
      }
      output$saved <- renderTable({ # Display local calibrations table
        send_table$saved
      })
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$delete_temp, {
    shinyalert::shinyalert("Deleting...", type = "info")
    DBI::dbExecute(pool, paste0("DELETE FROM calibrate_temperature WHERE calibration_id = ", calibration_data$next_id))
    #reset the fields
    reset_temp()
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved[1] != "Temperature calibration" , , drop = FALSE] #drop = FALSE is necessary to return a table and not a vector, which is default if returning only one col
    send_table$restarted_cal <- send_table$restarted_cal[send_table$restarted_cal[1] != "Temperature calibration" , , drop=FALSE]
    if (nrow(send_table$saved) == 0) {
      if (!restarted$restarted) {
        send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE)
      } else {
        send_table$saved <- data.frame("Saved calibrations (this session)" = "Nothing saved yet", check.names = FALSE)
      }
    }
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
    output$saved <- renderTable({ # Display local calibrations table
      send_table$saved
    })
    output$restart_table <- renderTable({ # Display remotely saved calibrations tables
      send_table$restarted_cal
    })
    complete$temperature <- FALSE
  }, ignoreInit = TRUE)


  ### Save/Delete ORP
  observeEvent(input$save_cal_orp, {
    if (validation_check$orp) {

      calibration_data$orp <- data.frame(calibration_id = calibration_data$next_id,
                                         orp_std = input$orp_std,
                                         orp_pre_mv = input$orp_pre_mV,
                                         orp_post_mv = input$orp_post_mV)
      if (!complete$orp) {
        DBI::dbAppendTable(pool, "calibrate_orp", calibration_data$orp)
        complete$orp <- TRUE
        shinyjs::show("delete_orp")
      } else {
        DBI::dbExecute(pool, paste0("UPDATE calibrate_orp SET orp_std = ", input$orp_std,
                                    " orp_pre_mv = ", input$orp_pre_mV,
                                    " orp_post_mv = ", input$orp_post_mV,
                                    " WHERE calibration_id = ", calibration_data$next_id))
      }
      if ("ORP calibration" %in% send_table$saved[ ,1] | "ORP calibration" %in% send_table$restarted_cal[ ,1]) {
        shinyalert::shinyalert(title = "ORP calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "ORP calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet") {
        send_table$saved[1,1] <- "ORP calibration"
      } else if (!("ORP calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved)+1,1] <- "ORP calibration"
      }
      output$saved <- renderTable({ # Display local calibrations table
        send_table$saved
      })
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$delete_orp, {
    shinyalert::shinyalert("Deleting...", type = "info")
    DBI::dbExecute(pool, paste0("DELETE FROM calibrate_orp WHERE calibration_id = ", calibration_data$next_id))
    #reset the fields
    reset_orp()
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved[1] != "ORP calibration" , , drop = FALSE]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$restarted_cal[1] != "ORP calibration" , , drop = FALSE]
    if (nrow(send_table$saved) == 0) {
      if (!restarted$restarted) {
        send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE)
      } else {
        send_table$saved <- data.frame("Saved calibrations (this session)" = "Nothing saved yet", check.names = FALSE)
      }
    }
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
    output$saved <- renderTable({ # Display local calibrations table
      send_table$saved
    })
    output$restart_table <- renderTable({ # Display remotely saved calibrations tables
      send_table$restarted_cal
    })
    complete$orp <- FALSE
  }, ignoreInit = TRUE)


  ### Save/Delete SpC
  observeEvent(input$save_cal_SpC, {
    if (input$spc_or_not & is.null(calibration_data$temp)) {
      shinyalert::shinyalert("Calibrate temperature first!", "You must calibrate temperature first if entering non-specific conductivity")
      updateSelectInput(session, "selection", selected = "Temperature calibration")
      return()
    } else {
      if (validation_check$SpC) {

        calibration_data$SpC <- data.frame(calibration_id = calibration_data$next_id,
                                           spc1_std = input$SpC1_std,
                                           spc1_pre = if (input$spc_or_not) input$SpC1_pre/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC1_pre,
                                           spc1_post = if (input$spc_or_not) input$SpC1_post/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC1_post,
                                           spc2_std = input$SpC2_std,
                                           spc2_pre = if (input$spc_or_not) input$SpC2_pre/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC2_pre,
                                           spc2_post = if (input$spc_or_not) input$SpC2_post/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC2_post)
        if (!complete$SpC) {
          DBI::dbAppendTable(pool, "calibrate_specific_conductance", calibration_data$SpC)
          complete$SpC <- TRUE
          shinyjs::show("delete_SpC")
        } else {
          DBI::dbExecute(pool, paste0("UPDATE calibrate_specific_conductance SET spc1_std = ", input$SpC1_std,
                                      " spc1_pre = ", if (input$spc_or_not) input$SpC1_pre/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC1_pre,
                                      " spc1_post = ", if (input$spc_or_not) input$SpC1_post/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC1_post,
                                      " spc2_std = ", input$SpC2_std,
                                      " spc2_pre = ", if (input$spc_or_not) input$SpC2_pre/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC2_pre,
                                      " spc2_post = ", if (input$spc_or_not) input$SpC2_post/(1 + 0.02*(calibration_data$temp$temp_reference - 25)) else input$SpC2_post,
                                      " WHERE calibration_id = ", calibration_data$next_id))
        }
        if ("Conductivity calibration" %in% send_table$saved[ ,1] | "Conductivity calibration" %in% send_table$restarted_cal[ ,1]) {
          shinyalert::shinyalert(title = "Conductivity calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
        } else {
          shinyalert::shinyalert(title = "Conductivity calibration saved", type = "success", timer = 2000, immediate = TRUE)
        }
        if (send_table$saved[1,1] == "Nothing saved yet") {
          send_table$saved[1,1] <- "Conductivity calibration"
        } else if (!("Conductivity calibration" %in% send_table$saved[ ,1])) {
          send_table$saved[nrow(send_table$saved) + 1, 1] <- "Conductivity calibration"
        }
        output$saved <- renderTable({ # Display local calibrations table
          send_table$saved
        })
      } else {
        shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
      }
    }
  }, ignoreInit = TRUE)
  observeEvent(input$delete_SpC, {
    shinyalert::shinyalert("Deleting...", type = "info")
    DBI::dbExecute(pool, paste0("DELETE FROM calibrate_specific_conductance WHERE calibration_id = ", calibration_data$next_id))
    #reset the fields
    reset_spc()
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved[1] != "Conductivity calibration", , drop = FALSE]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$restarted_cal[1] != "Conductivity calibration", , drop = FALSE]
    if (nrow(send_table$saved) == 0) {
      if (!restarted$restarted) {
        send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE)
      } else {
        send_table$saved <- data.frame("Saved calibrations (this session)" = "Nothing saved yet", check.names = FALSE)
      }
    }
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
    output$saved <- renderTable({ # Display local calibrations table
      send_table$saved
    })
    output$restart_table <- renderTable({ # Display remotely saved calibrations tables
      send_table$restarted_cal
    })
    complete$SpC <- FALSE
  }, ignoreInit = TRUE)

  ### Save/Delete turbidity
  observeEvent(input$save_cal_turb, {
    if (validation_check$turb) {

      calibration_data$turb <- data.frame(calibration_id = calibration_data$next_id,
                                          turb1_std = input$turb1_std,
                                          turb1_pre = input$turb1_pre,
                                          turb1_post = input$turb1_post,
                                          turb2_std = input$turb2_std,
                                          turb2_pre = input$turb2_pre,
                                          turb2_post = input$turb2_post)
      if (!complete$turbidity) {
        DBI::dbAppendTable(pool, "calibrate_turbidity", calibration_data$turb)
        complete$turbidity <- TRUE
        shinyjs::show("delete_turb")
      } else {
        DBI::dbExecute(pool, paste0("UPDATE calibrate_turbidity SET turb1_std = ", input$turb1_std,
                                    ", turb1_pre = ", input$turb1_pre,
                                    ", turb1_post = ", input$turb1_post,
                                    ", turb2_std = ", input$turb2_std,
                                    ", turb2_pre = ", input$turb2_pre,
                                    ", turb2_post = ", input$turb2_post,
                                    " WHERE calibration_id = ", calibration_data$next_id))
      }
      if ("Turbidity calibration" %in% send_table$saved[ ,1] | "Turbidity calibration" %in% send_table$restarted_cal[ ,1]) {
        shinyalert::shinyalert(title = "Turbidity calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "Turbidity calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet") {
        send_table$saved[1,1] <- "Turbidity calibration"
      } else if (!("Turbidity calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved) + 1,1] <- "Turbidity calibration"
      }
      output$saved <- renderTable({ # Display local calibrations table
        send_table$saved
      })
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$delete_turb, {
    shinyalert::shinyalert("Deleting...", type = "info")
    DBI::dbExecute(pool, paste0("DELETE FROM calibrate_turbidity WHERE calibration_id = ", calibration_data$next_id))
    #reset the fields
    reset_turb()
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved[1] != "Turbidity calibration" , , drop=FALSE]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$restarted_cal[1] != "Turbidity calibration" , , drop=FALSE]
    if (nrow(send_table$saved) == 0) {
      if (!restarted$restarted) {
        send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE)
      } else {
        send_table$saved <- data.frame("Saved calibrations (this session)" = "Nothing saved yet", check.names = FALSE)
      }
    }
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
    output$saved <- renderTable({ # Display local calibrations table
      send_table$saved
    })
    output$restart_table <- renderTable({ # Display remotely saved calibrations tables
      send_table$restarted_cal
    })
    complete$turbidity <- FALSE
  }, ignoreInit = TRUE)

  ### Save/Delete DO
  observeEvent(input$save_cal_DO, {
    if (validation_check$DO) {
      calibration_data$DO <- data.frame(calibration_id = calibration_data$next_id,
                                        baro_press_pre = input$baro_press_pre,
                                        baro_press_post = input$baro_press_post,
                                        do_pre_mgl = input$DO_pre,
                                        do_post_mgl = input$DO_post)
      if (!complete$DO) {
        DBI::dbAppendTable(pool, "calibrate_dissolved_oxygen", calibration_data$DO)

        complete$DO <- TRUE
        shinyjs::show("delete_DO")
      } else {
        DBI::dbExecute(pool, paste0("UPDATE calibrate_dissolved_oxygen SET baro_press_pre = ", input$baro_press_pre,
                                    ", baro_press_post = ", input$baro_press_post,
                                    ", do_pre_mgl = ", input$DO_pre,
                                    ", do_post_mgl = ", input$DO_post,
                                    " WHERE calibration_id = ", calibration_data$next_id))
      }
      if ("DO calibration" %in% send_table$saved[ ,1] | "DO calibration" %in% send_table$restarted_cal[ ,1]) {
        shinyalert::shinyalert(title = "DO calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "DO calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet") {
        send_table$saved[1,1] <- "DO calibration"
      } else if (!("DO calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved) + 1,1] <- "DO calibration"
      }
      output$saved <- renderTable({ # Display local calibrations table
        send_table$saved
      })
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$delete_DO, {
    shinyalert::shinyalert("Deleting...", type = "info")
    DBI::dbExecute(pool, paste0("DELETE FROM calibrate_dissolved_oxygen WHERE calibration_id = ", calibration_data$next_id))
    #reset the fields
    reset_do()
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved[1] != "DO calibration" , , drop = FALSE]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$restarted_cal[1] != "DO calibration" , , drop = FALSE]
    if (nrow(send_table$saved) == 0) {
      if (!restarted$restarted) {
        send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE)
      } else {
        send_table$saved <- data.frame("Saved calibrations (this session)" = "Nothing saved yet", check.names = FALSE)
      }
    }
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
    output$saved <- renderTable({ # Display local calibrations table
      send_table$saved
    })
    output$restart_table <- renderTable({ # Display remotely saved calibrations tables
      send_table$restarted_cal
    })
    complete$DO <- FALSE
  }, ignoreInit = TRUE)

  ### Save/Delete depth
  observeEvent(input$save_cal_depth, {
    if (validation_check$depth) {

      calibration_data$depth <- data.frame(calibration_id = calibration_data$next_id,
                                           depth_check_ok = input$depth_check_ok,
                                           depth_changes_ok = if (input$depth_changes_ok != "Not Checked") input$depth_changes_ok else NA)
      if (!complete$depth) {
        DBI::dbAppendTable(pool, "calibrate_depth", calibration_data$depth)
        complete$depth <- TRUE
        shinyjs::show("delete_depth")
      } else {
        if (input$depth_changes_ok != "Not Checked") {
          DBI::dbExecute(pool, paste0("UPDATE calibrate_depth SET depth_check_ok = '", input$depth_check_ok, "', depth_changes_ok ='", input$depth_changes_ok, "' WHERE calibration_id = ", calibration_data$next_id))
        } else {
          DBI::dbExecute(pool, paste0("UPDATE calibrate_depth SET depth_check_ok = '", input$depth_check_ok, "', depth_changes_ok = NULL WHERE calibration_id = ", calibration_data$next_id))
        }

      }
      if ("Depth calibration" %in% send_table$saved[ ,1] | "Depth calibration" %in% send_table$restarted_cal[ ,1]) {
        shinyalert::shinyalert(title = "Depth calibration overwritten", type = "success", timer = 2000, immediate = TRUE)
      } else {
        shinyalert::shinyalert(title = "Depth calibration saved", type = "success", timer = 2000, immediate = TRUE)
      }
      if (send_table$saved[1,1] == "Nothing saved yet") {
        send_table$saved[1,1] <- "Depth calibration"
      } else if (!("Depth calibration" %in% send_table$saved[ ,1])) {
        send_table$saved[nrow(send_table$saved) + 1,1] <- "Depth calibration"
      }
      output$saved <- renderTable({ # Display local calibrations table
        send_table$saved
      })
    } else {
      shinyalert::shinyalert(title = "Validate your measurements first!", type = "error", timer = 2000)
    }
  }, ignoreInit = TRUE)
  observeEvent(input$delete_depth, {
    shinyalert::shinyalert("Deleting...", type = "info")
    DBI::dbExecute(pool, paste0("DELETE FROM calibrate_depth WHERE calibration_id = ", calibration_data$next_id))

    #reset the fields
    reset_depth()
    #remove from display tables
    send_table$saved <- send_table$saved[send_table$saved[1] != "Depth calibration" , , drop = FALSE]
    send_table$restarted_cal <- send_table$restarted_cal[send_table$restarted_cal[1] != "Depth calibration" , , drop = FALSE]
    if (nrow(send_table$saved) == 0) {
      if (!restarted$restarted) {
        send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE)
      } else {
        send_table$saved <- data.frame("Saved calibrations (this session)" = "Nothing saved yet", check.names = FALSE)
      }
    }
    shinyalert::shinyalert("Deleted", type = "success", timer = 2000, immediate = TRUE)
    output$saved <- renderTable({ # Display local calibrations table
      send_table$saved
    })
    output$restart_table <- renderTable({ # Display remotely saved calibrations tables
      send_table$restarted_cal
    })
    complete$depth <- FALSE
  }, ignoreInit = TRUE)


  observeEvent({input$submit_btn}, {
    if (!("Basic info" %in% send_table$saved[ ,1] | "Basic info" %in% send_table$restarted_cal[ ,1])) {
      shinyalert::shinyalert(title = "There is no basic information yet!!!", text = "Fill in your name, calibration time and date, and required serial numbers.", type = "error")
    } else if (!("Temperature calibration" %in% send_table$saved[ ,1] | "Temperature calibration" %in% send_table$restarted_cal[ ,1])) {
      shinyalert::shinyalert(title = "Temperature calibration is mandatory", text = "If you've filled it in already you probably forgot to save it!", type = "error")
    } else {
      shinyalert::shinyalert("Are you sure?", "Finalized calibrations cannot be edited.", showCancelButton = TRUE,
                             callbackR = function(x) {
                               if (x) {# Mark it as complete == TRUE. Read in calibrations again as the sheet now has a new row or a row that is being edited from incomplete calibration.
                                 DBI::dbExecute(pool, paste0("UPDATE calibrations SET complete = 'TRUE' WHERE calibration_id = ", calibration_data$next_id))
                                 calibrations$calibrations <- DBI::dbReadTable(pool, "calibrations")

                                 calibrations$calibrations[calibrations$calibrations$calibration_id == calibration_data$next_id, "complete"] <- TRUE  #mark as complete in the local df as well
                                 shinyalert::shinyalert(title = paste0("Calibration finalized."),
                                                        type = "success", immediate = TRUE)
                                 #Reset fields
                                 reset_basic(keep_observer = TRUE)
                                 reset_ph()
                                 reset_temp()
                                 reset_orp()
                                 reset_spc()
                                 reset_turb()
                                 reset_do()
                                 reset_depth()
                                 # Reset complete flags
                                 complete$basic <- FALSE
                                 complete$temperature <- FALSE
                                 complete$SpC <- FALSE
                                 complete$pH <- FALSE
                                 complete$orp <- FALSE
                                 complete$turbidity <- FALSE
                                 complete$DO <- FALSE
                                 complete$depth <- FALSE
                                 # Reset data.frames
                                 calibration_data$basic <- NULL
                                 calibration_data$temperature <- NULL
                                 calibration_data$SpC <- NULL
                                 calibration_data$pH <- NULL
                                 calibration_data$orp <- NULL
                                 calibration_data$turbidity <- NULL
                                 calibration_data$DO <- NULL
                                 calibration_data$depth <- NULL
                                 # Reset tables
                                 send_table$saved <- data.frame("Saved calibrations" = "Nothing saved yet", check.names = FALSE) #Title is modified later for clarity if user want to restart a cal
                                 output$saved <- renderTable({ # Display local calibrations table
                                   send_table$saved
                                 })
                                 shinyjs::hide("restart_table")
                                 updateNumericInput(session, "restart_index", value = 0) #in case the user was restarting an old calibration
                                 updateSelectInput(session, "first_selection", selected = "Calibrate")
                                 updateSelectInput(session, "selection", selected = "Basic calibration info")
                                 #Get the next ID in case user is calibrating/working in app again
                                 calibration_data$next_id <- NULL
                               }
                             } #end of callbackR
      )
    }
  }, ignoreInit = TRUE)
}
