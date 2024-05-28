#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


app_ui <- function(request) {
  #set up for background color when validating calibrations
  jsCode <- '
    shinyjs.backgroundCol = function(params) {
      var defaultParams = {
        id : null,
        col : "red"
      };
      params = shinyjs.getParams(params, defaultParams);

      var el = $("#" + params.id);
      el.css("background-color", params.col);
    }'

  tagList(
    fluidPage(
      shinyjs::useShinyjs(),
      tags$head(
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        tags$link(rel = "manifest", href = "manifest.json"),
        tags$link(rel = "apple-touch-icon", href = "app-icon.png"),
        tags$link(rel = "icon", type = "image/png", href = "app-icon.png"),
        tags$script(src = "serviceworker.js", type = "text/javascript"),
        tags$style(type = 'text/css', ".selectize-dropdown-content {max-height: 400px; }"),
        tags$style(
          HTML(
            ".load_sensors_btn .btn {
        display: block !important;
        margin-bottom: 10px;}"
          )
        ),
        tags$style(
          HTML(
            ".show_sensors_btns .btn:not(.hidden) {
        display: block !important;}"
          )
        )
      ),

      shinyjs::extendShinyjs(text = jsCode, functions = c("backgroundCol")),

      # Title
      titlePanel("Instrument Calibration and Tracking"),
      # Broad category selection
      selectInput("first_selection", label = "Choose a task", choices = c("Calibrate", "Manage instruments", "Manage sensors and log maintenance", "View unfinished calibrations"#, "View completed calibrations"
      )),
      # Input form
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.first_selection == 'Manage instruments'",
            selectInput("existing_serial_no", "Search existing serial numbers", choices = "New record"),
            textInput("serial_no", "New serial no (add alias by appending to serial #, e.g. 012345Blue)", value = "Search first!"),
            textInput("recorder", label = "Observer name", value = ""),
            selectInput("make", label = "Instrument make", choices = c("", "YSI", "Solinst", "HOBO/Onset", "OTT", "RBR", "Campbell Sci", "In-Situ", "Other"), ),
            textInput("model", label = "Instrument model", value = ""),
            selectInput("type", label = "Instrument type", choices = c("", "Logger (single/multi-param deployable, fixed sensors)", "Sonde (multi-param deployable, interchangeable sensors)", "Bulkhead (requires handheld, not deployable)", "Handheld (connects to bulkhead)", "Sensor (single/multi-param not deployable i.e. thermometer or combo meter")), #beware if changing names that 'Handheld (connects to bulkhead)' is employed in app_server
            textInput("asset_tag", "Asset tag number", value = ""),
            dateInput("date_in_service", label = "Date in service"),
            dateInput("date_purchased", label = "Date purchased"),
            dateInput("date_retired", label = "Date retired"),
            textInput("retired_by", label = "Retired by", value = ""),
            actionButton("save_cal_instrument", "Save new instrument")
          ),
          conditionalPanel(
            # On initial load, this page will only show the selectInput box. Once an instrument is selected, instrument details will be listed in the mainPanel and sensors will appear in the sidePanel. Each sensor will appear as an actionButton, and clicking that actionButton will load its details in the mainPanel. Once that happens, the sidebarPanel will change to show actionButtons to "Change" or "Maintain" the sensor.
            condition = "input.first_selection == 'Manage sensors and log maintenance'",
            textOutput("sensors_reminder"),
            selectInput("maintain_serial", "Select your instrument", choices = "loading choices..."), #updated by the server
            div(class = "load_sensors_btn",
                actionButton("load_sensors", "Show sensors"), #triggers shinyjs::show for the sensors, depending on how many are on the device
            ),
            div (class = "show_sensors_btns",
                 actionButton("sensor1_show", "Slot 1"), #updated by the server, hidden until load_sensors is clicked. Value will go on another line using HTML("line 1<br>line2)
                 actionButton("sensor2_show", "Slot 2"),
                 actionButton("sensor3_show", "Slot 3"),
                 actionButton("sensor4_show", "Slot 4"),
                 actionButton("sensor5_show", "Slot 5"),
                 actionButton("sensor6_show", "Slot 6"),
                 actionButton("sensor7_show", "Slot 7"),
                 actionButton("sensor8_show", "Slot 8")
            ),


            selectizeInput("add_sensor_dropdown", "Add a slot w/ sensor", choices = c("", "pH", "pH/ORP", "ORP", "Conductivity/Temperature", "Conductivity", "Turbidity", "Temperature", "DO", "Depth", "Nitrate", "Ammonium", "Chloride" , "DOM", "Rhodamine", "Total algae", "Central Wiper")),
            textInput("new_sensor_serial", "Serial number", ""),
            textOutput("add_sensor_note"),
            textInput("add_sensor_name", "What's your name?", value = ""),
            actionButton("add_sensor", "Submit")
          ),
          conditionalPanel(
            condition = "input.first_selection == 'View unfinished calibrations'",
            numericInput("restart_index", label = "Select a calibration by index number", value = 0),
            actionButton("restart_calibration", "Restart selected calibration"),
            actionButton("delete_calibration", "Delete selected calibration")
          ),
          # conditionalPanel(
          #   condition = "input.first_selection == 'View completed calibrations'",
          #   shinyWidgets::airDatepickerInput("old_cal_start", label = "Start date/time", range = TRUE, value = c(as.Date("1980-01-01"), Sys.Date()+1), multiple = FALSE, maxDate = Sys.Date()+1, startView = Sys.Date(), update_on = "close"),
          #   textInput("old_cal_user", "Calibrator (fuzzy match)", value = ""),
          #   textInput("old_cal_serial", "Serial number", value = ""),
          #   actionButton("filter_old_cals", "Apply filter"),
          # ),
          conditionalPanel(
            condition = "input.first_selection == 'Calibrate'",
            selectInput("selection",
                        label = "Select a parameter",
                        choices = c("Basic calibration info", "Temperature calibration", "Conductivity calibration", "pH calibration", "ORP calibration", "Turbidity calibration", "DO calibration", "Depth calibration")),
            conditionalPanel(
              condition = "input.selection == 'Basic calibration info'",
              textInput("observer", label = "Calibrator name", value = ""),
              shinyWidgets::airDatepickerInput("obs_datetime", label = "Calibration date/time", value = .POSIXct(Sys.time(), tz = "MST"), range = FALSE, multiple = FALSE, timepicker = TRUE, maxDate = Sys.Date()+1, startView = Sys.Date(), update_on = "close", timepickerOpts = shinyWidgets::timepickerOptions(minutesStep = 15, timeFormat = "HH:mm")),
              textOutput("instrument_reminder"),
              div(
                selectizeInput("ID_sensor_holder", label = "Logger/bulkhead/sonde serial #", choices = ""),
                style = "color: white; background-color: blue;"
              ),
              div(
                selectizeInput("ID_handheld_meter", label = "Handheld serial # (if applicable)", choices = "NA"),
                style = "color: white; background-color: green;"
              ),
              actionButton("save_basic_info", "Save this sheet")
            )
          ),
          conditionalPanel(
            condition = "input.first_selection == 'Calibrate'",
            conditionalPanel(
              condition = "input.selection == 'pH calibration'",
              numericInput("pH1_std", label = "Low pH solution value", value = "4"),
              numericInput("pH2_std", label = "Neutral pH solution value", value = "7"),
              numericInput("pH3_std", label = "High pH solution value", value = "10"),
              numericInput("pH1_pre_val", label = "pH 4 Pre-Cal Value", value = ""),
              numericInput("pH2_pre_val", label = "pH 7 Pre-Cal Value", value = ""),
              numericInput("pH3_pre_val", label = "pH 10 Pre-Cal Value", value = ""),
              numericInput("pH1_mV", label = "pH 4 mV", value = ""),
              numericInput("pH2_mV", label = "pH 7 mV", value = ""),
              numericInput("pH3_mV", label = "pH 10 mV", value = ""),
              #Important!!! the post-val fields are simply hidden using shinyjs::hide in the server. If they're ever needed, find the section near the top of the server file.
              actionButton("show_post_pH", "Show post-cal fields"),
              numericInput("pH1_post_val", label = "pH 4 Post-Cal Value", value = 4),
              numericInput("pH2_post_val", label = "pH 7 Post-Cal Value", value = 7),
              numericInput("pH3_post_val", label = "pH 10 Post-Cal Value", value = 10),
              actionButton("validate_pH", "Validate measurements"),
              actionButton("save_cal_pH", "Save this sheet"),
              actionButton("delete_pH", "Delete this sheet") #This should toggle available only once created or loaded in a previous calibration
            )
          ),
          conditionalPanel(
            condition = "input.first_selection == 'Calibrate'",
            conditionalPanel(
              condition = "input.selection == 'ORP calibration'",
              numericInput("orp_std", label = "ORP Standard solution mV", value = ""),
              numericInput("orp_pre_mV", label = "ORP mV Pre-Cal Value", value = ""),
              #Important!!! the post-val fields are simply hidden using shinyjs::hide in the server. If they're ever needed, find the section near the top of the server file.
              actionButton("show_post_orp", "Show post-cal fields"),
              numericInput("orp_post_mV", label = "ORP mV Post-Cal Value", value = ""),
              actionButton("validate_orp", "Validate measurements"),
              actionButton("save_cal_orp", "Save this sheet"),
              actionButton("delete_orp", "Delete this sheet") #This should toggle available only once created or loaded in a previous calibration
              )
          ),
          conditionalPanel(
            condition = "input.first_selection == 'Calibrate'",
            conditionalPanel(
              condition = "input.selection == 'Turbidity calibration'",
              numericInput("turb1_std", label = "Low Turb Standard Value", value = 0),
              numericInput("turb2_std", label = "High Turb Standard Value", value = 124),
              numericInput("turb1_pre", label = "Low Turb Pre-cal Value", value = ""),
              numericInput("turb2_pre", label = "High Turb Pre-cal Value", value = ""),
              #Important!!! the post-val fields are simply hidden using shinyjs::hide in the server. If they're ever needed, find the section near the top of the server file.
              actionButton("show_post_turb", "Show post-cal fields"),
              numericInput("turb1_post", label = "Low Turb Post-cal Value", value = 0),
              numericInput("turb2_post", label = "High Turb Post-cal Value", value = 124),
              actionButton("validate_turb", "Validate measurements"),
              actionButton("save_cal_turb", "Save this sheet"),
              actionButton("delete_turb", "Delete this sheet") #This should toggle available only once created or loaded in a previous calibration
            )
          ),
          conditionalPanel(
            condition = "input.first_selection == 'Calibrate'",
            conditionalPanel(
              condition = "input.selection == 'Temperature calibration'",
              textInput("temp_reference_desc", label = "Temp Reference Type", value = "Lab thermometer"),
              numericInput("temp_reference", label = "Reference Temp", value = ""),
              numericInput("temp_observed", label = "Sensor Temp", value = ""),
              actionButton("validate_temp", "Validate measurements"),
              actionButton("save_cal_temp", "Save this sheet"),
              actionButton("delete_temp", "Delete this sheet") #This should toggle available only once created or loaded in a previous calibration
            )
          ),
          conditionalPanel(
            condition = "input.first_selection == 'Calibrate'",
            conditionalPanel(
              condition = "input.selection == 'Conductivity calibration'",
              numericInput("SpC1_std", label = "SpC Low-Range Standard", value = 0),
              numericInput("SpC2_std", label = "SpC High-Range Standard", value = 1413),
              checkboxInput("spc_or_not", "Enter non-specific conducvitity instead?", value = FALSE),
              numericInput("SpC1_pre", label = "SpC Low-Range Pre-Cal Value", value = ""),
              numericInput("SpC2_pre", label = "SpC High-Range Pre-Cal Value", value = ""),
              #Important!!! the post-val fields are simply hidden using shinyjs::hide in the server. If they're ever needed, find the section near the top of the server file.
              actionButton("show_post_SpC", "Show post-cal fields"),
              numericInput("SpC1_post", label = "SpC Low-Range Post-Cal Value", value = 0),
              numericInput("SpC2_post", label = "SpC High-Range Post-Cal Value", value = 1413),
              actionButton("validate_SpC", "Validate measurements"),
              actionButton("save_cal_SpC", "Save this sheet"),
              actionButton("delete_SpC", "Delete this sheet") #This should toggle available only once created or loaded in a previous calibration
            )
          ),
          conditionalPanel(
            condition = "input.first_selection == 'Calibrate'",
            conditionalPanel(
              condition = "input.selection == 'DO calibration'",
              numericInput("baro_press_pre", label = "Baro Pressure Pre-Cal (mmHg)", value = ""),
              numericInput("baro_press_post", label = "Baro Pressure Post-Cal (mmHg)", value = ""),
              numericInput("DO_pre_prct", label = "DO Pre-Cal % LOCAL", value = ""),
              numericInput("DO_post_prct", label = "DO Post-Cal % LOCAL", value = ""),
              actionButton("calc_abs_DO", "Calculate mg/l values"),
              actionButton("calc_prct_DO", "Calculate % values"),
              numericInput("DO_pre", label = "DO Pre-Cal mg/l", value = ""),
              numericInput("DO_post", label = "DO Post-Cal mg/l", value = ""),
              actionButton("validate_DO", "Validate measurements"),
              actionButton("save_cal_DO", "Save this sheet"),
              actionButton("delete_DO", "Delete this sheet") #This should toggle available only once created or loaded in a previous calibration
            )
          ),
          conditionalPanel(
            condition = "input.first_selection == 'Calibrate'",
            conditionalPanel(
              condition = "input.selection == 'Depth calibration'",
              radioButtons(inputId = "depth_check_ok", label = "Depth Sensor Output Near 0 or as Expected in air?", choiceNames = c("FALSE", "TRUE"), choiceValues = c("FALSE", "TRUE")),
              radioButtons(inputId = "depth_changes_ok", label = "Depth Sensor Output Changes as Expected with depth?", choiceNames = c("Not Checked", "FALSE", "TRUE"), choiceValues = c("Not Checked", "FALSE", "TRUE")),
              actionButton("validate_depth", "Validate measurements"),
              actionButton("save_cal_depth", "Save this sheet"),
              actionButton("delete_depth", "Delete this sheet") #This should toggle available only once created or loaded in a previous calibration
            )
          ),
          actionButton("submit_btn", "Finalize Calibration")
        ), #end of sidebarPanel

        # Output tables
        mainPanel(
          conditionalPanel(
            condition = "input.first_selection == 'Calibrate'",
            DT::dataTableOutput("calibration_instruments_table"),
            tableOutput("restart_table"),
            tableOutput("saved"),
            textOutput("pH_mV_note"),
            textOutput("ORP_molarity_note")
          ),
          conditionalPanel(
            condition = "input.first_selection == 'View unfinished calibrations'",
            DT::dataTableOutput("incomplete_table")
          ),
          conditionalPanel(
            condition = "input.first_selection == 'Manage instruments'",
            DT::dataTableOutput("manage_instruments_table")
          ),
          conditionalPanel(
            condition = "input.first_selection == 'Manage sensors and log maintenance'",
            DT::dataTableOutput("manage_sensors_table"), #hidden once the user loads a specific instrument's sensors
            DT::dataTableOutput("sensor1_details"), # this and subsequent sensors are hidden until the user selects to view
            DT::dataTableOutput("sensor2_details"),
            DT::dataTableOutput("sensor3_details"),
            DT::dataTableOutput("sensor4_details"),
            DT::dataTableOutput("sensor5_details"),
            DT::dataTableOutput("sensor6_details"),
            DT::dataTableOutput("sensor7_details"),
            DT::dataTableOutput("sensor8_details"),
            selectInput("change_sensor", "Assign a new sensor", choices = c("", "pH", "pH/ORP", "ORP", "Conductivity/Temperature", "Conductivity", "Turbidity", "Temperature", "DO", "Depth", "Nitrate", "Ammonium", "Chloride" , "DOM", "Rhodamine", "Total algae", "Central Wiper")),
            textInput("add_sensor_serial", "Serial number", ""),
            textAreaInput("add_comment", "Add a note", "", height = "100px"),
            textInput("add.change_sensor.comment_name", "What's your name?"),
            actionButton("add.change_sensor.comment", "Submit new record")
          )
        )
      )
    )
  )
}
