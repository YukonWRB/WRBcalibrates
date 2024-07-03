app_ui <- function(request) {
  # Set up for background color when validating calibrations
  instrSelectBGCol <- '
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
        tags$link(rel = "apple-touch-icon", href = "app-icon.png"),
        tags$link(rel = "icon", type = "image/png", href = "app-icon.png"),
        tags$style(type = 'text/css', ".selectize-dropdown-content {max-height: 400px; }"),
        tags$style(
          HTML(
            ".load_sensors_btn .btn {
              display: block !important;
              margin-bottom: 10px;
            }"
          )
        ),
        tags$style(
          HTML(
            ".show_sensors_btns .btn:not(.hidden) {
              display: block !important;
            }"
          )
        ),
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/2.2.1/js.cookie.min.js")  # Cookies library
      ),
      shinyjs::extendShinyjs(text = instrSelectBGCol, functions = c("backgroundCol")),

      # Title
      titlePanel("Instrument Calibration and Tracking"),

      # Tabs
      navbarPage(
        title = "",
        theme = shinythemes::shinytheme("flatly"),
        collapsible = TRUE,
        id = "first_selection",
        fluid = TRUE,

        tabPanel(
          "Calibrate",
          sidebarLayout(
            sidebarPanel(
              selectizeInput("selection", label = "Select a parameter", choices = c("Basic calibration info", "Temperature calibration", "Conductivity calibration", "pH calibration", "ORP calibration", "Turbidity calibration", "DO calibration", "Depth calibration")),
              conditionalPanel(
                condition = "input.selection == 'Basic calibration info'",
                uiOutput("observer"),
                shinyWidgets::airDatepickerInput("obs_datetime", label = "Calibration date/time (modify with the menu only)", value = .POSIXct(Sys.time(), tz = "MST"), range = FALSE, multiple = FALSE, timepicker = TRUE, maxDate = Sys.Date() + 1, startView = Sys.Date(), update_on = "change", timepickerOpts = shinyWidgets::timepickerOptions(minutesStep = 15, timeFormat = "HH:mm")),
                textOutput("instrument_reminder"),
                uiOutput("ID_sensor_holder"),
                uiOutput("ID_handheld_meter"),
                actionButton("save_basic_info", "Save this parameter info"),
              ),
              conditionalPanel(
                condition = "input.selection == 'pH calibration'",
                numericInput("pH1_std", label = "Low pH solution value", value = 4),
                numericInput("pH2_std", label = "Neutral pH solution value", value = 7),
                numericInput("pH3_std", label = "High pH solution value", value = 10),
                numericInput("pH1_pre_val", label = "pH 4 Pre-Cal Value", value = ""),
                numericInput("pH2_pre_val", label = "pH 7 Pre-Cal Value", value = ""),
                numericInput("pH3_pre_val", label = "pH 10 Pre-Cal Value", value = ""),
                numericInput("pH1_mV", label = "pH 4 mV", value = ""),
                numericInput("pH2_mV", label = "pH 7 mV", value = ""),
                numericInput("pH3_mV", label = "pH 10 mV", value = ""),
                actionButton("show_post_pH", "Show post-cal fields"),
                numericInput("pH1_post_val", label = "pH 4 Post-Cal Value", value = 4),
                numericInput("pH2_post_val", label = "pH 7 Post-Cal Value", value = 7),
                numericInput("pH3_post_val", label = "pH 10 Post-Cal Value", value = 10),
                actionButton("save_cal_pH", "Save this sheet"),
                actionButton("delete_pH", "Delete this sheet")
              ),
              conditionalPanel(
                condition = "input.selection == 'Temperature calibration'",
                textInput("temp_reference_desc", label = "Temp Reference Type", value = "Lab thermometer"),
                numericInput("temp_reference", label = "Reference Temp", value = ""),
                numericInput("temp_observed", label = "Sensor Temp", value = ""),
                actionButton("save_cal_temp", "Save this sheet"),
                actionButton("delete_temp", "Delete this sheet")
              ),
              conditionalPanel(
                condition = "input.selection == 'Conductivity calibration'",
                numericInput("SpC1_std", label = "SpC Low-Range Standard", value = 0),
                numericInput("SpC2_std", label = "SpC High-Range Standard", value = 1413),
                checkboxInput("spc_or_not", "Enter non-specific conductivity instead?", value = FALSE),
                numericInput("SpC1_pre", label = "SpC Low-Range Pre-Cal Value", value = ""),
                numericInput("SpC2_pre", label = "SpC High-Range Pre-Cal Value", value = ""),
                actionButton("show_post_SpC", "Show post-cal fields"),
                numericInput("SpC1_post", label = "SpC Low-Range Post-Cal Value", value = 0),
                numericInput("SpC2_post", label = "SpC High-Range Post-Cal Value", value = 1413),
                actionButton("save_cal_SpC", "Save this sheet"),
                actionButton("delete_SpC", "Delete this sheet")
              ),
              conditionalPanel(
                condition = "input.selection == 'ORP calibration'",
                numericInput("orp_std", label = "ORP Standard solution mV", value = ""),
                numericInput("orp_pre_mV", label = "ORP mV Pre-Cal Value", value = ""),
                actionButton("show_post_orp", "Show post-cal fields"),
                numericInput("orp_post_mV", label = "ORP mV Post-Cal Value", value = ""),
                actionButton("save_cal_orp", "Save this sheet"),
                actionButton("delete_orp", "Delete this sheet")
              ),
              conditionalPanel(
                condition = "input.selection == 'Turbidity calibration'",
                numericInput("turb1_std", label = "Low Turb Standard Value", value = 0),
                numericInput("turb2_std", label = "High Turb Standard Value", value = 124),
                numericInput("turb1_pre", label = "Low Turb Pre-cal Value", value = ""),
                numericInput("turb2_pre", label = "High Turb Pre-cal Value", value = ""),
                actionButton("show_post_turb", "Show post-cal fields"),
                numericInput("turb1_post", label = "Low Turb Post-cal Value", value = 0),
                numericInput("turb2_post", label = "High Turb Post-cal Value", value = 124),
                actionButton("save_cal_turb", "Save this sheet"),
                actionButton("delete_turb", "Delete this sheet")
              ),
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
                actionButton("save_cal_DO", "Save this sheet"),
                actionButton("delete_DO", "Delete this sheet")
              ),
              conditionalPanel(
                condition = "input.selection == 'Depth calibration'",
                radioButtons(inputId = "depth_check_ok", label = "Depth sensor output near 0 or as expected in air?", choiceNames = c("FALSE", "TRUE"), choiceValues = c("FALSE", "TRUE")),
                radioButtons(inputId = "depth_changes_ok", label = "Depth sensor output changes as expected with depth?", choiceNames = c("Not Checked", "FALSE", "TRUE"), choiceValues = c("Not Checked", "FALSE", "TRUE")),
                actionButton("save_cal_depth", "Save this sheet"),
                actionButton("delete_depth", "Delete this sheet")
              ),
              actionButton("submit_btn", "Finalize + submit calibration", style = c("margin-top: 10px;"))
            ),
            mainPanel(
              DT::dataTableOutput("calibration_instruments_table"),
              tableOutput("restart_table"),
              tableOutput("saved"),
              htmlOutput("pH_mV_note"),
              htmlOutput("ORP_molarity_note")
            )
          )
        ),
        tabPanel(
          "Add/modify instruments",
          sidebarLayout(
            sidebarPanel(
              selectizeInput("existing_serial_no", "Search serial numbers or 'New record' for a new instrument", choices = "New record"),
              textInput("serial_no", "New serial no (add alias by appending to serial #, e.g. 012345Blue)", value = "Search first!"),
              uiOutput("recorder"),
              selectizeInput("make", label = "Instrument make", choices = "placeholder"),
              selectizeInput("model", label = "Instrument model", choices = "placeholder"),
              selectizeInput("type", label = "Instrument type", choices = "placeholder"),
              textInput("instrument_owner", "Instrument owner", value = ""),
              checkboxInput("replaceableSensors", "Replaceable sensors?", value = FALSE),
              textInput("asset_tag", "Asset tag number (if exists)", value = ""),
              dateInput("date_in_service", label = "Date in service (if known)"),
              dateInput("date_purchased", label = "Date purchased (if known)"),
              dateInput("date_retired", label = "Date retired (if known)"),
              uiOutput("retired_by"),
              actionButton("save_cal_instrument", "Save new instrument")
            ),
            mainPanel(
              DT::dataTableOutput("manage_instruments_table")
            )
          )
        ),
        tabPanel(
          "Maintain instruments",
          sidebarLayout(
            sidebarPanel(
              DT::dataTableOutput("maintain_instr_table")
            ),
            mainPanel(
              DT::dataTableOutput("past_instr_maintenance"),
              actionButton("clear_selection", "Clear selection"),
              textAreaInput("maintain_comment", "Describe the new maintenance performed", "", height = "100px", width = "800px"),
              uiOutput("maintain_recorder"),
              actionButton("submit_instr_maintain", "Save new maintenance event")
            )
          )
        ),
        tabPanel(
          "Change/maintain sensors",
          sidebarLayout(
            sidebarPanel(
              textOutput("sensors_reminder"),
              selectizeInput("maintain_serial", "Select your instrument", choices = "loading choices..."),
              div(class = "load_sensors_btn", actionButton("load_sensors", "Show sensors")),
              div(class = "show_sensors_btns",
                  actionButton("sensor1_show", "Slot 1", style = c("margin-bottom: 5px;")),
                  actionButton("sensor2_show", "Slot 2", style = c("margin-bottom: 5px;")),
                  actionButton("sensor3_show", "Slot 3", style = c("margin-bottom: 5px;")),
                  actionButton("sensor4_show", "Slot 4", style = c("margin-bottom: 5px;")),
                  actionButton("sensor5_show", "Slot 5", style = c("margin-bottom: 5px;")),
                  actionButton("sensor6_show", "Slot 6", style = c("margin-bottom: 5px;")),
                  actionButton("sensor7_show", "Slot 7", style = c("margin-bottom: 5px;")),
                  actionButton("sensor8_show", "Slot 8", style = c("margin-bottom: 5px;"))
              ),
              selectizeInput("add_sensor_type_dropdown", "ADD a slot w/ sensor (CHANGE assigned sensor to the right)", choices = "placeholder"),
              selectizeInput("new_sensor_serial", "Serial number (type your own if not in yet)", choices = NULL, options = list(create = TRUE)),
              actionButton("add_new_sensor_serial", "Add new sensor to database"),
              uiOutput("add_sensor_name"),
              textOutput("add_sensor_note"),
              actionButton("add_sensor_slot", "Submit")
            ),
            mainPanel(
              DT::dataTableOutput("manage_sensors_table"),
              DT::dataTableOutput("sensor1_details"),
              DT::dataTableOutput("sensor2_details"),
              DT::dataTableOutput("sensor3_details"),
              DT::dataTableOutput("sensor4_details"),
              DT::dataTableOutput("sensor5_details"),
              DT::dataTableOutput("sensor6_details"),
              DT::dataTableOutput("sensor7_details"),
              DT::dataTableOutput("sensor8_details"),
              htmlOutput("sensor_change_note"),
              selectizeInput("change_sensor", "Assign a new sensor or leave as-is to log only maintenance note", choices = "placeholder", width = "500px"),
              selectizeInput("add_sensor_serial", "Serial number (type your own if not in yet)", choices = NULL, options = list(create = TRUE)),
              actionButton("add_new_sensor_serial2", "Add new sensor to database"),
              textAreaInput("add_comment", "Add a note (date is already captured)", "", height = "100px", width = "800px"),
              uiOutput("sensor_change_name"),
              actionButton("submit_sensor_change", "Submit new record")
            )
          )
        ),
        tabPanel(
          "Deploy/Recover instruments",
          mainPanel(
            textOutput("deploy_instrument_placeholder")
          )
        ),
        tabPanel(
          "View unfinished calibrations",
          sidebarLayout(
            sidebarPanel(
              numericInput("restart_index", label = "Select a calibration by index number", value = 0),
              actionButton("restart_calibration", "Restart selected calibration"),
              actionButton("delete_calibration", "Delete selected calibration")
            ),
            mainPanel(
              DT::dataTableOutput("incomplete_table")
            )
          )
        )
      )
    )
  )
}
