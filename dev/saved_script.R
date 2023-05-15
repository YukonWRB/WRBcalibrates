#Code save. This allows for only two seleted rows from a datatable, and has them colored blue and green. Could be used as inputs to the instrument numbers when doing a calibtation.

library(shiny)
library(DT)

table_reset <- '
var colors = ["blue", "green"];
var stack = [];
table.on("click", "tr", function() {
  var $rows = $("#my_table tbody tr"); // SIMONSIMON change the name of the table here
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

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      div(
        selectizeInput("input1", "Input 1", choices = ""),
        style = "color: white; background-color: blue;"
      ),

      div(
        selectizeInput("input2", "Input 2", choices = ""),
        style = "color: white; background-color: green;"
      )
    ),
    mainPanel(
      br(), br(),
      DTOutput("my_table")
    )

  )
)

server <- function(input, output, session) {

  output[["my_table"]] <- renderDT({
    datatable(
      iris,
      selection = "multiple",
      callback = JS(table_reset)
    )
  }, server = TRUE)

  click_count <- reactiveValues(value = 0)
  observeEvent(input$my_table_rows_selected, {
    updateSelectizeInput(session, "input1", choices = 1:nrow(iris))
    updateSelectizeInput(session, "input2", choices = 1:nrow(iris))
    if (click_count$value <= 2){
      print(input$my_table_rows_selected)
      updateSelectizeInput(session, "input1", selected = input$my_table_rows_selected[1])
      updateSelectizeInput(session, "input2", selected = input$my_table_rows_selected[2])
      click_count$value <- click_count$value+1
    } else {
      print(input$my_table_rows_selected)
      updateSelectizeInput(session, "input1", selected = input$my_table_rows_selected[2])
      updateSelectizeInput(session, "input2", selected = input$my_table_rows_selected[3])
    }
    if (length(input$my_table_rows_selected[input$my_table_rows_selected != 0]) > 2) {
      selection <- NULL
      proxy <- dataTableProxy("my_table")
      selectRows(proxy, selection, selected = FALSE)
      output[["my_table"]] <- renderDT({
        datatable(
          iris,
          selection = "multiple",
          callback = JS(js)
        )
      }, server = TRUE)
    }
  })

}

shinyApp(ui = ui, server = server)

