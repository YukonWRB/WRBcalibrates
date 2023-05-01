#Code save. This allows for only two seleted rows from a datatable, and has them colored blue and green. Could be used as inputs to the instrument numbers when doing a calibtation.

library(shiny)
library(DT)

js <- '
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
  br(), br(),
  DTOutput("my_table")
)

server <- function(input, output, session) {

  output[["my_table"]] <- renderDT({
    datatable(
      iris,
      selection = "multiple",
      callback = JS(js)
    )
  }, server = TRUE)

  observeEvent(input$my_table_rows_selected, {
    print(input$my_table_rows_selected)
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
