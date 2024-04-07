box::use(
  shiny[
    moduleServer,
    NS,
    div,
    selectInput,
    p,
    observeEvent,
    updateSelectInput
  ]
)

box::use(
  app/logic/app_utils[get_table_list]
)

choices <- get_table_list()

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "argus-filter-area",
    selectInput(
      inputId = ns("application"),
      choices = choices$applications,
      label = NULL
    ),
    p(" →️ "),
    selectInput(
      inputId = ns("table"),
      choices = choices$tables[["Ebenezer"]],
      label = NULL
    )
  )
}

#' @export
server <- function(id, selected_table_name = NULL) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$application, {
      updateSelectInput(
        session = session,
        "table",
        choices = choices$tables[[input$application]]
      )
    })

    observeEvent(input$table, {
      selected_table_name(input$table)
    })

  })
}
