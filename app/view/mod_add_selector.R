box::use(
  shiny,
  shinyvalidate[
    InputValidator,
    sv_lte
  ],
)

box::use(
  app/logic/app_utils[
    get_table_list,
    process_table_data
  ],
  app/logic/api_utils[
    get_data
  ],
)

choices <- get_table_list()

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "argus-filter-area",
    shiny$p("Adding new entry to"),
    shiny$selectInput(
      inputId = ns("application"),
      choices = sort(choices$applications, decreasing = TRUE),
      label = NULL
    ),
    shiny$p("—"),
    shiny$selectInput(
      inputId = ns("table"),
      choices = choices$tables[["Test"]],
      label = NULL
    )
  )
}

#' @export
server <- function(id, selected) {
  shiny$moduleServer(id, function(input, output, session) {

    shiny$observeEvent(input$application, {
      shiny$updateSelectInput(
        session = session,
        "table",
        choices = choices$tables[[input$application]]
      )
    })

  })
}
