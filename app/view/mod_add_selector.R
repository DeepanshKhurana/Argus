box::use(
  shiny[
    moduleServer,
    NS,
    div,
    selectInput,
    p,
    observeEvent,
    updateSelectInput,
    eventReactive,
    textOutput,
    renderText,
    numericInput,
    updateNumericInput,
    actionButton,
    icon
  ],
  shinyvalidate[
    InputValidator,
    sv_lte
  ]
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
  ns <- NS(id)
  div(
    class = "argus-filter-area",
    p("Adding new entry to"),
    selectInput(
      inputId = ns("application"),
      choices = sort(choices$applications, decreasing = TRUE),
      label = NULL
    ),
    p("—"),
    selectInput(
      inputId = ns("table"),
      choices = choices$tables[["Test"]],
      label = NULL
    ),
    actionButton(
      inputId = ns("go"),
      label = NULL,
      icon = icon("arrow-right"),
      class = "go-button"
    )
  )
}

#' @export
server <- function(id, selected) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$application, {
      updateSelectInput(
        session = session,
        "table",
        choices = choices$tables[[input$application]]
      )
    })

  })
}
