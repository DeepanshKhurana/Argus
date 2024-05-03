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
    selectInput(
      inputId = ns("operation"),
      choices = c(
        "Viewing" = "viewing",
        "Editing" = "editing"
      ),
      label = NULL
    ),
    numericInput(
      inputId = ns("row"),
      min = 1,
      max = 1,
      value = 1,
      label = NULL
    ),
    p("of"),
    textOutput(
      ns("total_rows")
    ),
    p("entries from"),
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

    table_data <- eventReactive(input$table, {
      process_table_data(
        get_data(
          input$table
        )
      )
    })

    observeEvent(input$row, {
      if (is.na(input$row)) {
        updateNumericInput(
          session = session,
          "row",
          value = 1
        )
      }
    })

    observeEvent(input$go, {
      selected$table_name <- input$table
      selected$row <- input$row
      selected$operation <- input$operation
      selected$table_data <- table_data
    })

    observeEvent(table_data(), {

      total <- nrow(table_data())

      updateNumericInput(
        session = session,
        "row",
        max = total
      )

      iv <- InputValidator$new()
      iv$add_rule("row", sv_lte(total, message = ""))
      iv$enable()

      output$total_rows <- renderText({
        total
      })

    })


  })
}
