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
    shiny$selectInput(
      inputId = ns("operation"),
      choices = c(
        "Viewing" = "viewing",
        "Editing" = "editing"
      ),
      label = NULL
    ),
    shiny$numericInput(
      inputId = ns("row"),
      min = 1,
      max = 1,
      value = 1,
      label = NULL
    ),
    shiny$p("of"),
    shiny$textOutput(
      ns("total_rows")
    ),
    shiny$p("entries from"),
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
    ),
    shiny$actionButton(
      inputId = ns("go"),
      label = NULL,
      icon = shiny$icon("arrow-right"),
      class = "go-button"
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

    table_data <- shiny$eventReactive(input$table, {
      process_table_data(
        get_data(
          input$table
        )
      )
    })

    shiny$observeEvent(input$row, {
      if (is.na(input$row)) {
        shiny$updateNumericInput(
          session = session,
          "row",
          value = 1
        )
      }
    })

    shiny$observeEvent(input$go, {
      selected$table_name <- input$table
      selected$row <- input$row
      selected$operation <- input$operation
      selected$table_data <- table_data
    })

    shiny$observeEvent(table_data(), {

      total <- nrow(table_data())

      shiny$updateNumericInput(
        session = session,
        "row",
        max = total
      )

      iv <- InputValidator$new()
      iv$add_rule("row", sv_lte(total, message = ""))
      iv$enable()

      output$total_rows <- shiny$renderText({
        total
      })

    })


  })
}
