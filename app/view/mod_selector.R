box::use(
  shiny,
  shinyvalidate[
    InputValidator,
    sv_lte
  ],
  dplyr[
    select,
    everything
  ],
)

box::use(
  app/logic/app_utils[
    process_table_data
  ],
  app/logic/api_utils[
    get_data
  ],
)

#' @export
ui <- function(id, app_state) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "argus-filter-area",
    if (app_state$mode == "add") {
      shiny$p("Adding new entry to")
    } else {
      shiny$tagList(
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
        shiny$p("entries from")
      )
    },
    shiny$selectInput(
      inputId = ns("application"),
      choices = app_state$apps,
      label = NULL,
      selected = app_state$selected_app
    ),
    shiny$p("—"),
    shiny$selectInput(
      inputId = ns("table"),
      choices = app_state$tables(),
      label = NULL,
      selected = app_state$selected_table()
    )
  )
}

#' @export
server <- function(id, app_state) {
  shiny$moduleServer(id, function(input, output, session) {

    input_validator <- InputValidator$new()

    shiny$observeEvent(input$application, {
      app_state$selected_app <- input$application
    },
    ignoreInit = TRUE)

    shiny$observeEvent(input$table, {
      app_state$selected_table <- shiny$reactive({
        input$table
      })
    },
    ignoreInit = TRUE)

    shiny$observeEvent(app_state$total_rows(), {
      total_rows <- app_state$total_rows()
      output$total_rows <- shiny$renderText({
        total_rows
      })

      shiny$updateNumericInput(
        session = session,
        "row",
        max = total_rows
      )

      input_validator$add_rule("row", sv_lte(total_rows, message = ""))
      input_validator$enable()
    })

    shiny$observeEvent(input$operation, {
      app_state$operation <- input$operation
    })

    shiny$observeEvent(input$row, {
      if (input_validator$is_valid()) {
        app_state$selected_row <- input$row
      }
    })

  })
}
