box::use(
  shiny,
  dplyr[
    select,
    everything
  ],
  shinyjs[
    useShinyjs,
    runjs
  ],
)

box::use(
  app/logic/app_utils[
    process_table_data
  ],
  app/logic/api_utils[
    get_data,
    put_row
  ],
)

#' @export
ui <- function(id, app_state) {
  ns <- shiny$NS(id)
  shiny$div(
    useShinyjs(),
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
          label = NULL,
          selected = app_state$operation()
        ),
        shiny$numericInput(
          inputId = ns("row"),
          min = 1,
          max = app_state$total_rows(),
          value = app_state$selected_row(),
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
    ),
    shiny$uiOutput(
      ns("save_button")
    )
  )
}

#' @export
server <- function(id, app_state) {
  shiny$moduleServer(id, function(input, output, session) {

    ns <- session$ns

    shiny$observeEvent(input$application, {
      app_state$selected_app <- input$application
    }, ignoreInit = TRUE)

    shiny$observeEvent(input$table, {
      app_state$selected_table <- shiny$reactive({
        input$table
      })
    }, ignoreInit = TRUE)

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

      if (app_state$selected_row() > total_rows) {
        shiny$updateNumericInput(
          session = session,
          "row",
          value = total_rows
        )
      }

    })

    shiny$observeEvent(input$operation, {
      app_state$operation <- shiny$reactive({
        input$operation
      })
    }, ignoreInit = TRUE)

    shiny$observeEvent(input$row, {

      if (input$row > app_state$total_rows()) {
        shiny$updateNumericInput(
          session = session,
          "row",
          value = app_state$total_rows(),
          max = app_state$total_rows()
        )
      } else {
        app_state$selected_row <- shiny$reactive({
          input$row
        })
      }

    })

    shiny$observeEvent(
      eventExpr = c(app_state$mode, app_state$operation()),
      handlerExpr = {
        if (app_state$mode == "add" || app_state$operation() == "editing") {
          output$save_button <- shiny$renderUI({
            shiny$actionButton(
              inputId = ns("save"),
              label = NULL,
              icon = shiny$icon("save"),
              class = "save-button"
            )
          })
        } else {
          shiny$removeUI("save_button")
          output$save_button <- NULL
          NULL
        }
      }
    )

    shiny$observeEvent(input$save, {

      if (app_state$mode == "view") { # Edit mode

        put_row(
          table_name = app_state$selected_table(),
          is_update = TRUE,
          app_state$user_inputs() |> unlist()
        )

        # TODO Deepansh
        # The putting works correctly, but the table does not update
        # Make sure it updates

        app_state$operation <- shiny$reactive({
          "viewing"
        })
      } else { # Add mode
        app_state$mode <- "view"
        runjs("App.toggleIconMode('.argus-icon');")
      }

    }, ignoreInit = TRUE)

  })
}
