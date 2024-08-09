box::use(
  shiny,
  dplyr[
    select,
    everything
  ],
  shinyalert[
    shinyalert
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
    put_row,
    delete_row
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
    ),
    shiny$uiOutput(
      ns("delete_button")
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

      if (shiny$isTruthy(input$row) && input$row > app_state$total_rows()) {
        shiny$updateNumericInput(
          session = session,
          "row",
          value = app_state$total_rows(),
          max = app_state$total_rows()
        )
      } else {
        app_state$selected_row <- shiny$reactive({
          ifelse(
            shiny$isTruthy(input$row),
            input$row,
            1
          )
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

          delete_icon <- ifelse(
            app_state$mode == "add",
            "square-minus",
            "trash-can"
          )

          output$delete_button <- shiny$renderUI({
            shiny$actionButton(
              inputId = ns("delete"),
              label = NULL,
              icon = shiny$icon(delete_icon),
              class = "delete-button"
            )
          })

        } else {
          shiny$removeUI("save_button")
          shiny$removeUI("delete_button")
          output$save_button <- NULL
          output$delete_button <- NULL
          NULL
        }
      }
    )

    app_state$observers$delete_observer <- shiny$observeEvent(
      eventExpr = input$delete,
      handlerExpr = {
        if (app_state$mode == "add") {
          app_state$mode <- "view"
          runjs("App.toggleIconMode('.argus-icon');")
        } else {
          shinyalert(
            title = "Delete entry?",
            text = "This permanently removes this row.",
            showConfirmButton = TRUE,
            showCancelButton = TRUE,
            confirmButtonText = "Yes",
            cancelButtonText = "No",
            inputId = "confirm" # shinyalert does not need ns()
          )
        }
      },
      ignoreInit = TRUE
    )

    app_state$observers$confirm_observer <- shiny$observeEvent(
      eventExpr = input$confirm,
      handlerExpr = {

        if (input$confirm) {
          delete_row(
            table_name = app_state$selected_table(),
            row = app_state$selected_row_data()$id |>
              unlist()
          )
          app_state$table_data <- shiny$reactive({
            process_table_data(
              get_data(
                app_state$selected_table()
              )
            ) |> select(
              id, everything()
            )
          })
        }

        app_state$mode <- "view"
        app_state$operation <- shiny$reactive({
          "viewing"
        })

      },
      ignoreInit = TRUE
    )

    app_state$observers$save_observer <- shiny$observeEvent(input$save, {

      if (app_state$mode == "view") { # Edit mode

        put_row(
          table_name = app_state$selected_table(),
          is_update = TRUE,
          app_state$user_inputs() |> unlist()
        )

        app_state$operation <- shiny$reactive({
          "viewing"
        })

      } else { # Add mode

        put_row(
          table_name = app_state$selected_table(),
          is_update = FALSE,
          app_state$user_inputs() |> unlist()
        )

        app_state$mode <- "view"
        runjs("App.toggleIconMode('.argus-icon');")
      }

      app_state$table_data <- shiny$reactive({
        process_table_data(
          get_data(
            app_state$selected_table()
          )
        ) |> select(
          id, everything()
        )
      })

    }, ignoreInit = TRUE)

  })
}
