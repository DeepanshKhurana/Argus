box::use(
  shiny,
)

box::use(
  app/view/mod_add,
  app/view/mod_view,
  app/view/mod_edit,
  app/view/mod_selector,
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

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$fluidPage(
    class = "argus",
    shiny$div(
      class = "argus-header",
      shiny$icon(
        "eye",
        class = "fa-solid argus-icon"
      ),
      shiny$h1(
        "Argus"
      )
    ),
    shiny$uiOutput(ns("selector_ui")),
    shiny$hr(),
    shiny$uiOutput(ns("data_area_ui"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    table_list_data <- get_table_list()

    app_state <- shiny$reactiveValues(
      mode = "view",
      operation = "viewing",
      apps = table_list_data$applications,
      tables = shiny$eventReactive(app_state$selected_app, {
        table_list_data$tables[[app_state$selected_app]]
      }),
      selected_app = table_list_data$applications[1],
      selected_table = shiny$eventReactive(app_state$selected_app, {
        table_list_data$tables[[app_state$selected_app]][1]
      }),
      table_data = shiny$eventReactive(app_state$selected_table, {
        process_table_data(
          get_data(
            app_state$selected_table()
          )
        )
      }),
      total_rows = shiny$eventReactive(app_state$table_data(), {
        nrow(app_state$table_data())
      }),
      selected_row = 1,
      selected_row_data = shiny$eventReactive(app_state$selected_row, {
        app_state$table_data()[app_state$selected_row, ]
      })
    )

    shiny$observeEvent(input$app_mode, {
      app_state$mode <- input$app_mode
    })

    shiny$observeEvent(app_state$mode, {
        output$selector_ui <- shiny$renderUI({
          mod_selector$ui(
            ns("selector"),
            app_state
          )
        })
        mod_selector$server(
          "selector",
          app_state
        )
    })

    shiny$observeEvent(app_state$selected_row_data(), {
      # TODO Deepansh
      # Implement logic to view data
    }, ignoreInit = TRUE)
  })
}
