box::use(
  shiny,
  shinymanager,
  dplyr[
    select,
    everything
  ],
)

box::use(
  app/view/mod_add,
  app/view/mod_view,
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

credentials <- data.frame(
  user = c("huginn", "muninn", "odin"),
  password = c("U27nyPaK#TWkDpNC", "U27nyPaK#TWkDpNC", "RB#ZuHVkSN6y$Lb8"),
  admin = c(FALSE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

check_credentials <- shinymanager$check_credentials(credentials)

login_greeting <- shiny$tags$div(
  style = "text-align: center; padding: 10px;",
  shiny$tags$h4(
    paste0(
      sample(c("\U0001F441", "\U0001F50D", "\U0001F4CA", "\U0001F5C3"), 1),
      " Hello, Deepansh!"
    )
  )
)

#' @export
ui <- shinymanager$secure_app(
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
    shiny$uiOutput("selector_ui"),
    shiny$hr(),
    shiny$uiOutput("data_area_ui")
  ),
  tags_top = login_greeting
)

#' @export
server <- function(input, output, session) {
  ns <- session$ns

  shinymanager$secure_server(check_credentials)

  table_list_data <- get_table_list()

  app_state <- shiny$reactiveValues(
    mode = "view",
    operation = shiny$reactive({
      "viewing"
    }),
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
    selected_row = shiny$reactive({
      1
    }),
    selected_row_data = shiny$eventReactive(
      c(
        app_state$selected_row(),
        app_state$table_data()
      ),
      {
        app_state$table_data()[app_state$selected_row(), ]
      }
    ),
    user_inputs = shiny$reactive({
      NULL
    })
  )

  shiny$observeEvent(input$app_mode, {
    app_state$mode <- input$app_mode
  })

  shiny$observeEvent(app_state$mode, {

    if (!is.null(app_state$observers$save_observer)) {
      app_state$observers$save_observer$destroy()
    }

    if (!is.null(app_state$observers$delete_observer)) {
      app_state$observers$delete_observer$destroy()
    }

    if (!is.null(app_state$observers$confirm_observer)) {
      app_state$observers$confirm_observer$destroy()
    }

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

  shiny$observeEvent(app_state$selected_row(), {
    shiny$removeUI(ns("data_area_ui"))
    output$data_area_ui <- NULL
    if (app_state$mode == "view") {
      output$data_area_ui <- shiny$renderUI({
        mod_view$ui(
          ns("view")
        )
      })
      mod_view$server(
        "view",
        app_state
      )
    } else {
      output$data_area_ui <- shiny$renderUI({
        mod_add$ui(
          ns("add")
        )
      })
      mod_add$server(
        "add",
        app_state
      )
    }
  }, ignoreInit = TRUE)
}
