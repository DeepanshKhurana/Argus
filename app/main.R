box::use(
  shiny,
)

box::use(
  app/view/mod_add_selector,
  app/view/mod_add,
  app/view/mod_view,
  app/view/mod_view_selector,
  app/view/mod_edit,
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

    selected <- shiny$reactiveValues(
      table_name = NULL,
      row = NULL,
      operation = "viewing",
      table_data = NULL,
      user_input = NULL
    )

    observe_selector_ui <- function() {
      if (input$app_mode == "view" || is.null(input$app_mode)) {
        output$selector_ui <- shiny$renderUI({
          mod_view_selector$server(
            "selector",
            selected
          )
          mod_view_selector$ui(
            ns("selector")
          )
        })
      } else {
        output$selector_ui <- shiny$renderUI({
          mod_add_selector$server(
            "selector",
            selected
          )
          mod_add_selector$ui(
            ns("selector")
          )
        })
      }
    }

    observe_data_area_ui <- function() {
      shiny$req(selected$operation)
      if (selected$operation == "viewing") {
        output$data_area_ui <- shiny$renderUI({
          mod_view$server(
            "data_area",
            selected
          )
          mod_view$ui(
            ns("data_area")
          )
        })
      } else {
        output$data_area_ui <- shiny$renderUI({
          mod_edit$server(
            "data_area",
            selected
          )
          mod_edit$ui(
            ns("data_area")
          )
        })
      }
    }

    shiny$observeEvent(
      c(selected$table_data, selected$operation),
      {
        observe_data_area_ui()
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    shiny$observeEvent(
      input$app_mode,
      {
        observe_selector_ui()
        if (input$app_mode != "view" && !is.null(input$app_mode)) {
          output$data_area_ui <- shiny$renderUI({
            mod_add$server(
              "data_area",
              selected
            )
            mod_add$ui(
              ns("data_area")
            )
          })
        }
      },
      ignoreNULL = FALSE
    )
  })
}
