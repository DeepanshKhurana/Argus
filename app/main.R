box::use(
  shiny,
)

box::use(
  app/view/mod_add_selector,
  app/view/mod_view,
  app/view/mod_view_selector,
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
      operation = NULL,
      table_data = NULL
    )

    shiny$observeEvent(selected$table_data, {

      output$data_area_ui <- shiny$renderUI({

        mod_view$server(
          "data_area",
          selected
        )

        mod_view$ui(
          ns("data_area")
        )

      })

    })

    shiny$observeEvent(input$app_mode, {

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
    }, ignoreNULL = FALSE)

  })
}
