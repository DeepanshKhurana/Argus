box::use(
  shiny,
)

box::use(
  # app/view/mod_add_selector,
  app/view/mod_add,
  app/view/mod_view,
  # app/view/mod_view_selector,
  app/view/mod_edit,
  app/view/mod_selector,
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
      app_mode = "view",
      table_name = NULL,
      row = NULL,
      operation = "viewing",
      table_data = NULL,
      user_input = NULL
    )

    shiny$observeEvent(input$app_mode, {
      selected$app_mode <- input$app_mode
    })

    shiny$observeEvent(selected$app_mode, {
        output$selector_ui <- shiny$renderUI({
          mod_selector$ui(
            ns("selector"),
            selected
          )
        })
        mod_selector$server(
          "selector",
          selected
        )
      }
    )
  })
}
