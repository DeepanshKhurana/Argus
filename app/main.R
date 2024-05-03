box::use(
  shiny,
)

box::use(
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
      shiny$p(
        "Argus"
      )
    ),
    mod_selector$ui(ns("selector")),
    shiny$div(
      class = "argus-data-area"
    )
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

    mod_selector$server(
      "selector",
      selected
    )

    shiny$observeEvent(selected$table_data, {
      # Code Here
    })

    shiny$observeEvent(input$app_mode, {
      # Code Here
    })

  })
}
