box::use(
  shiny,
  reactable.extras[
    reactable_extras_dependency
  ]
)

box::use(
  app/view/mod_selector,
  app/view/mod_table,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$fluidPage(
    reactable_extras_dependency(),
    class = "argus-body",
    shiny$div(
      class = "argus-header",
      shiny$h1(
        "Argus"
      ),
      mod_selector$ui(ns("selector"))
    ),
    shiny$div(
      class = "argus-table-area",
      mod_table$ui(
        ns("selected_table")
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {

    ns <- session$ns

    selected_table_name <- shiny$reactiveVal()

    mod_selector$server(
      "selector",
      selected_table_name
    )

    shiny$observeEvent(selected_table_name(), {
      mod_table$server(
        "selected_table",
        selected_table_name
      )
    }, ignoreNULL = TRUE, once = TRUE)

  })
}
