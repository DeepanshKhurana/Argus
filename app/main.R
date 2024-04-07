box::use(
  shiny[
    fluidPage,
    div,
    h1,
    moduleServer,
    NS,
    renderUI,
    tags,
    uiOutput,
    p,
    selectInput,
    observeEvent,
    updateSelectInput,
    req,
    reactive,
    icon,
    reactiveVal
  ]
)

box::use(
  app/view/mod_selector,
  app/view/mod_table
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    class = "argus-body",
    div(
      class = "argus-header",
      h1(
        "Argus"
      ),
      mod_selector$ui(ns("selector"))
    ),
    div(
      class = "argus-table-area",
      mod_table$ui(
        ns("selected_table")
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    selected_table_name <- reactiveVal()

    mod_selector$server(
      "selector",
      selected_table_name
    )

    observeEvent(selected_table_name(), {
      mod_table$server(
        "selected_table",
        selected_table_name
      )
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

  })
}
