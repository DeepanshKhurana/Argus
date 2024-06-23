box::use(
  glue[
    glue
  ],
  shiny[
    div,
    moduleServer,
    NS,
    observeEvent,
    p,
    renderUI,
    uiOutput
  ],
  stats[
    setNames
  ],
  tools[
    toTitleCase
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "argus-data-area",
    uiOutput(ns("argus_data"))
  )
}

#' @export
server <- function(id, selected) {
  moduleServer(id, function(input, output, session) {

    observeEvent(selected$table_data, {
      output$argus_data <- renderUI({

        keys <- names(selected$table_data())
        labels <- gsub(
          "_",
          " ",
          names(selected$table_data())
        ) |>
          toTitleCase()

        keys <- setNames(as.list(labels), keys)
        total <- length(keys)

        div(
          class = "argus-data-grid",
          lapply(
            names(keys),
            function(key) {
              div(
                class = glue(
                  "argus-field-block {ifelse(key == 'id', 'is-id-block', '')}"
                ),
                p(
                  class = "argus-field-heading",
                  keys[key]
                ),
                p(
                  class = "argus-field-value",
                  selected$table_data()[key][selected$row, ]
                )
              )
            }
          )
        )
      })
    }, ignoreNULL = TRUE)

  })
}
