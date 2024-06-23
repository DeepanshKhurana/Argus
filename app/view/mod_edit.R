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
    uiOutput,
    tagAppendAttributes,
    textInput
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

    ns <- session$ns

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
              if (key == "id") {
                class <- "argus-field-block is-id-block"
                element <- p(
                  class = "argus-field-value",
                  selected$table_data()[key][selected$row, ]
                )
              } else {
                class <- "argus-field-block"
                element <- textInput(
                  inputId = ns(key),
                  label = NULL,
                  value = selected$table_data()[key][selected$row, ]
                ) |>
                  tagAppendAttributes(
                    class = "argus-field-input"
                  )
              }
              div(
                class = class,
                p(
                  class = "argus-field-heading",
                  keys[key]
                ),
                element
              )
            }
          )
        )
      })
    }, ignoreNULL = TRUE)

  })
}
