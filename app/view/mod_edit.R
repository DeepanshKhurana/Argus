box::use(
  glue[
    glue
  ],
  shiny,
  stats[
    setNames
  ],
  tools[
    toTitleCase
  ],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "argus-data-area",
    shiny$uiOutput(ns("argus_data"))
  )
}

#' @export
server <- function(id, selected) {
  shiny$moduleServer(id, function(input, output, session) {

    ns <- session$ns

    shiny$observeEvent(selected$table_data, {
      output$argus_data <- shiny$renderUI({

        keys <- names(selected$table_data())
        labels <- gsub(
          "_",
          " ",
          names(selected$table_data())
        ) |>
          toTitleCase()

        keys <- setNames(as.list(labels), keys)
        total <- length(keys)

        shiny$div(
          class = "argus-data-grid",
          lapply(
            names(keys),
            function(key) {
              if (key == "id") {
                class <- "argus-field-block is-id-block"
                element <- shiny$p(
                  class = "argus-field-value",
                  selected$table_data()[key][selected$row, ]
                )
              } else {
                class <- "argus-field-block"
                element <- shiny$textInput(
                  inputId = ns(key),
                  label = NULL,
                  value = selected$table_data()[key][selected$row, ]
                ) |>
                  shiny$tagAppendAttributes(
                    class = "argus-field-input"
                  )
              }
              shiny$div(
                class = class,
                shiny$p(
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
