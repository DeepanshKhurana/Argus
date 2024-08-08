box::use(
  glue[
    glue
  ],
  dplyr[
    select
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
server <- function(id, app_state) {
  shiny$moduleServer(id, function(input, output, session) {

    ns <- session$ns

    shiny$observeEvent(app_state$selected_row_data(), {

      output$argus_data <- shiny$renderUI({

        keys <- names(app_state$selected_row_data())
        labels <- gsub(
          "_",
          " ",
          names(app_state$selected_row_data())
        ) |>
          toTitleCase()

        keys <- setNames(as.list(labels), keys)

        shiny$div(
          class = "argus-data-grid",
          lapply(
            names(keys),
            function(key) {
              if (app_state$operation() == "editing") {
                if (key == "id") {
                  class <- "argus-field-block id-block"
                  element <- shiny$p(
                    class = "argus-field-value",
                    app_state$selected_row_data()[key]
                  )
                } else {
                  class <- "argus-field-block"
                  element <- shiny$textInput(
                    inputId = ns(key),
                    label = NULL,
                    value = unlist(app_state$selected_row_data()[key])
                  ) |>
                    shiny$tagAppendAttributes(
                      class = "argus-field-input"
                    )
                }
              } else {
                class <- glue(
                  "argus-field-block {ifelse(key == 'id', 'id-block', '')}"
                )
                element <- shiny$p(
                  class = "argus-field-value",
                  app_state$selected_row_data()[key]
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
    })

    shiny$observe({
      app_state$user_inputs <- shiny$reactive({
        input_schema <- as.list(app_state$selected_row_data()[1, ])
        keys <- names(app_state$selected_row_data())
        input_schema <- c(
          input_schema$id,
          lapply(
            keys[-which(keys == "id")],
            function(key) {
              input_schema[[key]] <- input[[key]]
            }
          )
        )
        names(input_schema) <- keys
        input_schema[order(names(input_schema))]
      })
    })

  })
}
