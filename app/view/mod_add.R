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

    empty_dataframe <- shiny$reactive({
      data.frame(
        app_state$table_data()[0, ]
      )
    })

    max_id <- shiny$reactive({
      app_state$table_data()$id |>
        unlist() |>
        max()
    })

    shiny$observeEvent(app_state$table_data, {

      output$argus_data <- shiny$renderUI({

        keys <- names(empty_dataframe())
        labels <- gsub(
          "_",
          " ",
          names(empty_dataframe())
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
                class <- "argus-field-block id-block"
                element <- shiny$p(
                  class = "argus-field-value",
                  max_id() + 1
                )
              } else {
                class <- "argus-field-block"
                element <- shiny$textInput(
                  inputId = ns(key),
                  label = NULL,
                  value = ""
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

    shiny$observe({
      app_state$user_inputs <- shiny$reactive({
        input_schema <- as.list(
          app_state$selected_row_data()[1, ] |>
            select(-id)
        )
        keys <- names(input_schema)
        input_schema <- c(
          lapply(
            keys,
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
