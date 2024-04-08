box::use(
  shiny,
  glue[
    glue
  ],
  dplyr[
    select
  ],
  reactable[
    updateReactable
  ],
)

box::use(
  app/logic/api_utils[get_data, put_row],
  app/logic/app_utils[process_table_data],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
}

#' @export
server <- function(
  id,
  table_data,
  selected_table_name,
  is_update = FALSE,
  reload_table = FALSE
) {
  shiny$moduleServer(id, function(input, output, session) {

    ns <- session$ns

    shiny$showModal(
      shiny$modalDialog(
        title = "Selected row data",
        easyClose = TRUE,
        footer = shiny$div(
          shiny$actionButton(
            ns("cancel"),
            "Cancel",
            icon = shiny$icon("close")
          ),
          shiny$actionButton(
            ns("save"),
            "Save",
            icon = shiny$icon("save")
          )
        ),
        do.call(
          shiny$tagList,
          lapply(
            names(table_data[1, ]),
            function(col_name) {
              shiny$textInput(
                inputId = ns(glue("data-{col_name}")),
                label = col_name,
                value = ifelse(
                  is_update,
                  table_data[[col_name]],
                  ""
                )
              )
            }
          )
        )
      )
    )

    # Cancel ---

    shiny$observeEvent(input$cancel, {
      shiny$removeModal()
    })

    # Save ----

    shiny$observeEvent(input$save, {

      column_names <- names(
        table_data
      )

      if (is_update) {
        column_names <- column_names
      } else {
        column_names <- column_names[column_names != "id"]
      }

      update_data <- lapply(
        column_names,
        function(col_name) {
          input[[glue("data-{col_name}")]]
        }
      )

      put_row(
        table_name = selected_table_name(),
        update_data |> unlist(),
        is_update = is_update
      )

      reload_table(TRUE)

      shiny$removeModal()

    })

  })
}
