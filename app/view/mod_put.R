box::use(
  shiny[
    moduleServer,
    NS,
    tagList,
    showModal,
    modalDialog,
    renderUI,
    uiOutput,
    observeEvent,
    textInput,
    div,
    actionButton,
    icon,
    removeModal
  ],
  glue[
    glue
  ],
  dplyr[
    select
  ]
)

box::use(
  app/logic/api_utils[put_row]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
}

#' @export
server <- function(
  id,
  table_data,
  selected_table_name,
  is_update = FALSE
) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    showModal(
      modalDialog(
        title = "Selected row data",
        easyClose = TRUE,
        footer = div(
          actionButton(
            ns("cancel"),
            "Cancel",
            icon = icon("close")
          ),
          actionButton(
            ns("save"),
            "Save",
            icon = icon("save")
          )
        ),
        do.call(
          tagList,
          lapply(
            names(table_data[1, ]),
            function(col_name) {
              textInput(
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

    observeEvent(input$cancel, {
      removeModal()
    })

    # Save ----

    observeEvent(input$save, {

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

      removeModal()

    })

  })
}
