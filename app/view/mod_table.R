box::use(
  shiny,
  reactable[
    reactable,
    renderReactable,
    reactableOutput,
    colDef,
    updateReactable
  ],
  reactable.extras[
    button_extra
  ],
  dplyr[
    select,
    everything,
    mutate_all,
    mutate
  ],
  glue[
    glue
  ],
)

box::use(
  app/logic/api_utils[get_data, delete_row, put_row],
  app/logic/app_utils[process_table_data],
  app/view/mod_put,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    shiny$actionButton(
      ns("create"),
      label = "Create",
      icon = shiny$icon("plus")
    ),
    reactableOutput(
      ns("selected_table_data")
    )
  )

}

#' @export
server <- function(id, selected_table_name) {
  shiny$moduleServer(id, function(input, output, session) {

    ns <- session$ns

    shiny$req(selected_table_name())

    table_data <- shiny$reactive({
      process_table_data(get_data(selected_table_name()))
    })

    output$selected_table_data <- renderReactable({
      reactable(
        table_data(),
        columns = list(
          edit = colDef(
            cell = button_extra(
              ns("edit"),
              class = "btn btn-primary"
            )
          ),
          delete = colDef(
            cell = button_extra(
              ns("delete"),
              class = "btn btn-primary"
            )
          )
        )
      )
    })

    reload_table <- shiny$reactiveVal(FALSE)

    # Create Functionality ---

    shiny$observeEvent(input$create, {

      mod_put$server(
        "create_modal",
        table_data = table_data() |>
          select(-c(edit, delete)),
        selected_table_name = selected_table_name,
        is_update = FALSE,
        reload_table = reload_table
      )

    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Edit Functionality ----

    shiny$observeEvent(input$edit, {

      row <- table_data()[input$edit$row, ] |>
        select(-c(edit, delete))

      mod_put$server(
        "edit_modal",
        table_data = row,
        selected_table_name = selected_table_name,
        is_update = TRUE,
        reload_table = reload_table
      )

    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Delete Functionality ---

    shiny$observeEvent(input$delete, {

      delete_row(
        table_name = selected_table_name(),
        row_key = as.integer(table_data()[input$delete$row, ]$id)
      )

      reload_table(TRUE)

    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Reload Functionality ----

    shiny$observeEvent(reload_table(), {

      if (reload_table()) {

        table_data <- shiny$reactive({
          process_table_data(
            get_data(
              selected_table_name()
            )
          )
        })

        updateReactable(
          session = session,
          outputId = "selected_table_data",
          data = table_data()
        )

        reload_table(FALSE)

      }

    }, ignoreNULL = FALSE)

  })
}
