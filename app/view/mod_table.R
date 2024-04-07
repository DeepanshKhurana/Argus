box::use(
  shiny[
    moduleServer,
    NS,
    div,
    req,
    observeEvent,
    showModal,
    removeModal,
    modalDialog,
    reactive,
    tagList,
    textInput,
    actionButton,
    icon
  ],
  reactable[
    reactable,
    renderReactable,
    reactableOutput,
    colDef,
    updateReactable
  ],
  reactable.extras[
    reactable_extras_dependency,
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
  ]
)

box::use(
  app/logic/api_utils[get_data, delete_row, put_row],
  app/logic/app_utils[process_table_data],
  app/view/mod_put
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    reactable_extras_dependency(),
    actionButton(
      ns("create"),
      label = "Create",
      icon = icon("plus")
    ),
    reactableOutput(
      ns("selected_table_data")
    )
  )

}

#' @export
server <- function(id, selected_table_name) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    req(selected_table_name())

    table_data <- reactive({
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

    # Create Functionality ---

    observeEvent(input$create, {

      mod_put$server(
        "create_modal",
        table_data = table_data() |>
          select(-c(edit, delete)),
        selected_table_name = selected_table_name,
        is_update = FALSE
      )

    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Edit Functionality ----

    observeEvent(input$edit, {

      row <- table_data()[input$edit$row, ] |>
        select(-c(edit, delete))

      mod_put$server(
        "edit_modal",
        table_data = row,
        selected_table_name = selected_table_name,
        is_update = TRUE
      )

    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Delete Functionality ---

    observeEvent(input$delete, {

      delete_row(
        table_name = selected_table_name(),
        row_key = as.integer(table_data()[input$delete$row, ]$id)
      )

      updateReactable(
        "selected_table_data",
        data = process_table_data(
          get_data(
            selected_table_name()
          )
        )
      )

    }, ignoreNULL = TRUE, ignoreInit = TRUE)

  })
}
