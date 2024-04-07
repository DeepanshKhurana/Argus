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
    colDef
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
  app/logic/app_utils[process_table_data]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    reactable_extras_dependency(),
    reactableOutput(ns("selected_table_data"))
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

    # Edit Functionality ---

    observeEvent(input$cancel, {
      removeModal()
    })

    observeEvent(input$save, {

      id <- table_data()[input$edit$row, ]$id

      column_names <- names(
        table_data() |>
          select(-c(id, edit, delete))
      )

      update_data <- lapply(
        column_names,
        function(col_name) {
          input[[glue("edit_data-{col_name}")]]
        }
      )

      put_row(
        table_name = selected_table_name(),
        id |> unlist(),
        update_data |> unlist(),
        is_update = TRUE
      )

      removeModal()

    })

    observeEvent(input$edit, {

      row <- table_data()[input$edit$row, ] |>
        select(-c(edit, delete))

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
              names(row),
              function(col_name) {
                textInput(
                  inputId = ns(glue("edit_data-{col_name}")),
                  label = col_name,
                  value = row[[col_name]]
                )
              }
            )
           )
          )
        )
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # Delete Functionality ---

    observeEvent(input$delete, {

      delete_row(
        table_name = selected_table_name(),
        row_key = as.integer(table_data()[input$delete$row, ]$id)
      )

      table_data <- reactive({
        process_table_data(
          get_data(
            selected_table_name()
          )
        )
      })

      reactable::updateReactable(
        "selected_table_data",
        data = table_data()
      )

    }, ignoreNULL = TRUE, ignoreInit = TRUE)

  })
}
