box::use(
  shiny[
    moduleServer,
    NS,
    div,
    req,
    observeEvent,
    showModal,
    modalDialog,
    reactive
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
  ]
)

box::use(
  app/logic/api_utils[get_data, delete_row],
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

    observeEvent(input$edit, {
      showModal(modalDialog(
        title = "Selected row data",
        reactable(table_data()[input$edit$row, ])
      ))
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

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
