box::use(
  yaml[
    read_yaml
  ],
  dplyr[
    mutate,
    select
  ],
)

#' Get the list of tables updated in tables.yml
#' @export
get_table_list <- function() {
  tables <- read_yaml("tables.yml")
  list(
    "applications" = names(tables),
    "tables" = lapply(tables, function(x) unlist(strsplit(x, " ")))
  )
}

#' Process the API response
#' @export
process_table_data <- function(
  api_table_response
) {
  sapply(api_table_response, data.frame) |>
    t() |>
    data.frame() |>
    mutate(
      edit = "Edit",
      delete = "Delete"
    )
}
