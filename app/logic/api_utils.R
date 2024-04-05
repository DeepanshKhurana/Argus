box::use(
  httr2[
    request,
    req_url_query,
    req_method,
    req_headers,
    req_perform,
    resp_body_json
  ],
  glue[
    glue
  ]
)

#' Make an API endpoint URL
#' Constructs an API endpoint URL using the provided path components.
#' @param ... Character vectors specifying the path components of the endpoint URL.
#' @return A character string representing the constructed API endpoint URL.
make_endpoint <- function(...) {
  endpoint_path <- paste(c(...), collapse = "/")
  api_path <- Sys.getenv("API_PATH")
  if (nchar(api_path) > 1) {
    glue("{api_path}{endpoint_path}")
  } else {
    stop("API_PATH not set as an environment variable.")
  }
}

#' Get the API key
#' Retrieves the API key from the environment variable `API_KEY`.
#' @return A character string representing the API key.
get_api_key <- function(
) {
  api_key <- Sys.getenv("API_KEY")
  if (nchar(api_key) > 1) {
    api_key
  } else {
    stop("API_KEY not set as an environment variable.")
  }
}

#' Get the schema of a table
#' Retrieves the schema of a table from the API.
#' @param table_name A character string specifying the name of the table.
#' @return A JSON object representing the schema of the specified table.
#' @export
get_schema <- function(
  table_name = NULL
) {
  request(make_endpoint("schema")) |>
    req_headers(
      accept = "*/*",
      `X-API-KEY` = get_api_key(),
    ) |>
    req_url_query(
      "table_name" = table_name
    ) |>
    req_perform() |>
    resp_body_json()
}

#' Get data from a table
#' Retrieves data from a table in the API.
#' @param table_name A character string specifying the name of the table.
#' @param limit An integer specifying the maximum number of records to retrieve (optional).
#' @return A JSON object representing the retrieved data from the specified table.
#' @export
get_data <- function(
  table_name = NULL,
  limit = 0
) {
  request(make_endpoint("read")) |>
    req_headers(
      accept = "*/*",
      `X-API-KEY` = get_api_key(),
    ) |>
    req_url_query(
      "table_name" = table_name,
      "limit" = limit
    ) |>
    req_perform() |>
    resp_body_json()
}

#' Delete a row from a table
#' Deletes a row from a table in the API.
#' @param table_name A character string specifying the name of the table.
#' @param row_key A character string specifying the key of the row to delete.
#' @param show_old A logical value indicating whether to include the old row data in the response (optional, default is TRUE).
#' @return A JSON object representing the response from the API after deleting the row.
#' @export
delete_row <- function(
  table_name = NULL,
  row_key = NULL,
  show_old = TRUE
) {
  request(make_endpoint("delete")) |>
    req_headers(
      accept = "*/*",
      `X-API-KEY` = get_api_key(),
    ) |>
    req_method("DELETE") |>
    req_url_query(
      "table_name" = table_name,
      "row_key" = row_key,
      "show_old" = tolower(show_old)
    ) |>
    req_perform() |>
    resp_body_json()
}

#' Put a row into a table
#' Creates or updates a row in a table in the API.
#' @param table_name A character string specifying the name of the table.
#' @param ... Unnamed values representing the columns and values to put into the table.
#' @param show_old A logical value indicating whether to include the old row data in the response (optional, default is TRUE).
#' @param is_update A logical value indicating whether the operation is an update (optional, default is FALSE).
#' @return A JSON object representing the response from the API after putting the row.
#' @export
put_row <- function(
  table_name = NULL,
  ...,
  show_old = TRUE,
  is_update = FALSE
) {
  endpoint <- ifelse(is_update, "update", "create")
  request(make_endpoint(endpoint)) |>
    req_headers(
      accept = "*/*",
      `X-API-KEY` = get_api_key(),
    ) |>
    req_method("PUT") |>
    req_url_query(
      "table_name" = table_name,
      "input_list" = c(...),
      "show_old" = tolower(show_old),
      .multi = "explode"
    ) |>
    req_perform() |>
    resp_body_json()
}
