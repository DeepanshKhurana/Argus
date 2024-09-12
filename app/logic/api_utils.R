box::use(
  dplyr[
    select,
    filter
  ],
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
  ],
  methods[
    as
  ],
  purrr[
    map2
  ],
  checkmate[
    assert,
    check_class
  ],
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
#' @return A JSON object for the response from the API after deleting the row.
#' @export
delete_row <- function(
  table_name = NULL,
  row_key = NULL
) {
  request(make_endpoint("delete")) |>
    req_headers(
      accept = "*/*",
      `X-API-KEY` = get_api_key(),
    ) |>
    req_method("DELETE") |>
    req_url_query(
      "table_name" = table_name,
      "row_key" = row_key
    ) |>
    req_perform() |>
    resp_body_json()
}

#' Map SQL data types to R data types
#'
#' @param data_type SQL data type as a character string.
#' @param type_mapping A named list mapping SQL data types to R data types.
#' @return Corresponding R data type as a character string.
map_sql_to_r <- function(
  data_type,
  type_mapping = list(
    "bigint" = "numeric",
    "text" = "character",
    "double precision" = "numeric",
    "date" = "Date",
    "timestamp with time zone" = "POSIXct",
    "boolean" = "logical"
  )
) {
  matched_type <- type_mapping[[data_type]]
  if (is.null(matched_type)) {
    stop(glue("Unrecognized data type: {data_type}"))
  }
  matched_type
}

#' Coerce a string value to the expected R type
#'
#' @param value The value to be coerced (usually a string).
#' @param expected_type A string representing the expected R type
#'
#' @return The value coerced to the expected type.
coerce_to_type <- function(
  value,
  expected_type
) {
  switch(
    expected_type,
    "integer" = as.integer(value),
    "numeric" = as.numeric(value),
    "logical" = as.logical(value),
    "character" = as.character(value),
    "Date" = as.Date(value, format = "%Y-%m-%d"),
    "POSIXct" = as.POSIXct(value),
    value
  )
}

#' Validate input values against schema information
#'
#' @param input_list A list of input values to validate.
#' @param table_schema A data frame containing schema information.
#' @return NULL. Throws an error if validation fails.
validate_input_types <- function(
  input_list,
  table_schema
) {
  column_names <- table_schema$column_name[seq_along(input_list)]
  map2(
    column_names,
    input_list,
    ~ {
      expected_type <- map_sql_to_r(
        table_schema$data_type[table_schema$column_name == .x] |>
          unlist()
      )
      coerced_value <- coerce_to_type(
        .y,
        expected_type
      )
      assert(
        check_class(
          coerced_value,
          expected_type
        )
      )
    }
  )
}

#' Put a row into a table
#' Creates or updates a row in a table in the API.
#' @param table_name A character string specifying the name of the table.
#' @param ... Unnamed values for the columns and values to put into the table.
#' @param is_update A logical value indicating whether to update
#' @return A JSON object for the response from the API after putting the row.
#' @export
put_row <- function(
  table_name = NULL,
  ...,
  is_update = FALSE
) {

  table_schema <- sapply(
    get_schema(table_name),
    data.frame
  ) |>
    t() |>
    data.frame() |>
    dplyr::filter(
      column_name != "created_at"
    )

  table_columns <- table_schema$column_name |>
    unlist()

  if (is_update) {
    endpoint <- "update"
    input_list <- c(...)[table_columns]
  } else {
    endpoint <- "create"
    input_list <- c(...)[table_columns[table_columns != "id"]]
  }

  validate_input_types(
    input_list,
    table_schema
  )

  request(make_endpoint(endpoint)) |>
    req_headers(
      accept = "*/*",
      `X-API-KEY` = get_api_key(),
    ) |>
    req_method("PUT") |>
    req_url_query(
      "table_name" = table_name,
      "input_list" = c(
        input_list |> unlist()
      ),
      .multi = "explode"
    ) |>
    req_perform() |>
    resp_body_json()
}
