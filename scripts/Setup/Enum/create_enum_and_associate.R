#' Create ENUM Type and Associate Codes with Descriptions
#'
#' This function creates an ENUM type in DuckDB and associates codes with their descriptions.
#' It can be used to create other ENUM types and associations
#'
#' @param duckdb_conn A DuckDB connection object.
#' @param enum_name A string specifying the name of the ENUM type to be created.
#' @param table_name A string specifying the name of the ENUM dictionary table.
#' @param codes A character vector of codes to be included in the ENUM type.
#' @param descriptions A character vector of descriptions corresponding to the codes.
#' @example
#' \dontrun{
#' library(DBI)
#' 
#' codes <- c('0', '1', '2', '3', '45', '48', '51', '53', '55', '56', '57', 
#'            '61', '63', '65', '66', '67', '71', '73', '75', '77', '80', '81', 
#'            '82', '85', '86', '95', '96', '99')
#' descriptions <- c('Clear sky', 'Mainly clear', 'Partly cloudy', 'Overcast', 
#'                   'Fog', 'Depositing rime fog', 'Drizzle: Light', 'Drizzle: Moderate', 
#'                   'Drizzle: Dense', 'Freezing Drizzle: Light', 'Freezing Drizzle: Dense', 
#'                   'Rain: Slight', 'Rain: Moderate', 'Rain: Heavy', 'Freezing Rain: Light', 
#'                   'Freezing Rain: Heavy', 'Snow fall: Slight', 'Snow fall: Moderate', 
#'                   'Snow fall: Heavy', 'Snow grains', 'Rain showers: Slight', 
#'                   'Rain showers: Moderate', 'Rain showers: Violent', 'Snow showers: Slight', 
#'                   'Snow showers: Heavy', 'Thunderstorm: Slight or moderate', 
#'                   'Thunderstorm with slight hail', 'Thunderstorm with heavy hail')
#' 
#' result <- create_enum_and_associate(duckdb_con, "WeatherCode", codes, descriptions)
#' print(result)
#' }
#' @export
create_enum_and_associate <- function(duckdb_con, enum_name, table_name, code_frame) {
     
     # Attempt to drop the ENUM type if it exists
     drop_query <- paste0("DROP TYPE IF EXISTS ", enum_name, ";")
     
     tryCatch({
          dbExecute(duckdb_con, drop_query)
          message(paste("Dropped existing ENUM type:", enum_name))
     }, error = \(e) {
          message(paste0("No existing ENUM type to drop: ", enum_name))
     })
     
     # Create the ENUM type
     enum_query <- paste0(
          "CREATE TYPE ", enum_name, " AS ENUM (",
          paste0(
               "'", code_frame$code, "'", collapse = ", "), ");"
          )
     
     dbExecute(duckdb_con, enum_query)
     message(paste0("Created ENUM type: ", enum_name))
     
     # Write an association table for reference
     dbWriteTable(
          duckdb_con,
          table_name,
          code_frame,
          overwrite = TRUE
     )
}
