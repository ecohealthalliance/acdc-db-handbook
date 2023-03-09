

create_data_dictionary <- function(metadata, table) {
  table_details <- metadata |>
    dplyr::filter(`Table Name` == table) |>
    dplyr::select(`Table ID`, `Table Name`, `Table Desc`) |>
    dplyr::summarise(
      `Table ID` = unique(`Table ID`),
      `Table Name` = unique(`Table Name`),
      `Table Description` = unique(`Table Desc`)
    )
  
  field_details <- metadata |>
    dplyr::filter(`Table Name` == table) |>
    dplyr::select(`Field Name`, `Field Desc`, `Field Type`, `Field Opt Name`) |>
    dplyr::rename(
      `Field Description` = `Field Desc`,
      `Field Values` = `Field Opt Name`
    )
  
  list(table_details, field_details) |>
    (\(x) { names(x) <- c("table_details", "field_details"); x })()
}