# Table Theming Script ----
#' @description
#' This script provides functions to create and theme tables using the `gt` package.
#' It includes options for customizing colors, footnotes, and other stylistic elements.
#' 
# eval_palette ----
#' @description
#' A helper function to evaluate color palettes using the `paletteer` package.
#' @param pal_name The name of the palette to evaluate.
#' @param n The number of colors to generate (default is 10).
#' @param pal_type The type of palette ("c" for continuous, "d" for discrete, or "dynamic" for dynamic palettes).
#' @param direction The direction of the palette (e.g., 1 for normal, -1 for reversed).
#' 
#' @return A vector of colors corresponding to the specified palette.
#' 
#' @example
#' \dontrun{
#' colors <- eval_palette("ggsci::springfield_simpsons", n = 5, pal_type = "d")
#' }
#' @export 
eval_palette <- function(pal_name, n = 10, pal_type, direction = NULL) {
     if (pal_type == "c") {
          return(paletteer_c(pal_name, n, direction))
     } else if (pal_type == "d") {
          return(paletteer_d(pal_name, n, direction))
     } else if (pal_type == "dynamic") {
          return(paletteer_dynamic(pal_name, n, direction))
     }
}

# r_table_theming ----
#' @description
#' The main function to create and theme a table using the `gt` package.
#' @details
#' **Color Coding** Applies color palettes to specific columns or the entire table.
#' **Footnotes** Adds footnotes to specific columns or locations in the table.
#' **Column Labels** Customizes the appearance of column labels, including background colors.
#' **Table Styling** Applies various styling options, such as borders, padding, and font weights.
#' **Shadow Effects** Optionally adds shadow effects to table body cells.
#'
#' @param r_df The data frame to be converted into a table.
#' @param title The title of the table.
#' @param subtitle The subtitle of the table.
#' @param footnotes_df A data frame containing footnotes and their locations.
#' @param source_note A source note to be added at the bottom of the table.
#' @param pal_df A data frame containing color palettes and columns to apply them to.
#' @param color_by_columns Columns to apply color to (default is NULL).
#' @param row_name_col The column to use as row names (default is NULL).
#' @param do_col_labels Whether to apply custom styling to column labels (default is FALSE).
#' @param target_everything Whether to apply color to all columns (default is FALSE).
#' @param doBodyShadows Whether to apply shadow effects to table body cells (default is FALSE).
#'
#' @return A themed `gt` table object.
#' 
#' @example 
#' \dontrun{
#'   data <- data.frame(
#'     Name = c("Alice", "Bob", "Charlie"),
#'     Score = c(85, 92, 78)
#'   )
#'   pal_df <- data.frame(
#'     cols = list("Score"),
#'     pals = list(eval_palette("ggsci::springfield_simpsons", n = 3, pal_type = "d"))
#'   )
#'   footnotes_df <- data.frame(
#'     notes = list("High score"),
#'     locations = list("Score")
#'   )
#'   themed_table <- r_table_theming(
#'     r_df = data,
#'     title = "Student Scores",
#'     subtitle = "Fall 2023",
#'     footnotes_df = footnotes_df,
#'     source_note = "Source: School Records",
#'     pal_df = pal_df,
#'     do_col_labels = TRUE
#'   )
#'   themed_table
#'  }
#'  
# r_table_theming ----
# Main function to create and theme a table using the `gt` package.
#' @export
r_table_theming <- function(r_df,
                            title,
                            subtitle,
                            footnotes_df,
                            source_note,
                            pal_df,
                            color_by_columns = NULL,
                            row_name_col = NULL,
                            do_col_labels = FALSE,
                            target_everything = FALSE,
                            doBodyShadows = FALSE,
                            footnotes_multiline = TRUE,
                            table_font_size = pct(100),
                            multiline_feet = TRUE
                            ) {
     # Initialize the gt table
     if(is.null(row_name_col)) {
          # If no row name column is specified, create a basic gt table
          r_table <- gt(r_df)
     } else {
          # If a row name column is specified, use it as the row names in the table
          r_table <- gt(r_df, rowname_col = row_name_col)
     }
     
     # Apply color coding to specific columns or the entire table
     if (nrow(r_df) > 1 && target_everything == FALSE) {
          # Apply color palettes to specific columns defined in pal_df
          r_table <- seq_len(nrow(pal_df)) |>
               reduce(\(acc, i) {
                    data_color(acc,
                               columns = pal_df$cols[[i]],  # Apply color to specified columns
                               palette = pal_df$pals[[i]]   # Use the specified palette
                    )
               }, .init = r_table)  # Start with the initial table and accumulate changes
     }
     else if (nrow(r_df) > 1 && target_everything == TRUE) {
          # Apply color palettes to all columns
          r_table <- seq_len(nrow(pal_df)) |>
               reduce(\(acc, i) {
                    data_color(
                         acc,
                         columns = color_by_columns,  # Apply color to specified columns
                         palette = pal_df$pals[[i]],  # Use the specified palette
                         target_columns = everything()  # Apply color to all columns
                    )
               }, .init = r_table)  # Start with the initial table and accumulate changes
     }
     
     # Add footnotes to the table
     r_table <- seq_len(nrow(footnotes_df)) |>
          reduce(\(acc, i) {
               tab_footnote(
                    acc,
                    footnote = footnotes_df$notes[[i]],  # Add the footnote text
                    location = cells_column_labels(
                         columns = footnotes_df$locations[[i]]),  # Specify the column for the footnote
                    placement = "auto"  # Automatically place the footnote
               )
          }, .init = r_table)  # Start with the initial table and accumulate changes
     
     # Apply custom styling to column labels (if enabled)
     if (ncol(r_df) > 1 && do_col_labels == TRUE) {
          cell_col_fills = pal_df$pals[[1]]  # Get the first palette for column labels
          # Apply background colors to column labels
          r_table <- seq_len(nrow(pal_df)) |>
               reduce(\(acc, i) {
                    tab_style(
                         acc,
                         style = cell_fill(color = cell_col_fills[i]),  # Fill column labels with color
                         locations = cells_column_labels(
                              columns = pal_df$cols[[i]])  # Apply to specified columns
                    )
               }, .init = r_table)  # Start with the initial table and accumulate changes
     }
     
     # Add a title and subtitle to the table
     r_table <- r_table |>
          tab_header(title = title, subtitle = subtitle)
     
     # Add a source note at the bottom of the table
     r_table <- r_table |>
          tab_source_note(source_note = source_note)
     
     # Apply general table styling options
     r_table <- r_table |>
          tab_options(
               column_labels.padding = px(10),  # Add padding to column labels
               column_labels.font.weight = "bold",  # Make column labels bold
               column_labels.background.color = '#333',  # Set background color for column labels
               column_labels.border.top.width = px(0),  # Remove top border for column labels
               column_labels.border.bottom.color = 'black',  # Set bottom border color for column labels
               column_labels.vlines.width = px(1),  # Set vertical line width for column labels
               column_labels.border.lr.width = px(1),  # Set left/right border width for column labels
               column_labels.border.bottom.width = px(0),  # Remove bottom border for column labels
               column_labels.border.lr.color = 'black',  # Set left/right border color for column labels
               column_labels.vlines.color = 'black',  # Set vertical line color for column labels
               footnotes.padding = px(5),  # Add padding to footnotes
               footnotes.background.color = '#222',  # Set background color for footnotes
               footnotes.sep = ", ",  # Set separator for footnotes
               footnotes.multiline = footnotes_multiline,  # Allow multiline footnotes (if enabled)
               heading.padding = px(10),  # Add padding to the heading
               heading.background.color = '#222',  # Set background color for the heading
               heading.title.font.size = pct(125),  # Set font size for the title
               heading.subtitle.font.size = pct(110),  # Set font size for the subtitle
               heading.border.bottom.width = px(0),  # Remove bottom border for the heading
               row.striping.include_table_body = TRUE,  # Enable row striping for the table body
               row.striping.include_stub = TRUE,  # Enable row striping for the stub
               row.striping.background_color = '#333',  # Set background color for striped rows
               row_group.as_column = TRUE,  # Display row groups as columns
               source_notes.background.color = '#222',  # Set background color for source notes
               stub.border.width = px(0),  # Remove border for the stub
               stub.font.weight = "bolder",  # Make stub text bolder
               table.margin.left = px(1),  # Set left margin for the table
               table.margin.right = px(1),  # Set right margin for the table
               table.align = "center",  # Center-align the table
               table.border.top.width = px(0),  # Remove top border for the table
               table.border.bottom.width = px(0),  # Remove bottom border for the table
               table.background.color = '#222',  # Set background color for the table
               table.font.size = table_font_size,  # Set font size for the table
               table.layout = "auto",  # Use automatic table layout
               table_body.hlines.color = 'black',  # Set horizontal line color for the table body
               table_body.hlines.width = px(0),  # Remove horizontal lines in the table body
               table_body.vlines.width = px(0),  # Remove vertical lines in the table body
               table_body.border.bottom.color = 'black',  # Set bottom border color for the table body
               table_body.border.top.color = 'black',  # Set top border color for the table body
               table_body.border.bottom.width = px(0),  # Remove bottom border for the table body
               table_body.border.top.width = px(0),  # Remove top border for the table body
          )
     
     return(r_table)
}
