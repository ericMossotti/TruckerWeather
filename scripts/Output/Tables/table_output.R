
# Table theming script

eval_palette <- function(pal_name, n = 10, pal_type, direction = NULL) {
     if (pal_type == "c") {
          return(paletteer_c(pal_name, n, direction))
     } else if (pal_type == "d") {
          return(paletteer_d(pal_name, n, direction))
     } else if (pal_type == "dynamic") {
          return(paletteer_dynamic(pal_name, n, direction))
     }
}

r_table_theming <- function(r_df,
                            title,
                            subtitle,
                            footnotes_df,
                            source_note,
                            pal_df,
                            multiline_feet = NULL,
                            tbl_font_size = NULL,
                            color_by_columns = NULL,
                            row_name_col = NULL,
                            do_col_labels = FALSE,
                            target_everything = FALSE
) {
     if(is.null(row_name_col)) {
          r_table <- gt(r_df)
          
     } else{
          r_table <- gt(r_df,
                        rowname_col = row_name_col,
                       )
     }
     
     r_table <- r_table |>
          tab_header(title = title, subtitle = subtitle)
     
     if (nrow(r_df) > 1 && target_everything == FALSE) {
          # Formatted rows are added to the accumulator, 
          # building up to the final result
          r_table <- seq_len(nrow(pal_df)) |>
               reduce(\(acc, i) {
                    data_color(acc,
                               palette = pal_df$pals[[i]],
                               columns = pal_df$cols[[i]])
               }, .init = r_table)
     }
     else if (nrow(r_df) > 1 && target_everything == TRUE) {
          # Formatted columns are added to the accumulator, 
          # building up to the final result
          r_table <- seq_len(nrow(pal_df)) |>
               reduce(\(acc, i) {
                    data_color(
                         acc,
                         columns = color_by_columns,
                         palette = pal_df$pals[[i]],
                         target_columns = everything()
                    )
               }, .init = r_table)
     }
     
     # Footnotes are added to the accumulator, building up to the final result
     r_table <- seq_len(nrow(footnotes_df)) |>
          reduce(\(acc, i) {
               tab_footnote(
                    acc,
                    footnote = footnotes_df$notes[[i]],
                    location = cells_column_labels(
                         columns = footnotes_df$locations[[i]]),
                    placement = "auto"
               )
          }, .init = r_table)
     
     if (ncol(r_df) > 1 && do_col_labels == TRUE) {
          cell_col_fills = pal_df$pals[[1]]
          # Formatted column labels are added to the accumulator, 
          # building up to the final result
          r_table <- seq_len(nrow(pal_df)) |>
               reduce(\(acc, i) {
                    tab_style(
                         acc,
                         style = cell_fill(color = cell_col_fills[i]),
                         locations = cells_column_labels(
                              columns = pal_df$cols[[i]])
                    )
               }, .init = r_table)
     }
     
     r_table <- r_table |>
          tab_source_note(source_note = source_note)
     
     r_table <- r_table |>
          tab_options(
               column_labels.padding = px(10),
               column_labels.font.weight = "bold",
               column_labels.background.color = '#333',
               column_labels.border.top.width = px(0),
               column_labels.border.bottom.color = 'black',
               column_labels.vlines.width = px(1),
               column_labels.border.lr.width = px(1),
               column_labels.border.bottom.width = px(0),
               column_labels.border.lr.color = 'black',
               column_labels.vlines.color = 'black',
               footnotes.padding = px(5),
               footnotes.background.color = '#222',
               footnotes.sep = ", ",
               footnotes.multiline = multiline_feet,
               heading.padding = px(10),
               heading.background.color = '#222',
               heading.title.font.size = pct(125),
               heading.subtitle.font.size = pct(110),
               heading.border.bottom.width = px(0),
               row.striping.include_table_body = TRUE,
               row.striping.include_stub = TRUE,
               row.striping.background_color = '#333',
               row_group.as_column = TRUE,
               source_notes.background.color = '#222',
               stub.border.width = px(0),
               stub.font.weight = "bolder",
               table.margin.left = px(1),
               table.margin.right = px(1),
               table.align = "center",
               table.border.top.width = px(0),
               table.border.bottom.width = px(0),
               table.background.color = '#222',
               table.font.size = tbl_font_size,
               table.layout = "auto",
               table_body.hlines.color = 'black',
               table_body.hlines.width = px(0),
               table_body.vlines.width = px(0),
               table_body.border.bottom.color = 'black',
               table_body.border.top.color = 'black',
               table_body.border.bottom.width = px(0),
               table_body.border.top.width = px(0)
          )
     
     
 
     
     
     return(r_table)
}
