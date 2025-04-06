gluedSql <- function(con,
                     group_cols,
                     transformation_col,
                     metric_col,
                     from_tbl) {
     # Define parameters
     group_cols <- c("year", "month")
     transformation_col <- "temperature_2m"
     metric_col <- "avg_temp"
     from_tbl <- "historical_data"
     
     # Create parameterized query with glue_sql
     query <- glue::glue_sql(
          "
SELECT
     {`group_cols`*}
     ,AVG({`transformation_col`}) AS {`metric_col`}
FROM {`from_tbl`}
GROUP BY {`group_cols`*}
ORDER BY {`group_cols`*}
",
          .con = con
     )
     return(dbGetQuery(con, query))
}
