do_duckdb <- function(df = tidy_df, use_wrapper = TRUE) {
  # duckdb_register(con, "duck_df",overwrite = TRUE, orig_df)
  if (use_wrapper) {
    if ("duckplyr_df" %in% class(df)) {
      # df was converted outside of function
      result <- df |> 
        do_dplyr()
      return(result)
    } else {
      # convert df to duck right now
      result <- as_duckplyr_df(df) |>
        do_dplyr()
      return(result)
    }
  } else {
    con <- dbConnect(duckdb::duckdb())
    # use global tidy_df so it doesn't matter what df was fed to function
    duckdb_register(con, "duck_df", overwrite = TRUE, df)
    # achieve the same result with dbGetQuery
    result <- dbGetQuery(
      con,
      "SELECT returnflag, linestatus, 
         sum(quantity) as sum_qty, 
         sum(extendedprice) as sum_base_price,
         sum(extendedprice*(1-discount)) as sum_disc_price, 
         sum(extendedprice*(1-discount)*(1+tax)) as sum_charge, 
         avg(quantity) as avg_qty, 
         avg(extendedprice) as avg_price, 
         avg(discount) as avg_disc, 
         count(*) as count_order FROM duck_df 
         GROUP BY returnflag, linestatus 
         ORDER BY returnflag, linestatus"
    )
    result
    dbDisconnect(con, shutdown = TRUE)
    rm(con)
  }
  return(result)
}
