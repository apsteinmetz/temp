# benchmark duckdb
library(tidyverse)
library(chattr)
library(duckdb)
library(duckplyr)
library(microbenchmark)

chattr_use("copilot")
chattr_test()

lineitem <- as_tibble(duckdb:::sql("INSTALL tpch; LOAD tpch; CALL dbgen(sf=1); FROM lineitem;"))
lineitem_duck <- as_duckplyr_df(lineitem)

tpch_01 <- function(.data) {
  .data |>
    select(l_shipdate, l_returnflag, l_linestatus, l_quantity, l_extendedprice, l_discount, l_tax) |>
    filter(l_shipdate <= !!as.Date("1998-09-02")) |>
    select(l_returnflag, l_linestatus, l_quantity, l_extendedprice, l_discount, l_tax) |>
    summarise(
      sum_qty = sum(l_quantity),
      sum_base_price = sum(l_extendedprice),
      sum_disc_price = sum(l_extendedprice * (1 - l_discount)),
      sum_charge = sum(l_extendedprice * (1 - l_discount) * (1 + l_tax)),
      avg_qty = mean(l_quantity),
      avg_price = mean(l_extendedprice),
      avg_disc = mean(l_discount),
      count_order = n(),
      .by = c(l_returnflag, l_linestatus)
    ) |>
    arrange(l_returnflag, l_linestatus)
}

microbenchmark(tpch_01(lineitem),
               tpch_01(lineitem_duck),
               times = 1)

