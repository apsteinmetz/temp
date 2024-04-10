library(tidyverse)
cars <- as_tibble(mtcars,rownames = "model")

t_tibble <- function(df, first_col_name = "variable") {
  if (first_col_name %in% names(df)) {
    stop(
      paste0( 'The column name "',
        first_col_name,
        '" already exists in the data frame. Choose a different name.'
      )
    )
  }
  if (df[,-1] |> sapply(class) |> unique() |> length() >1){
    cat("WARNING in t_tibble: Mixed column types. Converting all to character\n")
    pivot_longer(df,
                 cols = -1,
                 names_to = first_col_name,
                 values_to = "value",
                 values_transform = as.character) %>%
      pivot_wider(names_from = names(df)[1],
                  values_from = "value")

  } else {
    pivot_longer(df,
                 cols = -1,
                 names_to = first_col_name,
                 values_to = "value") %>%
      pivot_wider(names_from = names(df)[1],
                  values_from = "value")

  }
}



cars |> t_tibble()

# transpose the data and change the name of the first column
cars |>  t_tibble() |> t_tibble("model")


# now do in base R
t_tibble2 <- function(df, first_col_name = "variable") {
  if (first_col_name %in% names(df)) {
    stop(
      paste0( 'The column name "',
        first_col_name,
        '" already exists in the data frame. Choose a different name.'
      )
    )
  }
  if (df[,-1] |> sapply(class) |> unique() |> length() >1){
    cat("WARNING in t_tibble: Mixed column types. Converting all to character\n")
  }
  #extract first column as a vector
  col_names = c(df[,1])[[1]]
  return(df[,-1] |> t() |>
           as_tibble(rownames = first_col_name) |>
           setNames(c(first_col_name, col_names)))
}

mydf <- cars
setNames(data.frame(t(mydf[,-1])), c("spec",mydf[,1]))
mydf[,-1] |> t() |> as_tibble(rownames = "spec")

cars2 <- cars |> mutate(alpha = "junk")
microbenchmark::microbenchmark(t_tibble(cars2),
                               t_tibble2(cars2),
                               times = 100)


