library(tidyverse)
library(duckdb)

system_time <- function (expr) {
  print(system.time(expr))
  expr
}

db <- dbConnect(duckdb())

fix_names <- function(x) {
  str_replace(x, ":.*$", "") |>
    str_to_lower()
}

csv_file <- "~/Downloads/ABS_RT_1.0.0.csv"

retail_sales <-
  tbl_file(db, csv_file) |>
  rename_with(fix_names) |>
  filter(unit_measure == "AUD: Australian Dollars",
         measure == "M1: Current Prices",
         tsest == "10: Original",
         freq == "M: Monthly",
         unit_mult == "6: Millions") |>
  select(-unit_measure, -measure, -tsest, -freq, -unit_mult,
         -obs_comment, -dataflow) |>
  mutate(time_period = as.Date(str_c(time_period, "-01"))) |>
  collect() |>
  separate_wider_regex(region,
                       c(region_code = ".*", ":\\s+", region = ".*")) |>
  separate_wider_regex(industry,
                       c(industry_code = ".*", ":\\s+", industry = ".*")) |>
  select(-region_code, -industry_code) |>
  distinct() |>
  arrange(time_period, industry, region) |>
  select(-obs_status) |>
  write_csv("data/retail_sales.csv") |>
  system_time()

library(readabs)

Sys.setenv(R_READABS_PATH = "~/Downloads/")

retail_sales <-
  read_abs("8501.0") |>
  filter(series_type == "Original") |>
  filter(data_type == "FLOW",
         table_title == "TABLE 11. Retail Turnover, State by Industry Subgroup, Original") |>
  separate_wider_regex(series,
                       c("^.*;\\s+", region = ".*?", "\\s+;\\s+", industry = ".*?", "\\s+;$")) |>
  rename(obs_value = value,
         time_period = date) |>
  mutate(obs_value = round(obs_value, 1)) |>
  select(industry, region, time_period, obs_value) |>
  mutate(region = as.character(region)) |>
  mutate(
    industry = if_else(industry == "Total (Industry)", "Total", industry),
    region = if_else(region == "Total (State)", "Australia", region))|>
  distinct() |>
  arrange(time_period, industry, region) |>
  write_csv("data/retail_sales.csv") |>
  system_time()
