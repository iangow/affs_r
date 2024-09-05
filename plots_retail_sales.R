library(tidyverse)

system_time <- function (expr) {
  print(system.time(expr))
  expr
}

retail_sales <-
  read_csv("data/retail_sales.csv", show_col_types = FALSE) |>
  system_time()

retail_sales |>
  filter(region == "Australia") |>
  filter(between(time_period, as.Date("2010-01-01"), as.Date("2024-08-31"))) |>
  ggplot(aes(x = time_period, y = obs_value)) +
  geom_line() +
  facet_wrap(industry ~ ., ncol = 3, scales = "free_y")

liquor <-
  retail_sales |>
  filter(region %in% c("Victoria", "Western Australia"),
         industry == "Liquor retailing") |>
  filter(between(time_period, as.Date("2018-01-01"), as.Date("2024-08-31"))) |>
  mutate(year = year(time_period),
         month = month(time_period, label = TRUE))

liquor |>
  filter(year == 2018) |>
  group_by(region) |>
  summarize(scale = mean(obs_value)) |>
  inner_join(liquor, by = "region") |>
  mutate(obs_value = obs_value / scale) |>
  ggplot(aes(x = month, y = obs_value, fill = region)) +
  geom_col(position = "dodge") +
  facet_wrap(year ~ ., ncol = 2) +
  theme(legend.position = "bottom") +
  ggtitle("Liquor retailing sales (normalized by 2018 average monthly sales)") +
  theme(plot.title.position = "plot")
