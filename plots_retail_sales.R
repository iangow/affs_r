library(tidyverse)

retail_sales <- read_csv("data/retail_sales.csv",
                         show_col_types = FALSE)

retail_sales |>
  filter(state == "Total (State)") |>
  filter(between(date, as.Date("2010-01-01"), as.Date("2024-08-31"))) |>
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(industry ~ ., ncol = 3, scales = "free_y")

liquor <-
  retail_sales |>
  filter(state %in% c("Victoria", "Western Australia"),
         industry == "Liquor retailing") |>
  filter(between(date, as.Date("2018-01-01"), as.Date("2024-08-31"))) |>
  mutate(year = year(date),
         month = month(date, label = TRUE))

liquor |>
  filter(year == 2018) |>
  group_by(state) |>
  summarize(scale = mean(value)) |>
  inner_join(liquor, by = "state") |>
  mutate(value = value / scale) |>
  ggplot(aes(x = month, y = value, fill = state)) +
  geom_col(position = "dodge") +
  facet_wrap(year ~ ., ncol = 2) +
  theme(legend.position = "bottom") +
  ggtitle("Liquor retailing sales (normalized by 2018 average monthly sales)") +
  theme(plot.title.position = "plot")
