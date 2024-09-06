library(httr2)         # request(), req_*(), resp_body_html()
library(rvest)         # html_elements(), html_table()
library(tidyverse)

extract_team_ids <- function(link_text) {
  matches <- str_match(link_text, '<a href="teams/(.*)_idx.html">(.*)</a>')
  team_id <- matches[, 2]
  team_name <- matches[, 3]
  tibble(team_id, team_name)
}

all_teams_url <- "https://afltables.com/afl/afl_index.html"

resp <-
  request(all_teams_url) |>
  req_user_agent(getOption("HTTPUserAgent")) |>
  req_perform() |>
  resp_body_html() |>
  html_elements("body")

team_urls <-
  resp |>
  html_elements("a") |>
  as.character(x = _) |>
  as_tibble() |>
  filter(str_detect(value, "teams"))

teams <-
  map_dfr(team_urls, extract_team_ids) |>
  filter(team_id != "allteams")

get_team_list <- function(team_id) {
  url <- str_c("https://afltables.com/afl/stats/alltime/",
               team_id, ".html")

  resp <-
    request(url) |>
    req_user_agent(getOption("HTTPUserAgent")) |>
    req_perform() |>
    resp_body_html() |>
    html_elements("body")

  make_range <- function(start, end) {
    start
  }

  df <-
    resp[[1]] |>
    html_table() |>
    mutate(DOB = as.Date(DOB)) |>
    filter(!is.na(DOB)) |>
    separate_wider_regex(cols = c(Debut, Last),
                         patterns = c(years = "^[0-9]+", "y ",
                                      days = "[0-9]+", "d$"),
                         names_sep = "_",
                         too_few = "align_start") |>
    mutate(debut = if_else(is.na(Debut_days),
                           DOB + years(Debut_years),
                           if_else(month(DOB) == 2 & day(DOB) == 29,
                                   DOB + days(Debut_days) + years(Debut_years),
                                   DOB + years(Debut_years) + days(Debut_days))),
           last = if_else(is.na(Last_days),
                          DOB + years(Last_years),
                          if_else(month(DOB) == 2 & day(DOB) == 29,
                                  DOB + days(Last_days) + years(Last_years),
                                  DOB + years(Last_years) + days(Last_days)))) |>
    arrange(desc(last)) |>
    select(-matches("^(Debut|Last)", ignore.case = FALSE)) |>
    mutate(team_id = team_id, .before = 1)
  df
}

team_lists <-
  map(teams$team_id, get_team_list) |>
  list_rbind()

team_debut_days <-
  team_lists |>
  mutate(year = year(debut)) |>
  group_by(team_id, year) |>
  summarize(team_debut = min(debut), .groups = "drop")

team_last_days <-
  team_lists |>
  mutate(year = year(last)) |>
  group_by(team_id, year) |>
  summarize(team_last = max(last), .groups = "drop")

team_days <-
  team_debut_days |>
  full_join(team_last_days, by = join_by(team_id, year))

player_features <-
  team_lists |>
  mutate(weight = parse_number(str_remove(WT, "kg$")),
         height = parse_number(str_remove(HT, "cm$"))) |>
  distinct(Player, DOB, height, weight)

player_team_stats <-
  team_lists |>
  select(team_id, Player, DOB, Cap, `#`, `Games (W-D-L)`, Goals) |>
  separate_wider_regex(`Games (W-D-L)`,
                       c(games = "^[0-9]+", " \\(",
                         won = "[0-9]+", "-",
                         drawn = "[0-9]+", "-",
                         lost = "[0-9]+", "\\)")) |>
  rename(number = `#`,
         cap = Cap,
         goals = Goals) |>
  mutate(across(cap:goals, parse_number))

player_team_seasons <-
  team_lists |>
  select(team_id, Player, DOB, Seasons) |>
  separate_longer_delim(Seasons, ",") |>
  mutate(Seasons = str_trim(Seasons)) |>
  separate_wider_regex(Seasons,
                       patterns = c(first_season = "^[0-9]+", "-",
                                    last_season = "[0-9]+$"),
                       too_few = "align_start") |>
  mutate(last_season = coalesce(last_season, first_season)) |>
  rowwise() |>
  mutate(seasons = list(first_season:last_season)) |>
  unnest(seasons) |>
  select(-first_season, -last_season)

player_teams_manual <-
  tribble(
    ~Player, ~DOB, ~team_id, ~debut, ~last,
    "Leahy, Pat", "1917-09-02", "geelong", "1939-06-24", "1939-08-05",
    "Leahy, Pat", "1917-09-02", "swans", "1943-05-22", "1943-08-14",
    "Leahy, Pat", "1917-09-02", "geelong", "1944-05-06", "1944-08-26",
    "Leahy, Pat", "1917-09-02", "swans", "1945-06-02", "1945-06-02",
    "Ditterich, Carl", "1945-10-10", "stkilda", "1963-04-20",  "1972-09-30",
    "Ditterich, Carl", "1945-10-10", "melbourne", "1973-04-07", "1975-08-30",
    "Ditterich, Carl", "1945-10-10", "stkilda",  "1976-04-03",  "1978-09-02",
    "Ditterich, Carl", "1945-10-10", "melbourne", "1979-04-07", "1980-08-30",
    "Harwood, Bill", "1920-09-15", "geelong", "1940-05-04", "1941-08-30",
    "Harwood, Bill", "1920-09-15", "swans", "1942-05-09", "1942-09-12",
    "Harwood, Bill", "1920-09-15", "geelong", "1946-04-27", "1948-07-31",
    "Southern, Wally", "1921-10-26", "geelong", "1941-06-14", "1941-06-14",
    "Southern, Wally", "1921-10-26", "swans", "1943-05-15", "1943-05-15",
    "Southern, Wally", "1921-10-26", "geelong", "1944-05-06", "1945-09-01",
    "Southern, Wally", "1921-10-26", "skilda", "1946-07-13", "1946-08-10",
    "Toyne, Len", "1922-07-12", "geelong", "1940-06-15", "1941-08-16",
    "Toyne, Len", "1922-07-12","fitzroy", "1942-05-09", "1942-08-08",
    "Toyne, Len", "1922-07-12", "geelong",  "1945-04-28", "1945-09-01",
    "Toyne, Len", "1922-07-12", "melbourne", "1949-05-28", "1949-08-27",
    "Tucker, Neil", "1915-05-09", "geelong", "1935-06-22", "1941-08-30",
    "Tucker, Neil", "1915-05-09", "carlton", "1943-06-12", "1943-06-19",
    "Tucker, Neil", "1915-05-09", "geelong", "1944-05-06", "1944-09-02") |>
  mutate(across(c(DOB, debut, last), as.Date))

player_teams <-
  team_lists |>
  select(team_id, Player, DOB, debut, last) |>
  anti_join(player_teams_manual, by = join_by(Player, DOB)) |>
  union_all(player_teams_manual)

overlaps <-
  player_teams |>
  inner_join(player_teams,
             by = join_by(Player, DOB,
                          overlaps(x$debut, x$last, y$debut , y$last)),
             relationship = "many-to-many") |>
  filter(team_id.x != team_id.y)

first_stints <-
  overlaps |>
  filter(debut.x < debut.y,
         last.x > last.y) |>
  mutate(year = year(debut.y) - 1) |>
  inner_join(team_days, by = join_by(team_id.x == team_id, year)) |>
  mutate(last.x = team_last) |>
  select(team_id.x:last.x) |>
  rename_with(\(x) str_replace(x, "\\.x$", "")) |>
  group_by(team_id, Player, DOB, debut) |>
  summarize(last = min(last), .groups = "drop")

second_stints <-
  overlaps |>
  filter(debut.x < debut.y,
         last.x > last.y) |>
  mutate(year = year(last.y) + 1) |>
  inner_join(team_days, by = join_by(team_id.x == team_id, year)) |>
  mutate(debut.x = team_debut) |>
  select(team_id.x:last.x) |>
  rename_with(\(x) str_replace(x, "\\.x$", "")) |>
  group_by(team_id, Player, DOB, last) |>
  summarize(debut = max(debut), .groups = "drop")

bracket_stints <-
  second_stints |>
  union_all(first_stints) |>
  group_by(Player) |>
  filter(n() > 1) |>
  ungroup()

player_teams_fixed <-
  player_teams |>
  anti_join(bracket_stints, by = join_by(team_id, Player, DOB)) |>
  union_all(bracket_stints)

overlaps_fixed <-
  player_teams_fixed |>
  inner_join(player_teams_fixed,
             by = join_by(Player, DOB,
                          overlaps(x$debut, x$last, y$debut , y$last)),
             relationship = "many-to-many") |>
  filter(team_id.x != team_id.y)

overlaps_fixed |>
  arrange(Player, debut.x)

teams |> write_csv("data/teams.csv")
player_team_seasons |> write_csv("data/player_team_seasons.csv")
player_teams |> write_csv("data/player_teams.csv")
player_team_stats |> write_csv("data/player_team_stats.csv")
player_features |> write_csv("data/player_features.csv")

