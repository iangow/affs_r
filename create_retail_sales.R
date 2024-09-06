Sys.setenv(R_READABS_PATH = "~/Downloads/")

library(tidyverse)
library(readabs)

retail_sales <-
  read_abs("8501.0") |>
  filter(series_type == "Original") |>
  filter(data_type == "FLOW",
         table_title == "TABLE 11. Retail Turnover, State by Industry Subgroup, Original") |>
  separate_wider_regex(series,
                       c("^.*;\\s+", state = ".*?", "\\s+;\\s+", industry = ".*?", "\\s+;$")) |>
  select(industry, state, date, value) |>
  distinct() |>
  arrange(date, industry, state) |>
  write_csv("data/retail_sales.csv")

library(readxl)

url <- "https://api.data.abs.gov.au/dataflow/all?detail=allstubs"
t <- tempfile()
download.file(url, t)

library(xml2)


temp <-
  read_xml(t) |>
  xml_child(2) |>
  xml_child(1) |>
  xml_contents() |>
  xml_attrs()

extract_data <- function(x) tibble(data.frame(t(x)))

df <- map(temp, extract_data) |> bind_rows()
df
url <- "https://api.data.abs.gov.au/datastructure/ABS/RT/M..M1..10.AUD.."
url <- "https://api.data.abs.gov.au/codelist/ABS/RT/1.0.0"

url <- "https://api.data.abs.gov.au/dataflow/ABS/RT"
url <- "https://api-reg.data.abs.gov.au/DisseminateNSIService/rest/dataflow/ABS/RT/1.0.0"
read_sdmx("/text/csv/ABS/{structureId}")

sdmx <- readSDMX(providerId = "ABS", resource = "data", flowRef = "RT",
                 key = list(NULL, NULL, NULL), start = 2010, end = 2011)
df <- as.data.frame(sdmx)

dsdUrl <- "https://api.data.abs.gov.au/datastructure/ABS/RT"
dsdUrl <- "https://api.data.abs.gov.au/datastructure/ABS/RT?references=codelist"
cls <- slot(dsd, "codelists")
codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))
codelists

codelist <- as.data.frame(slot(dsd, "codelists"), codelistId = "CL_STATE")
codelist
vdsdUrl <- "https://api.data.abs.gov.au/datastructure/ABS/RT?references=codelist"
cls <- slot(dsd, "codelists")
codelists <- sapply(slot(cls, "codelists"), function(x) slot(x, "id"))
codelists


https://api.data.abs.gov.au/datastructure/ABS/ALC
dsd <- readSDMX(dsdUrl)


concepts <- as.data.frame(slot(dsd, "concepts"))
concepts


codelists

codelist
attributes(dsd)

concepts <- as.data.frame(slot(dsd, "concepts"))
concepts

sdmx.data <- readSDMX(providerId = "OECD", resource = "data", flowRef = "MIG",
                      key = list("TOT", NULL, NULL), start = 2010, end = 2011)

sdmx.data <- readSDMX(providerId = "ABS", resource = "data", flowRef = "RT",
                      key = list("M1", NULL, 10, NULL, "M"))
df <- as_tibble(sdmx.data)


dsd <- readSDMX(providerId = "ABS", resource = "datastructure",
                resourceId = "RT")

#get codelists from DSD
cls <- slot(dsd, "codelists")
codelists <- sapply(slot(cls,"codelists"), slot, "id") #get list of codelists

#get a codelist
states <-
  as.data.frame(cls, codelistId = "CL_STATE") |>
  rename(state = label.en)
industries <-
  as.data.frame(cls, codelistId = "CL_RT_IND") |>
  rename(industry = label.en.label)


df |>
  inner_join(states, join_by(REGION == id)) |>
  inner_join(industries, join_by(INDUSTRY == id)) |>
  select(obsTime, industry, state, obsValue)


codelist
#get concepts from DSD
concepts <- as.data.frame(slot(dsd, "concepts"))


res <- readSDMX("https://api.data.abs.gov.au/data/ABS,RT,1.0.0/M1.20.10.AUS.M?dimensionAtObservation=AllDimensions")
res <- readSDMX("https://api.data.abs.gov.au/data/ABS,RT,1.0.0/M1..10..M&format=csv")
res <- readSDMX("https://api.data.abs.gov.au/data/ABS,RT,1.0.0/M1..10..M?dimensionAtObservation=AllDimensions")

df
#DSD
sdmx.dsd <- readSDMX(providerId = "OECD", resource = "datastructure", resourceId = "MIG")

#associate data and dsd
sdmx.data <- setDSD(sdmx.data, sdmx.dsd)
