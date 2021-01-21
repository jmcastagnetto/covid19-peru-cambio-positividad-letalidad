library(tidyverse)

src <- "https://raw.githubusercontent.com/jmcastagnetto/covid-19-peru-data/main/datos/covid-19-peru-data-augmented.csv"

peru <- read_csv(
  src,
  col_types = cols(
    country = col_character(),
    iso3c = col_character(),
    region = col_character(),
    date = col_date(format = ""),
    confirmed = col_double(),
    deaths = col_double(),
    recovered = col_double(),
    total_tests = col_double(),
    negative_tests = col_double(),
    pcr_test_positive = col_double(),
    serological_test_positive = col_double(),
    ag_test_positive = col_double(),
    pcr_serological_test_positive = col_double()
  )
)

nacional <- peru %>%
  filter(is.na(region)) %>%
  select(
    date,
    confirmed,
    negative_tests,
    deaths
  ) %>%
  filter(!is.na(confirmed) & !is.na(negative_tests)) %>%
  rename(
    pos = confirmed,
    neg = negative_tests
  )

regional <- peru %>%
  filter(!is.na(region)) %>%
  select(
    date,
    region,
    confirmed,
    negative_tests,
    deaths
  ) %>%
  filter(!is.na(confirmed) & !is.na(negative_tests)) %>%
  rename(
    pos = confirmed,
    neg = negative_tests
  )

save(
  nacional, regional,
  file = "datos/datos-procesados.Rdata"
)
