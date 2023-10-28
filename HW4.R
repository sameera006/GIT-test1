library(tidyverse)
library(sf)
library(here)
library(janitor)
library(countrycode)

HDI <- read_csv(here::here("Week4", "HDR21-22_Composite_indices_complete_time_series.csv"),
                locale = locale(encoding = "latin1"),
                na = " ", skip=0)

World <- st_read("Week4/World_Countries_Generalized/World_Countries_Generalized.shp")

HDINEW <- HDI %>%
  clean_names() %>%
  select(iso3, country, gii_2019, gii_2010) %>%
  #filter(region != " ") %>%
  mutate(difference = gii_2019 - gii_2010 ) %>%
  mutate(iso_code = countrycode(country, origin = 'country.name', destination = 'iso2c' )) %>%
  mutate(iso_code2 = countrycode(iso3, origin= 'iso3c', destination = 'iso2c'))

join_HDI <- World %>%
  clean_names() %>%
  left_join(.,
            HDINEW,
            by = c ("iso" = "iso_code"))


