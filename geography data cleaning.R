library(ISOcodes)
library(cld3)
library(dplyr)
library(stringdist)
library(fuzzyjoin)
library(sf)
library(tidyverse)
library(tidyr)
library(jsonlite)
library(tidytext)
library(textdata)

# LOAD DATA
setwd("/Users/carinaclewley/Documents/dissertation/data")
Sys.setenv(SHAPE_RESTORE_SHX = "YES")

# geographical
geospatial_data <- st_read("CShapes-2.0.shp")
geospatial_2 <- st_read("ne_10m_admin_0_countries.shp")

# internal politics
voting_data <- read_csv("votes.csv")
contestants_data <- read_csv("contestants.csv")
contestants_data <- contestants_data %>%
  mutate(to_country = recode(to_country, "North MacedoniaN.Macedonia" = "North Macedonia"),
         to_country = recode(to_country, "Czechia" = "Czech Republic"))
contestants_data <- contestants_data %>%
  distinct(year, to_country, song, performer, .keep_all = TRUE)

country_lookup <- contestants_data %>%
  distinct(to_country_id, to_country)

country_lookup <- country_lookup %>%
  bind_rows(
    tibble(
      to_country_id = c("ad", "wld"),
      to_country = c("Andorra", "World")
    )
  )

voting_data <- voting_data %>%
  left_join(country_lookup %>% rename(from_country_name = to_country), 
            by = c("from_country_id" = "to_country_id")) %>%
  left_join(country_lookup %>% rename(to_country_name = to_country), 
            by = c("to_country_id" = "to_country_id")) %>%
  select(-from_country, -to_country, -from_country_id, -to_country_id)  # drop the old id columns

geospatial_data <- geospatial_data %>%
  mutate(cntry_name = recode(cntry_name, "Belarus (Byelorussia)" = "Belarus"),
         cntry_name = recode(cntry_name, "Bosnia-Herzegovina" = "Bosnia & Herzegovina"),
         cntry_name = recode(cntry_name, "German Federal Republic" = "Germany"),
         cntry_name = recode(cntry_name, "German Democratic Republic" = "East Germany"),
         cntry_name = recode(cntry_name, "Macedonia (FYROM/North Macedonia)" = "North Macedonia"),
         cntry_name = recode(cntry_name, "Rumania" = "Romania"),
         cntry_name = recode(cntry_name, "Russia (Soviet Union)" = "Russia"),
         cntry_name = recode(cntry_name, "Turkey (Ottoman Empire)" = "Turkey"),
         cntry_name = recode(cntry_name, "Italy/Sardinia" = "Italy")
         )

geospatial_data <- geospatial_data %>%
  mutate(cntry_name = if_else(gweyear == 2008 & cntry_name == "Serbia", 
                              "Serbia & Montenegro", 
                              cntry_name))

participation <- contestants_data %>%
  group_by(to_country) %>%
  summarise(
    years_participated = n_distinct(year),
    first_year = min(year),
    last_year = max(year),
    .groups = "drop"
  ) %>%
  mutate(
    total_possible = last_year - first_year + 1,
    participation_rate = years_participated / total_possible
  ) %>%
  arrange(desc(participation_rate))

geospatial_data <- geospatial_data %>%
  filter(cntry_name %in% participation$to_country)

geospatial_data <- geospatial_data %>%
  filter(gweyear > 1956)

geospatial_data <- geospatial_data %>%
  mutate(gwedate = if_else(gwedate == as.Date("2019-12-31"), as.Date("2026-12-31"), gwedate))

geospatial_data <- geospatial_data %>%
  mutate(year_length = gweyear - gwsyear)

geospatial_data_clean <- contestants_data %>%
  select(year, to_country, song)

missing_countries <- geospatial_2 %>%
  filter(NAME_EN %in% c("Andorra", "Monaco", "San Marino")) %>%
  select(NAME_EN, geometry) %>%
  st_transform(st_crs(geospatial_data)) %>%   # transform CRS here before anything else
  rename(cntry_name = NAME_EN) %>%
  mutate(
    gwsdate = case_when(
      cntry_name == "Andorra"    ~ as.Date("1278-01-01"),
      cntry_name == "Monaco"     ~ as.Date("1861-01-01"),
      cntry_name == "San Marino" ~ as.Date("1463-01-01")
    ),
    gwedate = as.Date("2026-01-01"),
    area = NA_real_
  )

geospatial_data <- geospatial_data %>%
  mutate(
    gwsdate = case_when(
      cntry_name == "Serbia & Montenegro" ~ as.Date("2003-02-04"),  # when Serbia & Montenegro was formed
      cntry_name == "Serbia"              ~ as.Date("2006-06-05"),  # day after S&M dissolved
      TRUE ~ gwsdate
    )
  )

geospatial_data <- bind_rows(
  geospatial_data %>% select(cntry_name, gwsdate, gwedate, geometry, area, capname, caplong, caplat),
  missing_countries %>% select(cntry_name, gwsdate, gwedate, geometry, area)
)

geospatial_data_clean <- contestants_data %>%
  select(year, to_country, song, performer) %>%
  mutate(contest_date = as.Date(paste0(year, "-04-01"))) %>%
  left_join(
    geospatial_data %>% select(cntry_name, gwsdate, gwedate, geometry, area, capname, caplong, caplat),
    by = c("to_country" = "cntry_name"),
    relationship = "many-to-many"
  ) %>%
  group_by(to_country, year, song, performer) %>%
  arrange(
    # first prefer rows where contest date falls within the period
    !(contest_date >= gwsdate & contest_date <= gwedate),
    # then pick the period whose start date is closest to contest date
    abs(as.numeric(contest_date - gwsdate))
  ) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(years_since_border_change = as.numeric(contest_date - gwsdate) / 365.25) %>%
  select(-gwsdate, -gwedate, -contest_date)

# get unique country-year capital locations
capital_locations <- geospatial_data_clean %>%
  select(to_country, year, caplong, caplat) %>%
  filter(!is.na(caplong), !is.na(caplat)) %>%
  distinct(to_country, year, .keep_all = TRUE)


# get unique country-year geometries
border_geometries <- geospatial_data_clean %>%
  select(to_country, year, geometry) %>%
  filter(!is.na(geometry)) %>%
  distinct(to_country, year, .keep_all = TRUE) %>%
  st_as_sf() %>%
  st_make_valid()   # <-- add this line

# join capital coords for both from and to country
voting_data_geo <- voting_data %>%
  filter(from_country_name != "World") %>%   # drop world rows
  left_join(capital_locations %>% 
              rename(from_caplong = caplong, from_caplat = caplat),
            by = c("from_country_name" = "to_country", "year")) %>%
  left_join(capital_locations %>% 
              rename(to_caplong = caplong, to_caplat = caplat),
            by = c("to_country_name" = "to_country", "year"))

# capital to capital distance
voting_data_geo <- voting_data_geo %>%
  rowwise() %>%
  mutate(
    capital_distance_km = case_when(
      is.na(from_caplong) | is.na(to_caplong) ~ NA_real_,
      TRUE ~ as.numeric(st_distance(
        st_sfc(st_point(c(from_caplong, from_caplat)), crs = 4326),
        st_sfc(st_point(c(to_caplong, to_caplat)), crs = 4326)
      )) / 1000
    )
  ) %>%
  ungroup()

# extract geometries as list columns before joining
from_geoms <- border_geometries %>%
  mutate(from_geometry = st_geometry(geometry)) %>%
  st_drop_geometry() %>%
  rename(from_country_name = to_country) %>%
  select(from_country_name, year, from_geometry)

to_geoms <- border_geometries %>%
  mutate(to_geometry = st_geometry(geometry)) %>%
  st_drop_geometry() %>%
  rename(to_country_name = to_country) %>%
  select(to_country_name, year, to_geometry)

voting_data_geo <- voting_data_geo %>%
  left_join(from_geoms, by = c("from_country_name", "year")) %>%
  left_join(to_geoms, by = c("to_country_name", "year"))

# border to border distance
voting_data_geo <- voting_data_geo %>%
  rowwise() %>%
  mutate(
    border_distance_km = case_when(
      is.na(from_geometry) | is.na(to_geometry) ~ NA_real_,
      TRUE ~ as.numeric(st_distance(
        st_sfc(from_geometry, crs = st_crs(border_geometries)),
        st_sfc(to_geometry, crs = st_crs(border_geometries))
      )) / 1000
    )
  ) %>%
  ungroup() %>%
  select(-from_geometry, -to_geometry, -from_caplong, -from_caplat,
         -to_caplong, -to_caplat)

write_csv(voting_data_geo, "voting_data_geo.csv")
write_csv(geospatial_data_clean, "geospatial_data_clean.csv")