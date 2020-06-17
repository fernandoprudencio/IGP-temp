#' @title
#' Calculate missing values
#'
#' @description
#' this script calculates the amount of missing values
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("sf", "tidyverse")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' LOAD PACKAGES
library(tidyverse)
library(sf)

#' LOAD CONSTANTS
k.regions <- c(6, 8)
k.period <- c("1990-01-01", "2013-12-31")
k.thrshld <- 10

#' READ VECTORIAL DATA
#'   read cluster region
lyrs.rgn <- rgdal::ogrListLayers(
  "data/vector/cluster_region.gpkg"
)

sf.region <- st_read(
  dsn = "data/vector/cluster_region.gpkg",
  layer = lyrs.rgn[1], quiet = T, as_tibble = T
) %>%
  group_by(gridcode) %>%
  summarise(nfeature = length(gridcode)) %>%
  dplyr::filter(gridcode %in% k.regions)

#'   read station of temperature recording stations
lyrs.sttn <- rgdal::ogrListLayers(
  "data/vector/senamhi_weather_stations.gpkg"
)

sf.station <- st_read(
  dsn = "data/vector/senamhi_weather_stations.gpkg",
  layer = lyrs.sttn[2], quiet = T, as_tibble = T
) %>%
  st_intersection(sf.region) %>% # select only stations within regions
  dplyr::rename(cod = CODIGO) %>%
  mutate(cod = as.character(cod)) %>%
  dplyr::select(gridcode, cod)

#' LOAD DAILY DATA OF TEMPERATURE
df.temp <- read.csv("data/table/BD_Tmax.csv", sep = ";") %>%
  as_tibble() %>%
  dplyr::select(-X) %>%
  dplyr::rename(date = DATE) %>%
  mutate(
    date = as.Date(date, format = "%d/%m/%Y")
  ) %>%
  mutate_all(~ na_if(., -99.9))

#' CALCULATE THE NUMBER OF MISSING VALUES BY STATION
df.miss <- tibble(
  amnt =
    apply(df.temp, 2, FUN = function(x) {
      is.na(x) %>%
        which() %>%
        length()
    })[-1],
  cod = names(df.temp)[-1],
  per = amnt * 100 / nrow(df.temp)
) %>%
  filter(cod %in% sf.station$cod & per < 100)

#' BUILD A DATAFRAME TO PLOT THE DATA AVAILABILITY FROM 1982 TO 2015
df <- df.temp %>%
  gather(cod, temp, -date) %>%
  filter(cod %in% df.miss$cod) %>%
  left_join(
    sf.station %>% as_tibble() %>% dplyr::select(-geom),
    by = "cod"
  ) %>%
  mutate(gridcode = ifelse(gridcode == 6, "region 6", "region 8"))

#' PLOT THE DATA AVAILABILITY FROM 1982 TO 2015
plt <- ggplot(df, aes_string(y = "cod", x = "temp", group = "gridcode")) +
  aes_string(
    x = "date", fill = "temp",
    y = "cod"
  ) +
  geom_tile() +
  theme_bw() +
  scale_fill_continuous(na.value = "white", low = "#494949", high = "#494949") +
  facet_grid(gridcode ~ ., scales = "free", space = "free", switch = "both") +
  scale_x_date(
    breaks = seq(
      as.Date("1930-01-01"),
      as.Date("2015-10-31"),
      by = "10 years"
    ),
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    strip.background = element_rect(colour = "black", fill = "red"),
    strip.text = element_text(colour = "white", face = "bold", size = 20),
    #axis.line.x = element_line(color = "black", size = 1),
    #axis.line.y = element_line(color = "black", size = 1),
    axis.text.x = element_text(size = 18, colour = "black", face = "bold"),
    panel.border = element_rect(colour = "black", size = 1)
  )

ggsave(plt,
  filename = "exports/available_temperature_data_1928-2015.png",
  width = 20, height = 20, units = "cm", dpi = 500
)

#' SELECT DATA WITH CONTINUOUS RECORD
#'   load data between period
df.sltd <- read.csv("data/table/BD_Tmax.csv", sep = ";") %>%
  as_tibble() %>%
  dplyr::select(-X) %>%
  dplyr::rename(date = DATE) %>%
  mutate(
    date = as.Date(date, format = "%d/%m/%Y")
  ) %>%
  mutate_all(~ na_if(., -99.9)) %>%
  filter(date >= k.period[1] & k.period[2] >= date)

#'   select stations with less than 10% of missing values
df.miss.sltd <- tibble(
  amnt =
    apply(df.sltd, 2, FUN = function(x) {
      is.na(x) %>%
        which() %>%
        length()
    })[-1],
  cod = names(df.sltd)[-1],
  per = amnt * 100 / nrow(df.sltd)
) %>%
  filter(cod %in% df.miss$cod)

#' BUILD A DATAFRAME TO PLOT THE AVAILABILITY OF DATA FOR THE SELECTED PERIOD
df <- df.temp %>%
  gather(cod, temp, -date) %>%
  filter(
    cod %in%
      (
        df.miss.sltd %>%
          filter(per <= k.thrshld) %>%
          pull(cod)
      )
  ) %>%
  left_join(
    sf.station %>% as_tibble() %>% dplyr::select(-geom),
    by = "cod"
  ) %>%
  filter(date >= k.period[1] & k.period[2] >= date) %>%
  mutate(gridcode = ifelse(gridcode == 6, "region 6", "region 8"))

#' PLOT THE AVAILABILITY OF DATA FOR THE SELECTED PERIOD 
plt.prd <- ggplot(df, aes_string(y = "cod", x = "temp", group = "gridcode")) +
  aes_string(
    x = "date", fill = "temp",
    y = "cod"
  ) +
  geom_tile() +
  theme_bw() +
  scale_fill_continuous(na.value = "white", low = "#494949", high = "#494949") +
  facet_grid(gridcode ~ ., scales = "free", space = "free", switch = "both") +
  scale_x_date(
    breaks = seq(
      as.Date("2000-01-01"),
      as.Date("2015-10-31"),
      by = "2 years"
    ),
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    strip.background = element_rect(colour = "black", fill = "red"),
    strip.text = element_text(colour = "white", face = "bold", size = 20),
    # axis.line.x = element_line(color = "black", size = 1),
    # axis.line.y = element_line(color = "black", size = 1),
    axis.text.x = element_text(size = 18, colour = "black", face = "bold"),
    panel.border = element_rect(colour = "black", size = 1)
  )

ggsave(plt.prd,
  filename = "exports/available_temperature_data_1990-2013.png",
  width = 20, height = 20, units = "cm", dpi = 500
)

#' FIND AND DAVE AVALIBLE STATION OF TEMPERATURE
sf.temp <- sf.station %>%
  filter(cod %in% df.miss.sltd$cod) %>%
  left_join(df.miss.sltd, by = "cod")

st_write(sf.temp,
         dsn = "data/vector/senamhi_weather_stations.gpkg",
         layer = "temperature_stations", quiet = T, delete_layer = T
)