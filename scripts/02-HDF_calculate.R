#' @title
#' Hot Day Frequency of  gauged data
#'
#' @description
#' this script calculates the HDF of gauged data
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "sf", "tidyverse", "spatialEco", "reshape2", "stringr"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' LOAD PACKAGE
library(tidyverse)
library(sf)
library(spatialEco)
library(reshape2)
library(stringr)

#' CHANGE TO ENGLISH LANGUAGE
Sys.setlocale(category = "LC_ALL", locale = "english")

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' LOAD CONSTANTS
k.prdo <- c("1990-01-01", "2013-12-31")
k.date.day <- seq(as.Date("1928-11-02"), as.Date("2015-10-31"), by = "day")
k.date.year <- seq(as.Date("1990-01-01"), as.Date("2013-12-31"), by = "year")
k.date.plt <- seq(as.Date("2020-03-01"), as.Date("2020-12-31"), by = "day")
k.years <- c(1990:2013)
k.dry.yr <- c(2005, 2010)
k.regions <- c(6)
k.threshold <- 0.9

#' READ VECTORIAL DATA
#'   load cluster region
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

#'   load location of the stations
lyrs.gaug <- rgdal::ogrListLayers(
  "data/vector/senamhi_weather_stations.gpkg"
)

sf.gauge.lct <- st_read(
  dsn = "data/vector/senamhi_weather_stations.gpkg",
  layer = lyrs.gaug[3], quiet = T, as_tibble = T
) %>%
  arrange(cod)

#' READ TABLE DATA
tbl.gauge.data <- read.csv("data/table/BD_Tmax.csv",
  header = T, sep = ";"
) %>%
  dplyr::select(-X, -DATE) %>%
  mutate(date = k.date.day) %>%
  as_tibble() %>%
  mutate_all(~ na_if(., -99.9)) %>%
  filter(date >= k.prdo[1] & date <= k.prdo[2])

#' SELECT STATION INTO REGION CLUSTER
sf.gauge.rg <- sf.gauge.lct %>%
  point.in.poly(sf.region, sp = T) %>%
  st_as_sf() %>%
  drop_na() %>%
  arrange(cod) %>%
  mutate(cod = as.vector(cod))

#' CALCULATE AMOUNT OF NODATA VALUES FOR EACH STATION
miss.val.amoun <- tbl.gauge.data %>%
  dplyr::select(sf.gauge.rg$cod) %>%
  is.na() %>%
  colSums()

#' DATAFRAME WITH A LIST OF STATION WITH LESS THAN 10% OF MISSING VALUES
df.miss.val <- tibble(
  id = c(1:length(miss.val.amoun)),
  miss.per = miss.val.amoun * 100 / nrow(tbl.gauge.data),
  cod = sf.gauge.rg$cod
) %>%
  filter(miss.per <= 10)

#' DATAFRAME WITH MEAN RAINGAUGE DATA BY CLUSTER REGION WITH LESS THAN 10% OF
#'   MISSING VALUES. FURTHER, WITHOUT JANUARY AND FEBRUARY
df.rgn.data <- tibble(
  date = tbl.gauge.data$date,
  temp.mean = tbl.gauge.data %>%
    dplyr::select(df.miss.val$cod) %>%
    apply(1, FUN = function(x) mean(x, na.rm = T))
) %>%
  mutate(month = str_sub(date, 6, 7)) %>%
  group_by(month) %>%
  mutate(decil = quantile(temp.mean, k.threshold)) %>%
  ungroup() %>%
  mutate(hdf = ifelse(temp.mean >= decil, 1, 0)) %>%
  filter(!(substr(date, 6, 7) %in% c("01", "02")))

#' DATAFRAME WITH AVERAGE ACCUMULATED HDF BY CLUSTER REGION
for (i in k.years) {
  if (i == k.years[1]) {
    df.hdf.ac <- df.rgn.data %>%
      filter(substr(date, 1, 4) == i) %>%
      mutate(hdf.ac = cumsum(hdf)) %>%
      dplyr::select(hdf.ac) %>%
      as_tibble()
    names(df.hdf.ac)[i - (k.years[1] - 1)] <- sprintf("yr.%s", i)
  } else {
    df.hdf.ac <- df.hdf.ac %>%
      cbind(
        df.rgn.data %>%
          filter(substr(date, 1, 4) == i) %>%
          mutate(hdf.ac = cumsum(hdf)) %>%
          dplyr::select(hdf.ac)
      ) %>%
      as_tibble()
    names(df.hdf.ac)[i - (k.years[1] - 1)] <- sprintf("yr.%s", i)
  }
}

#' DATAFRAME WITH AVERAGE ACCUMULATED HDF BY CLUSTER REGION, DURING DRY YEARS
#'   AND NORMAL YEARS
df.hdf.ac.norm <- df.hdf.ac %>%
  dplyr::select(sprintf("yr.%s", k.years[!(k.years %in% k.dry.yr)]))

df.hdf.ac.dry <- df.hdf.ac %>%
  dplyr::select(sprintf("yr.%s", k.dry.yr)) %>%
  mutate(
    hdf.max = df.hdf.ac.norm %>% apply(1, max, na.rm = T),
    hdf.min = df.hdf.ac.norm %>% apply(1, min, na.rm = T),
    hdf.mean = df.hdf.ac.norm %>% apply(1, mean, na.rm = T),
    date = k.date.plt
  ) %>%
  as_tibble()

#' PLOT TEMPORAL EVOLUTION ACCUMULATED HDF
plt.hdf.ssnl <- ggplot(df.hdf.ac.dry, aes(date, yr.2005)) +
  # annotate("rect",
  #         xmin = as.Date("2020-04-20"),
  #         xmax = as.Date("2020-10-10"),
  #         ymin = 0, ymax = 80, alpha = 0.1, fill = "red", color = "red"
  # ) +
  geom_ribbon(
    aes(ymin = hdf.min, ymax = hdf.max),
    alpha = 0.5, color = "black"
  ) +
  geom_line(colour = "blue", size = 1) +
  labs(
    y = "Hot Day Frequency",
    title = "Temporal evolution of Hot Day Frequency",
    subtitle = "from 1990 to 2013"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.text.x = element_text(size = 12, angle = 0, hjust = -0.3),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 15),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(
      size = 0.3, color = "gray", linetype = "dashed"
    ),
    panel.border = element_rect(size = 1)
  ) +
  scale_x_date(date_labels = "%b", breaks = "1 month", expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0, 130, 20),
    limits = c(0, 130),
    expand = c(0, 0)
  ) +
  geom_line(aes(date, yr.2010), colour = "black", size = 1) +
  geom_line(aes(date, hdf.mean),
    colour = "white",
    size = 1,
    linetype = "dashed"
  )

ggsave(
  plot = plt.hdf.ssnl,
  sprintf(
    "exports/hdf_region_n%s_dec%s_1990-2013.png",
    k.regions,
    k.threshold
  ),
  width = 14, height = 15, units = "cm", dpi = 1000
)

#' PLOT ACCUMULATED HDF BY YEAR
hdf.yr <- tibble(
  hdf = apply(df.hdf.ac, 2, max),
  date = k.date.year
) %>%
  mutate(
    ubic.dry.05 = ifelse(str_sub(date, 1, 4) == "2005", hdf, NA),
    ubic.dry.10 = ifelse(str_sub(date, 1, 4) == "2010", hdf, NA)
  )

plt.hdf.yr <- ggplot(hdf.yr, aes(date, hdf)) +
  geom_hline(
    yintercept = hdf.yr$ubic.dry.05[!is.na(hdf.yr$ubic.dry.05)],
    linetype = "dashed", alpha = 1,
    color = "blue", size = 1
  ) +
  geom_hline(
    yintercept = hdf.yr$ubic.dry.10[!is.na(hdf.yr$ubic.dry.10)],
    linetype = "dashed", alpha = 1,
    color = "black", size = 1
  ) +
  geom_line(colour = "gray", size = 1.4) +
  geom_point(colour = "gray", size = 2.5, shape = 15) +
  geom_point(aes(date, ubic.dry.05), color = "blue", size = 4.5, shape = 15) +
  geom_point(aes(date, ubic.dry.10), color = "black", size = 4.5, shape = 15) +
  labs(y = "Hot Day Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 18, hjust = 0),
    plot.subtitle = element_text(size = 15, hjust = 0),
    axis.text.x = element_text(size = 15, angle = 0),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 17),
    panel.grid = element_line(size = 0.3, color = "gray", linetype = "dashed"),
    panel.border = element_rect(size = 1)
  ) +
  scale_x_date(date_labels = "%Y", breaks = "5 year", expand = c(0, 0)) +
  scale_y_continuous(
    breaks = seq(0, 130, 20),
    limits = c(0, 130),
    expand = c(0, 0)
  )

ggsave(
  plot = plt.hdf.yr,
  sprintf(
    "exports/hdf_yr_region_n%s_dec%s_1990-2013.png",
    k.regions,
    k.threshold
  ),
  width = 18, height = 9, units = "cm", dpi = 1000
)

#' BOXPLOT ACCUMULATED HDF
month.lbl <- tibble(month = sprintf("%.02d", 1:12), lbl = month.abb)

df.hdf.month <- df.hdf.ac %>%
  mutate(date = k.date.plt, month = str_sub(date, 6, 7)) %>%
  dplyr::select(-date) %>%
  melt(id.var = "month") %>%
  as_tibble() %>%
  group_by(month, variable) %>%
  summarise(hdf = max(value)) %>%
  group_by(month) %>%
  mutate(
    variable = str_sub(variable, -4, -1),
    outlier = ifelse(is_outlier(hdf), variable, NA),
    outlier2 = ifelse(hdf == max(hdf), variable, NA),
    ubic.otlr = ifelse(hdf == max(hdf), hdf, NA),
    txt.dry.05 = ifelse(variable == "2005", variable, NA),
    txt.dry.10 = ifelse(variable == "2010", variable, NA),
    ubic.dry.05 = ifelse(variable == "2005", hdf, NA),
    ubic.dry.10 = ifelse(variable == "2010", hdf, NA)
  ) %>%
  ungroup() %>%
  left_join(month.lbl %>% filter(month != c("01", "02")), by = "month") %>%
  dplyr::rename(year = variable)

boxplt.hdf <- ggplot(df.hdf.month, mapping = aes(month, hdf)) +
  geom_boxplot(
    alpha = 1, outlier.size = NULL, width = 0.5,
    fatten = 1.5, lwd = .8, color = "gray"
  ) +
  geom_jitter(
    shape = 16,
    size = 0.8, color = "gray",
    position = position_jitter(0.2)
  ) +
  stat_summary(
    fun.y = "mean",
    geom = "point",
    shape = 3, size = 4, colour = "red",
    fill = "red"
  ) +
  scale_x_discrete(label = month.lbl$lbl[3:12]) +
  scale_y_continuous(
    breaks = seq(0, 140, 20),
    limits = c(-5, 140),
    expand = c(0, 0)
  ) +
  geom_text(
    aes(label = outlier2),
    size = 3, na.rm = TRUE, hjust = 0.5,
    vjust = -.5, check_overlap = T, color = "gray"
  ) +
  geom_point(
    aes(month, ubic.otlr),
    color = "gray", size = 1.5
  ) +
  #  geom_text(
  #    aes(label = txt.dry.05),
  #    size = 3, na.rm = TRUE, hjust = 0.5,
  #    vjust = -.5, check_overlap = F, color = "blue"
  #  ) +
  #  geom_text(
  #    aes(label = txt.dry.10),
  #    size = 3, na.rm = TRUE, hjust = 0.5,
  #    vjust = -.5, check_overlap = F, color = "black"
  #  ) +
  geom_point(
    aes(month, ubic.dry.05),
    color = "blue", size = 1.5
  ) +
  geom_point(
    aes(month, ubic.dry.10),
    color = "black", size = 1.5
  ) +
  labs(
    title = "Monthly HDF distribution", subtitle = "from 1990 to 2013",
    y = "Hot Day Frequency"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 15),
    axis.text.x = element_text(size = 13, colour = "black"),
    axis.text.y = element_text(size = 13, colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.ticks.length = unit(.15, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(
      size = 0.3, color = "gray", linetype = "dashed"
    ),
    panel.border = element_rect(size = 1)
  )

ggsave(
  plot = boxplt.hdf,
  sprintf(
    "exports/hdf_month_region_n%s_dec0.1_1990-2013.png",
    k.regions,
    k.threshold
  ),
  width = 12, height = 16, units = "cm", dpi = 1000
)

#' PLOT SEASONAL BEHAVIOR OF RAINFALL
df.temp.rgn.data <- tibble(
  date = tbl.gauge.data$date,
  temp.mean = tbl.gauge.data %>%
    dplyr::select(df.miss.val$cod) %>%
    apply(1, FUN = function(x) mean(x, na.rm = T))
) %>%
  mutate(month = str_sub(date, 6, 7)) %>%
  group_by(month) %>%
  mutate(decil.1 = quantile(temp.mean, .1)) %>%
  ungroup() %>%
  mutate(cdf = ifelse(temp.mean <= decil.1, 1, 0)) %>%
  group_by(month) %>%
  mutate(decil.9 = quantile(temp.mean, .9)) %>%
  ungroup() %>%
  mutate(hdf = ifelse(temp.mean >= decil.9, 1, 0)) %>%
  left_join(month.lbl, by = "month")

avr.umbral.hdf <- unique(df.temp.rgn.data$decil.9) %>% mean()
avr.umbral.cdf <- unique(df.temp.rgn.data$decil.1) %>% mean()

#' region 6
#'   avr.umbral.ddf = 0.4363271
#'   avr.umbral.wdf = 3.915852

#' region 8
#'   avr.umbral.ddf = 0.3022238
#'   avr.umbral.wdf = 3.117474

boxplt.hdf <- ggplot(df.temp.rgn.data, mapping = aes(month, temp.mean)) +
  geom_boxplot(
    alpha = 1, outlier.size = .5, width = 0.5,
    fatten = 1.5, lwd = .5, color = "gray", outlier.color = "gray"
  ) +
  #  geom_jitter(
  #    shape = 16,
  #    size = 0.2, color = "gray",
  #    position = position_jitter(0.1)
  #  ) +
  stat_summary(
    fun.y = "mean",
    geom = "point",
    shape = 3, size = 4, colour = "black"
  ) +
  scale_x_discrete(label = month.lbl$lbl) +
  scale_y_continuous(
    breaks = seq(6, 26, 2),
    limits = c(6, 26),
    expand = c(0, 0)
  ) +
  geom_text(
    aes(x = month, y = decil.1, label = round(decil.1, 2)),
    size = 3, na.rm = TRUE, hjust = 0.5,
    vjust = 2, check_overlap = F, color = "blue"
  ) +
  geom_point(
    aes(month, decil.1),
    color = "blue", size = 2.5, shape = 25, fill = "blue"
  ) +
  geom_text(
    aes(x = month, y = decil.9, label = round(decil.9, 2)),
    size = 3, na.rm = TRUE, hjust = 0.5,
    vjust = -1, check_overlap = F, color = "red"
  ) +
  geom_point(
    aes(month, decil.9),
    color = "red", size = 2.5, shape = 24, fill = "red"
  ) +
  labs(
    title = "Daily temperature distribution", subtitle = "from 1990 to 2013",
    y = "temperature [°C]"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 15),
    plot.subtitle = element_text(size = 15),
    axis.text.x = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 10, colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20),
    axis.ticks.length = unit(.15, "cm"),
    # panel.grid.minor = element_blank(),
    # panel.grid = element_line(
    #  size = 0.3, color = "gray", linetype = "dashed"
    # ),
    panel.grid = element_blank(),
    panel.border = element_rect(size = 1)
  )

ggsave(
  plot = boxplt.hdf,
  sprintf("exports/temp_month_region_n%s_1990-2013.png", k.regions),
  width = 12, height = 16, units = "cm", dpi = 1000
)