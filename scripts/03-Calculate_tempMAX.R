#' @title
#' Monthly maximum data of ERA5
#'
#' @description
#' This script obtains monthly maximum data of ERA5
#'
#' @author Fernando Prudencio
#'

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster", "Hmisc", "ncdf4")

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
library(raster)
library(Hmisc)
library(ncdf4)

#' CONSTANTS
k.years <- 1981:2020

#' READ LIST OF DAILY MAXIMUM DATA (ERA5)
lst.temp <- list.files(
  "data/raster/era5/daily/",
  pattern = "*.tif", full.names = T
)

#' BUILD A DATE DATAFRAME
date <- tibble(
  date = seq(as.Date("1981-01-01"), as.Date("2020-04-30"), by = "1 day")
) %>%
  mutate(id = 1:length(date))

#' OBTAIN MONTHLY MAXIMUM DATA OF ERA5
for (i in k.years) { # loop by year
  for (j in sprintf("%.02d", 1:12)) { # loop by month
    n <- date %>%
      filter(
        str_sub(date, 1, 4) == i &
          str_sub(date, 6, 7) == j
      )
    era <- stack(lst.temp[n$id]) %>% max(na.rm = T)
    writeRaster(
      era,
      sprintf("data/raster/era5/monthly/max/era5_tempMAX_%s-%s.tif", i, j)
    )
  }
}