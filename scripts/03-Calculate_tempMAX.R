#' @title
#' Monthly average data of PISCO
#'
#' @description
#' This script obtains monthly average data of PISCO
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
k.years <- 1981:2016

#' READ DAILY MAXIMUM DATA (PISCO)
grd.temp <- brick("data/raster/pisco/daily/PISCO-TxD-v1.1.nc")

#' BUILD A DATE DATAFRAME
date <- tibble(
  date = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "1 day")
) %>%
  mutate(id = 1:length(date))

#' OBTAIN MONTHLY AVERAGE DATA OF PISCO
for (i in k.years) { # loop by year
  for (j in sprintf("%.02d", 1:12)) { # loop by month
    n <- date %>%
      filter(
        str_sub(date, 1, 4) == i &
          str_sub(date, 6, 7) == j
      )
    era <- grd.temp[[n$id]] %>% mean(na.rm = T)
    writeRaster(
      era,
      sprintf("data/raster/pisco/monthly/mean/pisco_tempMEAN_%s-%s.tif", i, j)
    )
  }
}