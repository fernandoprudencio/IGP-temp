#' @title
#' Read ERA5 data of hourly temperature in NetCDF
#'
#' @description
#' This script read hourly temperature data in NetCDF and obtain it daily
#'   temperature
#'
#' @author Fernando Prudencio
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster", "ncdf4", "rgdal")

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
library(ncdf4)
library(rgdal)

#' CONSTANTS
k.coord <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")

#' READ METADATA, LONGITUDE AND LATITUDE OF NETCDF FILE
data <- nc_open("data/raster/era5/hourly/era5_2011-2020.nc")
lon <- ncvar_get(data, "longitude")
lat <- ncvar_get(data, "latitude")

#' SECUENCIA DE DATA HORARIA
ts <- seq(24, 81793, 24)

#' SECUENCIA DE DATA DIARIA
date <- seq(as.Date("2011-01-01"), as.Date("2020-04-30"), by = "1 day")

for (i in 1:length(ts)) {
  if (i == 1) {
    array <- ncvar_get(
      data, "t2m",
      start = c(1, 1, 1), count = c(151, 226, 24)
    )
  } else {
    array <- ncvar_get(
      data, "t2m",
      start = c(1, 1, ts[i-1] + 1), count = c(151, 226, 24)
    )
  }

  img <- brick(
    array,
    xmn = min(lat), xmx = max(lat),
    ymn = min(lon), ymx = max(lon), crs = k.coord
  ) %>%
    t() %>%
    max(na.rm = T)

  img <- img - 273.15

  writeRaster(
    img, sprintf("data/raster/era5/daily/tif/era5_tempMAX_%s.tif", date[i]),
    overwrite = T
  )
}

#' STACK DAILY DATA IN A NETCDF FILE
#'   It's not possible for the moment
era5 <- list.files(
  "data/raster/era5/daily/tif",
  pattern = "*.tif", full.names = T
) %>% stack()

writeRaster(
  era5,
  "data/raster/era5/daily/netcdf/era5_tempMAX_1981-01-01_to_2020-04-30_v1.nc",
  overwrite = T
)