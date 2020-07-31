#' @title
#' Temperature climatology data
#'
#' @description
#' this script calculates the Temperature climatology data
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster", "ncdf4", "Hmisc")

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
library(raster)
library(ncdf4)
library(Hmisc)

#' CONSTANTS
k.yr.curr <- 2020
k.yrs.dry <- c(2005, 2010, 2016)
k.data <- "pisco" # ERA5 or PISCO data
k.type <- "mean" # maximum (max) or average (mean) monthly data

#' LIST OF DATA 
lst.temp <- list.files(
  sprintf("data/raster/%s/monthly/%s/", k.data, k.type),
  pattern = "*.tif", full.names = T
)

#' BUILD DATE DATAFRAME
date <- tibble(
  date = seq(
    as.Date("1981-01-01"),
    as.Date(sprintf("%s-12-01", k.yr.curr - 1)),
    by = "1 month"
  )
) %>%
  mutate(id = 1:length(date))

#' BUILD CLIMATOLOGY RASTER OF DRY AND NORMAL CONDITIONS
for (i in sprintf("%.02d", 1:12)) { # loop by month
  normal.yrs <- date %>% filter(
    str_sub(date, 1, 4) %nin% k.yrs.dry &
      str_sub(date, 6, 7) == i
  )
  
  dry.yrs <- date %>% filter(
    str_sub(date, 1, 4) %in% k.yrs.dry &
      str_sub(date, 6, 7) == i
  )
  
  if (i == "01") {
    grd.norm <- stack(lst.temp[normal.yrs$id] %>% na.omit()) %>% mean(na.rm = T)
    grd.dry <- stack(lst.temp[dry.yrs$id] %>% na.omit()) %>% mean(na.rm = T)
  } else {
    grd.norm <- stack(grd.norm,
      stack(lst.temp[normal.yrs$id] %>% na.omit()) %>% mean(na.rm = T)
    )
    grd.dry <- stack(grd.dry,
      stack(lst.temp[dry.yrs$id] %>% na.omit()) %>% mean(na.rm = T)
    )
  }
}

#' WRITE CLIMATOLOGY DATA OF DRY AND NORMAL CONDITIONS
if (k.data == "era5") {
  writeRaster(
    grd.norm,
    sprintf(
      "data/raster/%s/climatology/%s_mnthly%stemp_climNORM_1981-%s.nc",
      k.data, k.data, toupper(k.type), k.yr.curr - 1
    )
  )
  
  writeRaster(
    grd.dry,
    sprintf(
      "data/raster/%s/climatology/%s_mnthly%stemp_climDRY_1981-%s.nc",
      k.data, k.data, toupper(k.type), k.yr.curr - 1
    )
  )
}

if (k.data == "pisco") {
  writeRaster(
    grd.norm,
    sprintf(
      "data/raster/%s/climatology/%s_mnthly%stemp_climNORM_1981-2016.nc",
      k.data, k.data, toupper(k.type)
    )
  )
  
  writeRaster(
    grd.dry,
    sprintf(
      "data/raster/%s/climatology/%s_mnthly%stemp_climDRY_1981-2016.nc",
      k.data, k.data, toupper(k.type)
    )
  )
}