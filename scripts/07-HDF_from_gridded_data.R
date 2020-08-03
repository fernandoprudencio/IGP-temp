#' @title
#' Plot the seasonal evolution of Hot Day Frequency
#'
#' @description
#' this script plots the seasonal evolution of Hot Day Frequency (HDF) by
#'   region (vectorial data)
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("raster", "tidyverse", "sf", "Hmisc", "extrafont", "grid")

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
library(raster)
library(tidyverse)
library(sf)
library(Hmisc)
library(extrafont)
library(grid)

#' CHANGE TO ENGLISH LANGUAGE
Sys.setlocale(category = "LC_ALL", locale = "english")

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' LOAD CONSTANTS
cat("1. era5\n")
cat("2. pisco\n")
data <- as.integer(readline(prompt = "Enter data source [1/2]: \n"))
k.data <- switch(data, "era5", "pisco")

k.date.str <- as.character(readline(prompt = "\nEnter initial date: \n"))
k.date.end <- as.character(readline(prompt = "\nEnter final date: \n"))
k.thrhld <- as.integer(readline(prompt = "\nEnter threshold from decil value: \n")) / 10
link.rgns <- as.character(readline(prompt = "\nEnter vector location: \n"))

k.dry.yr <- c(2005, 2010, 2016) # dry years
k.date.prd <- seq(as.Date(k.date.str), as.Date(k.date.end), by = "1 day")
k.prd <- seq(as.Date("1981-01-01"), as.Date(k.date.end), by = "1 day")

#' READ LIST OF DATA
if (k.data == "era5") {
  lst.data <- list.files(
    sprintf("data/raster/%s/daily/", k.data),
    pattern = "*.tif", full.names = T
  )
  lst.lng <- length(lst.data)
}

if (k.data == "pisco") {
  lst <- list.files(
    sprintf("data/raster/%s/daily/", k.data),
    pattern = "*.nc", full.names = T
  )
  for (i in 1:length(lst)) {
    cat(sprintf("\n%s. %s", i, lst[i] %>% as.character()))
  }
  lst.opt <- as.integer(readline(prompt = "\nEnter raster file[1/2/3/...]: \n"))
  grd.data <- switch(lst.opt, lst[1], lst[2], lst[3], lst[4], lst[5]) %>%
    brick()
  lst.lng <- grd.data %>% nlayers()
}

if (length(k.prd) > lst.lng) {
  cat("\nError: faltan datos para actualizar a la fecha ingresada\n")
} else {
  #' READ VECTORIAL DATA
  #'   load cluster region
  lyrs <- rgdal::ogrListLayers(link.rgns)
  for (j in 1:length(lyrs)) {
    cat(sprintf("\n%s. %s\n", j, lyrs[j] %>% as.character()))
  }
  num.lyr <- as.integer(readline(prompt = "\nEnter layer[1/2/3/...]: \n"))
  lyrs.rgn <- switch(num.lyr, lyrs[1], lyrs[2], lyrs[3], lyrs[4], lyrs[5])
  k.regions <- as.integer(readline(prompt = "\nEnter rainfall region(s): \n"))

  sf.region <- st_read(
    dsn = link.rgns,
    layer = lyrs.rgn, quiet = T, as_tibble = T
  ) %>%
    filter(gridcode %in% k.regions) %>%
    mutate(id = 1) %>%
    group_by(id) %>%
    summarise()

  #' EXTRACT RASTER VALUES FROM VECTORIAL DATA
  file <- sprintf("%s_avg_MxT_clus%s.RData", k.data, k.regions)
  if (file %in% list.files("data/rdata/", "*.RData")) {
    load(
      sprintf("data/rdata/%s_avg_MxT_clus%s.RData", k.data, k.regions)
    )
    if (length(vls.stk) < length(k.date.prd)) {
      strt <- length(vls.stk) + 1
      end <- length(k.date.prd)
      for (i in strt:end) {
        if (k.data == "era5") {
          vls.day <- raster::extract(raster(lst.data[i]), sf.region) %>%
            lapply(
              function(x) if (!is.null(x)) mean(x, na.rm = TRUE) else NA
            ) %>%
            unlist()
        }
        
        if (k.data == "pisco") {
          vls.day <- raster::extract(grd.data[[i]], sf.region) %>%
            lapply(
              function(x) if (!is.null(x)) mean(x, na.rm = TRUE) else NA
            ) %>%
            unlist()
        }
        
        if (i == strt) vls.mn <- vls.day else vls.mn <- c(vls.mn, vls.day)
      }
      vls.stk <- c(vls.stk, vls.mn)
      save(
        vls.stk,
        file = sprintf("data/rdata/%s_avg_MxT_clus%s.RData", k.data, k.regions)
      )
    }
  } else {
    for (i in 1:length(lst.data)) {
      print(i)
      if (k.data == "era5") {
        vls.day <- raster::extract(raster(lst.data[i]), sf.region) %>%
          lapply(
            function(x) if (!is.null(x)) mean(x, na.rm = TRUE) else NA
          ) %>%
          unlist()
      }
      
      if (k.data == "pisco") {
        vls.day <- raster::extract(grd.data[[i]], sf.region) %>%
          lapply(
            function(x) if (!is.null(x)) mean(x, na.rm = TRUE) else NA
          ) %>%
          unlist()
      }

      if (i == 1) vls.stk <- vls.day else vls.stk <- c(vls.stk, vls.day)
    }
    save(
      vls.stk,
      file = sprintf("data/rdata/%s_avg_MxT_clus%s.RData", k.data, k.regions)
    )
  }

  #' BUILD A DATAFRAME OF TEMPERATURE DATE
  n <- length(seq(as.Date("1981-01-01"), as.Date(k.date.str), by = "1 day"))
  df.rgn.data <- tibble(
    date = k.date.prd,
    values = vls.stk[n:(n - 1 + length(k.date.prd))]
  ) %>%
    mutate(month = str_sub(date, 6, 7)) %>%
    group_by(month) %>%
    mutate(decil = quantile(values, k.thrhld)) %>%
    ungroup() %>%
    mutate(hdf = ifelse(values >= decil, 1, 0)) %>%
    filter(
      date <= as.Date(k.date.end) &
        str_sub(date, 6, 7) %nin% c("01", "02")
    )

  #' DATAFRAME WITH AVERAGE ACCUMULATED HDF BY CLUSTER REGION
  yr.str <- str_sub(k.date.str, 1, 4) %>% as.numeric()
  yr.end <- str_sub(k.date.end, 1, 4) %>% as.numeric()
  k.years <- yr.str:yr.end
  for (i in k.years) {
    if (i == k.years[1]) {
      df.hdf.ac <- df.rgn.data %>%
        filter(substr(date, 1, 4) == i) %>%
        mutate(hdf.ac = cumsum(hdf)) %>%
        dplyr::select(hdf.ac)

      names(df.hdf.ac)[i - (k.years[1] - 1)] <- sprintf("yr.%s", i)
    } else {
      df.hdf.ac <- df.hdf.ac %>%
        mutate(id = 1:n()) %>%
        left_join(
          df.rgn.data %>%
            filter(substr(date, 1, 4) == i) %>%
            mutate(hdf.ac = cumsum(hdf)) %>%
            dplyr::select(hdf.ac) %>%
            mutate(id = 1:n()),
          by = "id"
        ) %>%
        dplyr::select(-id)

      names(df.hdf.ac)[i - (k.years[1] - 1)] <- sprintf("yr.%s", i)
    }
  }

  #' DATAFRAME WITH AVERAGE ACCUMULATED HDF BY CLUSTER REGION, DURING DRY YEARS
  #'   AND NORMAL YEARS
  df.hdf.ac.norm <- df.hdf.ac %>%
    dplyr::select(sprintf("yr.%s", k.years[k.years %nin% c(k.dry.yr, yr.end)]))

  df.hdf <- df.hdf.ac %>%
    mutate(
      hdf.max = df.hdf.ac.norm %>% apply(1, max, na.rm = T),
      hdf.min = df.hdf.ac.norm %>% apply(1, min, na.rm = T),
      hdf.mean = df.hdf.ac.norm %>% apply(1, mean, na.rm = T),
      date = seq(as.Date("2020-03-01"), as.Date("2020-12-31"), by = "1 day")
    ) %>%
    dplyr::select(
      sprintf("yr.%s", c(k.dry.yr, yr.end)),
      hdf.max, hdf.min, hdf.mean, date
    ) %>%
    gather(key = "type", value = "value", -date, -hdf.max, -hdf.min)

  #' DEFINE LABELS FOR LEGEND
  lbls <- c(
    "average years under\nnormal conditions",
    "year 2005", "year 2010", "year 2016", sprintf("year %s", yr.end)
  )

  #' PLOT TEMPORAL EVOLUTION ACCUMULATED HDF
  plt.hdf <- ggplot(df.hdf, aes(date, value, group = type)) +
    labs(
      title = "SEASONAL EVOLUTION OF\nHOT DAY FREQUENCY (HDF)",
      y = "accumulated from hot days"
    ) +
    geom_ribbon(
      aes(ymin = hdf.min, ymax = hdf.max),
      size = .2, fill = "gray", color = "black", alpha = .1
    ) +
    geom_line(aes(linetype = type, color = type, size = type)) +
    scale_linetype_manual(
      values = c("dashed", "solid", "solid", "solid", "solid"), labels = lbls
    ) +
    scale_color_manual(
      values = c("gray", "blue", "black", "green", "red"),
      labels = lbls
    ) +
    scale_size_manual(values = rep(.8, 5), labels = lbls) +
    scale_x_date(
      limits = c(as.Date("2020-03-01"), as.Date("2020-12-31")),
      breaks = seq(as.Date("2020-03-01"), as.Date("2020-12-31"), by = "1 month"),
      date_labels = "%b", expand = expand_scale(mult = c(.02, 0))
    ) +
    scale_y_continuous(
      breaks = seq(0, 150, 20),
      limits = c(-.003, 150),
      expand = expand_scale(mult = c(0, 0))
    ) +
    annotation_ticks(
     sides = "l",
     ticklength = 1 * unit(0.1, "cm"),
     color = "black"
    ) +
    coord_cartesian(clip = "off") +
    theme_bw() +
    theme(
      legend.background = element_rect(fill = "white", color = "black", size = .3),
      legend.margin = margin(.25, 4, 6, 4),
      legend.key.width = unit(.35, "cm"),
      legend.key.height = unit(.25, "cm"),
      legend.position = c(0.25, 0.85),
      legend.title = element_blank(),
      legend.text = element_text(size = 8, family = "Source Sans Pro"),
      plot.title = element_text(size = 12, hjust = 1, family = "Source Sans Pro"),
      axis.text.x = element_text(
        size = 10, colour = "black", family = "Source Sans Pro",
        face = "bold", angle = 45, hjust = -.3, vjust = .1
      ),
      axis.text.y = element_text(
        size = 12, face = "bold", family = "Source Sans Pro", color = "black"
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_text(
        size = 12, face = "bold", family = "Source Sans Pro", color = "black"
      ),
      axis.ticks.x = element_line(color = "black"),
      axis.ticks.y = element_line(color = "black"),
      panel.grid = element_blank(),
      panel.border = element_rect(size = .5, color = "black"),
      plot.margin = margin(1.5, .1, 1, 1, "cm"),
      axis.line.y = element_line(
        size = .5, color = "black"
      )
    )
  plt.hdf
  ggsave(
    plot = plt.hdf,
    sprintf(
      "exports/%s_clus%s_hdf_from_%s_to_%s.png",
      k.data, k.regions, k.date.str, k.date.end
    ),
    width = 10, height = 12, units = "cm", dpi = 1000
  )
}