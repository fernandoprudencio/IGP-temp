#' @title
#' Calculate and plot PBIAS between ERA5 (warp) and PISCOv1.1 (base) temperature
#'   data
#'
#' @description
#' this script calculate and plot PBIAS between ERA5 (warp) and PISCOv1.1 (base)
#'   temperature data for average of july, august, september, ovtober and november
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "ncdf4", "sf", "lattice", "extrafont",
  "cptcity", "latticeExtra", "rasterVis", "maptools", "grid"
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
library(raster)
library(ncdf4)
library(sf)
library(lattice)
library(extrafont)
library(cptcity)
library(latticeExtra)
library(rasterVis)
library(maptools)
library(grid)

#' CONSTANTS
k.dep <- c(
  "Piura", "Cajamarca", "La Libertad", "Ancash", "Loreto", "Huancavelica",
  "Amazonas", "Madre de Dios", "Cusco", "Apurimac", "Puno", "Huanuco", "Pasco",
  "Junin"
)
k.cond <- "dry" # dry or normal conditions
k.month <- 7:11

#' READ RASTER DATA
pisco <- brick(
  sprintf(
    "data/raster/pisco/climatology/pisco_mnthlyMEANtemp_clim%s_1981-2016.nc",
    str_sub(k.cond, 1, 4) %>% toupper()
  )
)[[k.month]] %>%
  mean(na.rm = T)

era <- brick(
  sprintf(
    "data/raster/era5/climatology/era5_mnthlyMEANtemp_clim%s_1981-2019.nc",
    str_sub(k.cond, 1, 4) %>% toupper()
  )
)[[k.month]] %>%
  mean(na.rm = T) %>%
  crop(pisco) %>%
  resample(pisco)

#' CALCULATE PBIAS
pbias <- (((era - pisco) * 100) / pisco)
writeRaster(
  pbias,
  sprintf(
    "data/raster/pbias/pbias_era5_and_pisco_cond%s.tif",
    str_sub(k.cond, 1, 4) %>% toupper()
  ),
  overwrite = T
)

#' DEFINE COLOR PALETTE
cb.palette <-
  c(
    "#030B1A", "#030B1A", "#030B1A", "#030B1A", "#071E46", "#072E69", "#074F99",
    "#2171B5", "#4292C7", "#5AA0CD", "#78BFD6", "#AADCE6", "#FFFFFF", "#FFFFFF",
    "#FFFFFF", "#FFFFFF", "#FCBBAA", "#FC9272", "#FB6A4A", "#F03C2B", "#CC181E",
    "#A30E13", "#77090E", "#5F0000", "#420000", "#420000", "#420000", "#420000"
  )

#' LOAD VECTOR DATA TO PLOT WHIT RASTER OF TEMPERATURE
#'   load world countries limits
#'     load sf data
sf.world <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "world_countries", quiet = T, as_tibble = T
)
#'     load sp data
sp.world <- as(st_geometry(sf.world), Class = "Spatial")

#'   load Peru departaments points
#'     load sf data
sf.peru <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "peru_departaments", quiet = T, as_tibble = T
) %>%
  st_centroid(of_largest_polygon = FALSE) %>%
  filter(Departamen %in% k.dep) %>%
  mutate(Departamen = as.character(Departamen))
#'     load sp data
sp.peru <- as(st_geometry(sf.peru), Class = "Spatial")

#'   load Peru departaments polygon
#'     load sf data
sf.dep <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "peru_departaments", quiet = T, as_tibble = T
)
#'     load sp data
sp.dep <- as(st_geometry(sf.dep), Class = "Spatial")



#' SAVE PLOT
png(
  sprintf(
    "exports/PBIAS_era5_and_pisco_cond%s.png",
    str_sub(k.cond, 1, 4) %>% toupper()
  ),
  res = 1000, height = 28, width = 20, units = "cm"
)

levelplot(pbias,
  # col.regions = rev(cb_palette),
  main = list(
    sprintf(
      "PBIAS between ERA5 and PISCOv1.1\ntemperature data in %s conditions\nfrom 1981 to 2016 (jul - nov)",
      k.cond
    ),
    side = 1, line = 0.5, fontfamily = "Source Sans Pro"
  ),
  scales = list(
    x = list(limits = c(-81.8, -68.2)),
    y = list(limits = c(-18.7, .4))
  ),
  col.regions = rev(cb.palette),
  margin = F,
  pretty = T,
  maxpixels = 15e6,
  at = seq(-70, 70, 5),
  colorkey = list(
    at = seq(-70, 70, 5),
    space = "right", # location of legend
    labels = list(at = seq(-50, 50, 10), cex = 1.1),
    font = list(family = "Source Sans Pro")
  ),
  xlab = NULL,
  ylab = NULL,
  par.settings = list(
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    par.xlab.text = list(fontfamily = "Source Sans Pro"),
    par.ylab.text = list(fontfamily = "Source Sans Pro"),
    par.main.text = list(fontfamily = "Source Sans Pro"),
    par.sub.text = list(fontfamily = "Source Sans Pro")
  )
) +
  latticeExtra::layer(
    sp.lines(sp.world, col = "black", lwd = 2),
    # sp.lines(sp.dep, col = "black", lwd = .8),
    sp.points(sp.peru, pch = 20, cex = 1, col = "black"),
    sp.text(
      coordinates(sp.peru),
      txt = sf.peru$Departamen, pos = 1, cex = 1.2,
      fontfamily = "Source Sans Pro"
    )
  )

grid::grid.text(
  "[%]",
  x = unit(.928, "npc"),
  y = unit(.91, "npc"),
  rot = 0,
  gp = gpar(
    fontsize = 18,
    fontface = "bold",
    fontfamily = "Source Sans Pro",
    col = "black"
  )
)

#' CLOSE THE SAVED OF PLOT
dev.off()