#' this function obtains a binary number with a defined digits number "bits"
binND <- function(number, nbits) { # number in binary system, nbits is digits number
  nzeros <- nbits - nchar(number)
  return(paste(substr("0000000000000000", 1, nzeros), number, sep = ""))
}

#' this function filters MODIS dataset by quality band
#' this is the order of 16 bits of the quality band
# (15)(14)(13)(12)(11)(10)(09)(08)(07)(06)(05)(04)(03)(02)(01)(00) - MODIS NOMENCLATURE
# (01)(02)(03)(04)(05)(06)(07)(08)(09)(10)(11)(12)(13)(14)(15)(16) - R NOMENCLATURE
#'
qaFilter <- function(band, qaband, type, filter) {
  dataBIN <- sapply(DecToBin(c(1:65535)), FUN = function(x) {
    binND(x, nbits = 16)
  }) %>% as.character()
  if (type == "mxd09a1") {
    dataBIN_df <- data.frame(bin = dataBIN) %>%
      mutate(dec = c(1:length(bin))) %>%
      filter(substr(bin, 15, 16) %in% filter[[1]] | # Cloud State
               substr(bin, 14, 14) %in% filter[[2]] | # Cloud shadow
               substr(bin, 11, 13) %in% filter[[3]] | # Land/Water Flag
               substr(bin, 9, 10) %in% filter[[4]] | # Aerosol Quantity
               substr(bin, 7, 8) %in% filter[[5]] | # Cirrus detected
               substr(bin, 6, 6) %in% filter[[6]] | # Internal Cloud Algorithm Flag
               substr(bin, 5, 5) %in% filter[[7]] | # Internal Fire Algorithm
               substr(bin, 4, 4) %in% filter[[8]] | # MOD35 snow/ice flag
               substr(bin, 3, 3) %in% filter[[9]] | # Pixel adjacent to cloud
               substr(bin, 2, 2) %in% filter[[10]] | # BRDF correction performed
               substr(bin, 1, 1) %in% filter[[11]]) # Internal Snow Mask
  }
  #' changing the values of the quality band to NA and 1
  qaband[qaband %in% dataBIN_df$dec] <- NA
  qaband[!is.na(qaband)] <- 1
  return(band * qaband)
}

#' this function calculates different indexes related with vegetation
indexMODIS <- function(index, redBand = NULL, nirBand = NULL,
                       blueBand = NULL, greenBand = NULL,
                       swir1Band = NULL, swir2Band = NULL,
                       swir3Band = NULL)
{
  if (index == "ndvi") {
    return((nirBand - redBand) / (nirBand + redBand))
  }
  if (index == "savi") {
    return((nirBand - redBand) * (1 + 0.25) / (nirBand + redBand + 0.25))
  }
  if (index == "ndii") {
    return((nirBand - swir2Band) / (nirBand + swir2Band))
  }
  if (index == "gemi") {
    eta <- ((2 * ((nirBand^2) - (redBand^2))) + (1.5 * nirBand) + (0.5 * redBand)) / (nirBand + redBand + 0.5)
    return((eta * (1 - (0.25 * eta))) - ((redBand - 0.125) / (1 - redBand)))
  }
  if (index == "ndwi") {
    return((nirBand - swir1Band) / (nirBand + swir1Band))
  }
  if (index == "vari") {
    return((greenBand - redBand) / (greenBand + redBand - blueBand))
  }
  if (index == "evi") {
    return(2.5 * (nirBand - redBand) / (nirBand + (6 * redBand) - (7.5 * blueBand) + 1))
  }
  if (index == "gvmi") {
    return(((nirBand + 0.1) - (swir2Band + 0.02)) / ((nirBand + 0.1) + (swir2Band + 0.02)))
  }
}

#' this function plots histogram
histPLOT <- function(x, field, width, fill, col, title, subtitle) {
  plt <- ggplot(x, aes(x %>% dplyr::select(field))) +
    geom_histogram(binwidth = width, fill = fill, col = col) +
    theme_bw() +
    labs(x = "", y = "") +
    # ggtitle(title, subtitle) +
    theme(
      plot.title = element_text(size = 15),
      plot.subtitle = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 20),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    ) +
    scale_y_continuous(breaks = seq(0, 200, 25), limits = c(0, 200), expand = c(0, 0))
  return(plt)
}

#' this function extrats average value of raster by polygon vector
extract_data <- function(file, st) {
  return(file %>% mask(st) %>% getValues() %>% mean(na.rm = T))
}

#' this function return a logic value if it is an outlier vlaue or no
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#' this function return a vector to reclassify a raster data
rcls_vector <- function(minval, maxval, inter) {
  n <- if (minval %% 100 == 0) {
    c(seq(minval, maxval, inter), maxval)
  } else {
    c(minval, seq(minval + (100 - (minval %% 100)), maxval, inter), maxval)
  }
  for (i in 1:(length(n) - 1)) {
    if (i == 1) {
      rcls <- c(n[i], n[i + 1], i)
    } else {
      rcls <- c(rcls, n[i], n[i + 1], i)
    }
  }
  return(rcls)
}

#' this function adds minor tick marks in a plot
annotation_ticks <- function(sides = "b",
                             scale = "identity",
                             scaled = TRUE,
                             ticklength = unit(0.1, "cm"),
                             colour = "black",
                             size = 0.5,
                             linetype = 1,
                             alpha = 1,
                             color = NULL,
                             ticks_per_base = NULL,
                             ...) {
  if (!is.null(color)) {
    colour <- color
  }
  
  # check for invalid side
  if (grepl("[^btlr]", sides)) {
    stop(gsub("[btlr]", "", sides), " is not a valid side: b,t,l,r are valid")
  }
  
  # split sides to character vector
  sides <- strsplit(sides, "")[[1]]
  
  if (length(sides) != length(scale)) {
    if (length(scale) == 1) {
      scale <- rep(scale, length(sides))
    } else {
      stop("Number of scales does not match the number of sides")
    }
  }
  
  base <- sapply(scale, function(x) switch(x, "identity" = 10, "log10" = 10, "log" = exp(1)), USE.NAMES = FALSE)
  
  if (missing(ticks_per_base)) {
    ticks_per_base <- base - 1
  } else {
    if ((length(sides) != length(ticks_per_base))) {
      if (length(ticks_per_base) == 1) {
        ticks_per_base <- rep(ticks_per_base, length(sides))
      } else {
        stop("Number of ticks_per_base does not match the number of sides")
      }
    }
  }
  
  delog <- scale %in% "identity"
  
  layer(
    data = data.frame(x = NA),
    mapping = NULL,
    stat = StatIdentity,
    geom = GeomTicks,
    position = PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      base = base,
      sides = sides,
      scaled = scaled,
      ticklength = ticklength,
      colour = colour,
      size = size,
      linetype = linetype,
      alpha = alpha,
      ticks_per_base = ticks_per_base,
      delog = delog,
      ...
    )
  )
}

#' Base ggproto classes for ggplot2
#'
#' If you are creating a new geom, stat, position, or scale in another package,
#' you'll need to extend from ggplot2::Geom, ggplot2::Stat, ggplot2::Position, or ggplot2::Scale.
#'
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
#' @usage NULL
#' @format NULL
#' @rdname ggplot2-ggproto
#' @export
GeomTicks <- ggproto(
  "GeomTicks", Geom,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },
  
  draw_panel = function(data,
                        panel_scales,
                        coord,
                        base = c(10, 10),
                        sides = c("b", "l"),
                        scaled = TRUE,
                        ticklength = unit(0.1, "cm"),
                        ticks_per_base = base - 1,
                        delog = c(x = TRUE, y = TRUE)) {
    ticks <- list()
    
    for (s in 1:length(sides)) {
      if (grepl("[b|t]", sides[s])) {
        
        xticks <- panel_scales$x.minor
        
        # Make the grobs
        if (grepl("b", sides[s])) {
          ticks$x_b <- with(
            data,
            segmentsGrob(
              x0 = unit(xticks, "npc"),
              x1 = unit(xticks, "npc"),
              y0 = unit(0, "npc"),
              y1 = ticklength,
              gp = gpar(
                col = alpha(colour, alpha),
                lty = linetype,
                lwd = size * .pt
              )
            )
          )
        }
        if (grepl("t", sides[s])) {
          ticks$x_t <- with(
            data,
            segmentsGrob(
              x0 = unit(xticks, "npc"),
              x1 = unit(xticks, "npc"),
              y0 = unit(1, "npc"),
              y1 = unit(1, "npc") - ticklength,
              gp = gpar(
                col = alpha(colour, alpha),
                lty = linetype,
                lwd = size * .pt
              )
            )
          )
        }
      }
      
      
      if (grepl("[l|r]", sides[s])) {
        
        yticks <- panel_scales$y.minor
        
        # Make the grobs
        if (grepl("l", sides[s])) {
          ticks$y_l <- with(
            data,
            segmentsGrob(
              y0 = unit(yticks, "npc"),
              y1 = unit(yticks, "npc"),
              x0 = unit(0, "npc"),
              x1 = ticklength,
              gp = gpar(
                col = alpha(colour, alpha),
                lty = linetype, lwd = size * .pt
              )
            )
          )
        }
        if (grepl("r", sides[s])) {
          ticks$y_r <- with(
            data,
            segmentsGrob(
              y0 = unit(yticks, "npc"),
              y1 = unit(yticks, "npc"),
              x0 = unit(1, "npc"),
              x1 = unit(1, "npc") - ticklength,
              gp = gpar(
                col = alpha(colour, alpha),
                lty = linetype,
                lwd = size * .pt
              )
            )
          )
        }
      }
    }
    gTree(children = do.call("gList", ticks))
  },
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = 1)
)


# Calculate the position of log tick marks Returns data frame with: - value: the
# position of the log tick on the data axis, for example 1, 2, ..., 9, 10, 20, ...
# - start: on the other axis, start position of the line (usually 0) - end: on the
# other axis, end position of the line (for example, .1, .2, or .3)
calc_ticks <- function(base = 10,
                       ticks_per_base = base - 1,
                       minpow = 0,
                       maxpow = minpow + 1,
                       majorTicks = 0,
                       start = 0,
                       shortend = 0.1,
                       midend = 0.2,
                       longend = 0.3,
                       delog = FALSE) {
  
  # Number of blocks of tick marks
  reps <- maxpow - minpow
  
  # For base 10: 1, 2, 3, ..., 7, 8, 9, 1, 2, ...
  ticknums <- rep(seq(1, base - 1, length.out = ticks_per_base), reps)
  
  # For base 10: 1, 1, 1, ..., 1, 1, 1, 2, 2, ... (for example)
  powers <- rep(seq(minpow, maxpow - 1), each = ticks_per_base)
  
  ticks <- ticknums * base^powers
  
  ticks <- c(ticks, base^maxpow) # Add the last tick mark
  
  # Set all of the ticks short
  tickend <- rep(shortend, length(ticks))
  
  # Get the position within each cycle, 0, 1, 2, ..., 8, 0, 1, 2. ...
  cycleIdx <- ticknums - 1
  
  # Set the 'major' ticks long
  tickend[cycleIdx == 0] <- longend
  
  # Where to place the longer tick marks that are between each base For base 10, this
  # will be at each 5
  longtick_after_base <- floor(ticks_per_base / 2)
  tickend[cycleIdx == longtick_after_base] <- midend
  
  if (delog) {
    ticksCopy <- ticks
    
    regScale <- log(ticks, base)
    
    majorTicks <- sort(
      unique(
        c(
          minpow,
          regScale[which(regScale %in% majorTicks)],
          maxpow,
          majorTicks
        )
      )
    )
    
    expandScale <- c()
    
    if (length(majorTicks) > 1) {
      for (i in 1:(length(majorTicks) - 1)) {
        expandScale <- c(
          expandScale,
          seq(majorTicks[i], majorTicks[i + 1], length.out = (ticks_per_base + 1))
        )
      }
      
      ticks <- unique(expandScale)
      
      # Set all of the ticks short
      tickend <- rep(shortend, length(ticks))
      
      # Set the 'major' ticks long
      tickend[which(ticks %in% majorTicks)] <- longend
    }
  }
  
  tickdf <- data.frame(value = ticks, start = start, end = tickend)
  
  tickdf
}