## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(MESHr)
library(ggplot2)
MESH_streamflows <- MESH_streamflows

## ---- fig.width = 5, fig.height = 3--------------------------------------
simpleHydrograph(MESH_streamflows)

## ---- fig.width = 5, fig.height = 3--------------------------------------
station_names <- c("Station 1", "Station 2")
simpleHydrograph(MESH_streamflows, stationNames = station_names)

## ---- fig.width = 5, fig.height = 3--------------------------------------
p <- simpleHydrograph(MESH_streamflows, stationNames = station_names, byStation = FALSE)
p <- p + facet_wrap(~station, nrow = 2) + scale_x_date(date_labels = "%Y")
p

## ---- fig.width = 5, fig.height = 3--------------------------------------
p <- p + facet_wrap(~station, nrow = 2, scales = "free_y") 
p

## ---- fig.width = 5, fig.height = 3--------------------------------------
p <- simpleHydrograph(MESH_streamflows, stationNames = station_names, byYear = TRUE)
p <- p + facet_wrap(~YEAR, scales = "free_y")
p

## ---- fig.width = 5, fig.height = 6--------------------------------------
p <- simpleHydrograph(MESH_streamflows, stationNames = station_names, byStation = FALSE, byYear = TRUE)
p <- p + facet_grid(YEAR~station, scales = "free_y")
p

## ---- fig.width = 5, fig.height = 6--------------------------------------
plotcols <- c("red", "blue")
p <- p + scale_colour_manual(values = plotcols) +
  theme(strip.text.y = element_text(angle = 0))
p

## ---- fig.width = 5, fig.height = 6--------------------------------------
p <- simpleHydrograph(MESH_streamflows, stationNames = station_names, 
                      byStation = FALSE, byYear = TRUE, meas = FALSE) +
  facet_grid(YEAR~station, scales = "free_y") +
  scale_colour_manual(values = plotcols) +
  theme(strip.text.y = element_text(angle = 0)) +
  theme(legend.position = "none")

p

