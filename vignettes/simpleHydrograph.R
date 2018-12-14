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
library(ggplot2)
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

## ---- fig.width = 5, fig.height = 3--------------------------------------
calEnd <- "2010-01-01"
p <- simpleHydrograph(MESH_streamflows, stationNames = station_names, 
                       byStation = FALSE, calEnd = calEnd)
p <- p + facet_grid(station~period) + scale_x_date(date_labels = "%Y")
p

## ---- fig.width = 5, fig.height = 3--------------------------------------
plotcols <- c("red", "blue")
p1 <- p + facet_grid(station~period, scales = "free") + 
  scale_x_date(date_labels = "%Y") +
  scale_colour_manual(values = plotcols)
p1

## ------------------------------------------------------------------------
stats <- hydroStats(MESH_streamflows, stationNames = station_names)
stats

## ------------------------------------------------------------------------
NSE <- paste(stats$station, "NSE =", stats$NSE, sep = " ")
NSE

## ------------------------------------------------------------------------
all_NSE <- paste(NSE, collapse = ", ")
all_NSE

## ---- fig.width = 5, fig.height = 3--------------------------------------
p2 <- simpleHydrograph(MESH_streamflows, stationNames = station_names, byStation = TRUE) +
  scale_x_date(date_labels = "%Y")

dateLoc <- as.Date("2005-01-01", format = "%Y-%m-%d")

p3 <- p2 + annotate("text", x = dateLoc, y = 1500, label = all_NSE, hjust = "left")
p3

## ---- fig.width = 5, fig.height = 3--------------------------------------
wrapped_NSE <- paste(NSE, collapse = "\n")
p4 <- p2 + annotate("text", x = dateLoc, y = 1500, label = wrapped_NSE, hjust = "left")
p4

## ---- fig.width = 5, fig.height = 3--------------------------------------
p5 <- simpleHydrograph(MESH_streamflows, stationNames = station_names, byStation = FALSE)
p5 <- p5 + facet_wrap(~station, nrow = 2) + scale_x_date(date_labels = "%Y")
p5 <- p5 + annotate("text", x = dateLoc, y = 1500, label = NSE, hjust = "left")
p5

## ------------------------------------------------------------------------
stats2 <- hydroStats(MESH_streamflows, stationNames = station_names, calEnd = calEnd)
stats2

## ---- fig.width = 5, fig.height = 3--------------------------------------
facetted_NSE <- paste("NSE = ", stats2$NSE[order(stats2$station)])
p6 <- p1 + facet_grid(station~period) + 
  scale_x_date(date_labels = "%Y") +
  scale_colour_manual(values = plotcols) + 
  annotate("text", x = dateLoc, y = 1500, label = facetted_NSE, hjust = "left")
p6

