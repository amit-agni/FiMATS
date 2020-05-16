rm(list = ls())

library(here)
library(data.table)
library(tidyverse)

library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinycssloaders) #spinner busy
library(shinyWidgets) #sweetalert #switchinput

library(quantmod)
library(tidyquant)

library(kableExtra)
library(gridExtra)
library(tableHTML) #make_css
library(ggrepel)
library(batchtools)
library(assertive.types) #needed for gg_facet_nrow()

library(plotly)
library(DT)

PLOT_HEIGHT <- 500
FACET_ROW_HEIGHT <- 250
NO_OF_VALUE_BOXES <- 12
DATA_FOLDER <- "data"

#Load the UI, Server and Misc project functions
lapply(here::here('source',list.files(here::here('source'),pattern = '.R')),source)

DT_stats <- reactiveVal(readRDS(file = here::here(DATA_FOLDER, "DT_stats.Rds")))
DT_hist <- reactiveVal(readRDS(file = here::here(DATA_FOLDER, "DT_hist.Rds")))
DT_myShares <- reactiveVal(readRDS(file = here::here(DATA_FOLDER, "DT_myShares.Rds")))
DT_realTime <- reactiveVal(data.frame())

# DT_stats <- reactive(readRDS(file=here::here("100_data_raw-input","DT_stats.Rds")))
# DT_hist <- reactive(readRDS(file=here::here("100_data_raw-input","DT_hist.Rds")))
# DT_myShares <- reactive(readRDS(file=here::here("100_data_raw-input","DT_myShares.Rds")))

