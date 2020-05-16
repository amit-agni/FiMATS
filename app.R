#if(!require(pacman)) { install.packages("pacman"); library(pacman)}
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


#Source the UI, Server and Misc project functions are kept here :
lapply(here::here('210_src_R-scripts-functions'
                  ,list.files(here::here('210_src_R-scripts-functions'),pattern = '.R'))
       ,source)


# source(here::here("210_src_R-scripts-functions","fns_dataRefresh.R"))
# source(here::here("210_src_R-scripts-functions","fns_labs.R"))
# source(here::here("210_src_R-scripts-functions","fns_marketOverview.R"))
# source(here::here("210_src_R-scripts-functions","fns_miscProjectFunctions.R"))
# source(here::here("210_src_R-scripts-functions","fns_myInvestments.R"))
# source(here::here("210_src_R-scripts-functions","fns_technicalAnalysis.R"))



DT_stats <- reactiveVal(readRDS(file = here::here("100_data_raw-input", "DT_stats.Rds")))
DT_hist <- reactiveVal(readRDS(file = here::here("100_data_raw-input", "DT_hist.Rds")))
DT_myShares <- reactiveVal(readRDS(file = here::here("100_data_raw-input", "DT_myShares.Rds")))
DT_realTime <- reactiveVal(data.frame())



#source(here::here("210_src_R-scripts-functions","uiElements.R"))

# DT_stats <- reactive(readRDS(file=here::here("100_data_raw-input","DT_stats.Rds")))
# DT_hist <- reactive(readRDS(file=here::here("100_data_raw-input","DT_hist.Rds")))
# DT_myShares <- reactive(readRDS(file=here::here("100_data_raw-input","DT_myShares.Rds")))


PLOT_HEIGHT <- 500
FACET_ROW_HEIGHT <- 250
NO_OF_VALUE_BOXES <- 12







shinyApp(ui,server)