### Share load should be sourced by both ui and server.
##  load library --------------------
library(rjson)
library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(lubridate)
library(stringr)
library(withr)
library(treemap)
library(DT)
library(shinyBS)
library(shinyjs)
library(WDI)
library(geosphere)
library(magrittr)
library(shinycssloaders)
options(spinner.color="#006272")
library(timevis)
#library(RCurl)
#library(jsonlite)
# library(comtradr)
library(memoise)
library(networkD3)
library(promises)
library(future)

## load functions
source('helper_funs.R')

## load data
pr_p <- readRDS("pr_p.rds") ## pre-defined commodity list form SNZ
labels <- read_xlsx("labels.xlsx")

## setup global variables
max_date <- max(pr_p$date)

