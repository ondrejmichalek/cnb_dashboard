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
library(scales)
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
# options(OutDec= ",")
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
pr_p <- readRDS("pr_p.rds")
nowcast_hdp_all <- readRDS("dashboard_nowcast_hdp.rds")
nowcast_dfm_all <- readRDS("dashboard_nowcast_dfm_hdp.rds")
pr <- readRDS("pr.rds")
p604_sa <- readRDS("p604_sa.rds")
labels <- read_xlsx("labels.xlsx")
translator <- read_xlsx("translator.xlsx")

## setup global variables
max_date_pr <- max(pr_p$date)
max_date_ea <- max(nowcast_hdp_all$Date)
tmp_zam <- 8.9
tmp_mzdy <- 7.0
update_nowcast_hdp <- file.info("dashboard_nowcast_hdp.rds")$mtime
update_pr <- file.info("pr.rds")$mtime
period <- "2023q1"

nowcast_hdp_all1 <- nowcast_hdp_all %>%
  mutate(name = as.character(name))
