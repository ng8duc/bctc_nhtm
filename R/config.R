library(tidyverse)
library(here)
library(glue)
library(jsonlite)
library(plotly)
library(dotenv)
library(highcharter)

load_dot_env()

source("R/pull_from_d1.R")

D1_DATABASE <- Sys.getenv("D1_DATABASE")

# Cấu hình định dạng số kiểu Việt Nam
# (Dấu phẩy phân tách thập phân, dấu chấm phân tách phần nghìn)
lang_opts <- getOption("highcharter.lang")
if (is.null(lang_opts)) lang_opts <- list()
lang_opts$decimalPoint <- ","
lang_opts$thousandsSep <- "."
options(highcharter.lang = lang_opts)
