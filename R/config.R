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

# Chuyển hết font chữ thành Arial
my_theme <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    chart = list(style = list(fontFamily = "Arial, sans-serif", fontSize = "14px")),
    title = list(style = list(fontFamily = "Arial, sans-serif")),
    subtitle = list(style = list(fontFamily = "Arial, sans-serif")),
    xAxis = list(labels = list(style = list(fontFamily = "Arial, sans-serif", fontSize = "18px"))),
    yAxis = list(labels = list(style = list(fontFamily = "Arial, sans-serif", fontSize = "14px"))),
    legend = list(itemStyle = list(fontFamily = "Arial, sans-serif", fontSize = "14px")),
    tooltip = list(style = list(fontFamily = "Arial, sans-serif", fontSize = "14px")),
    plotOptions = list(
      series = list(dataLabels = list(style = list(fontFamily = "Arial, sans-serif", fontSize = "14px"))),
      # Cột trong cùng nhóm sát nhau (pointPadding = 0), vẫn giữ khoảng cách giữa các nhóm (groupPadding)
      column = list(groupPadding = 0.1, pointPadding = 0)
    )
  )
)

options(highcharter.theme = my_theme)

# Thêm nút export (Fullscreen/PNG/Excel) chạy offline (không cần export server)
# Dùng: chart %>% hc_export_menu()
hc_export_menu <- function(hc) {
  hc %>%
    hc_exporting(
      enabled = TRUE,
      fallbackToExportServer = FALSE,
      sourceWidth = 700,
      sourceHeight = 500,
      buttons = list(
        contextButton = list(
          menuItems = list("viewFullscreen", "downloadPNG", "downloadXLS")
        )
      )
    ) %>%
    hc_add_dependency("modules/exporting.js") %>%
    hc_add_dependency("modules/offline-exporting.js") %>%
    hc_add_dependency("modules/export-data.js")
}


