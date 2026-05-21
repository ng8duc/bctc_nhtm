df <- df_roa %>%
  filter(nchar(name) == 3,
         !is.na(value)) %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(yq == max(yq)) %>% 
  mutate(mau_cot = ifelse(name == "BID", "#fdb71a", "#006b68"))

chart_roa_rieng <- highchart() %>% 
  hc_yAxis(
    title = list(text = "%"),
    gridLineColor = "#e6e6e6"
  ) %>% 
  hc_xAxis(
    categories = df$name,
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6"
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = name, y = value * 100, color = mau_cot),
    type = "column",
    name = "Tỷ lệ ROA (%)",
    color = "#006b68",
    yAxis = 0,
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y:,.2f}", # Hiển thị nhãn giá trị với 2 chữ số thập phân
      style = list(fontSize = "10px")
    ),
    tooltip = list(
      valueSuffix = "%"
    )
  ) %>% 
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE,
    valueDecimals = 2
  ) %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_legend(
    align = "center",
    verticalAlign = "top",
    layout = "horizontal",
    symbolRadius = 0
  ) %>%
  hc_chart(zoomType = "x") %>% 
  hc_title(
    text = "Tỷ lệ ROA của 10 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  ) %>% 
  hc_subtitle(
    text = ifelse(
      month(max(df$yq)) == 10,
      "Bình quân cả năm",
      str_glue("Bình quân {month(max(df$yq)) + 2} tháng đầu năm {year(max(df$yq))}, annualized")
    ),
    style = list(fontStyle = "italic", color = "#666666")
  )
