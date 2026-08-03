df <- df_nim %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(
    yq == max(yq),
    nchar(name) == 3
  ) %>% 
  mutate(mau_cot = ifelse(name != "BID", "#006b68", "#fdb71a"))

chart_nim_rieng <- highchart() %>% 
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
    name = "NIM (%)",
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
  hc_legend(
    align = "center",
    verticalAlign = "top",
    layout = "horizontal",
    symbolRadius = 0
  ) %>%
  hc_chart(zoomType = "x") %>% 
  hc_title(
    text = "NIM của 10 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = ifelse(
      month(max(df$yq)) == 10,
      "Bình quân cả năm",
      str_glue("Bình quân {month(max(df$yq)) + 2} tháng đầu năm {year(max(df$yq))}, annualized")
    ),
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  ) %>% hc_export_menu()
