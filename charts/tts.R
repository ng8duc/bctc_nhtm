df <- df_tts

df <- df %>% mutate(yq = as.Date(yq))

df <- df %>%
  group_by(name) %>%
  arrange(yq) %>%
  mutate(
    yoy_growth_rate = value / lag(value, 4) - 1,
    ytd_growth_rate = value / value[match(as.Date(paste0(as.integer(format(yq, "%Y")) - 1, "-10-01")), yq)] - 1
  ) %>%
  ungroup()

df_long <- df %>%
  filter(nchar(name) == 3) %>%
  filter(yq == max(yq)) %>%
  arrange(desc(value)) %>%
  mutate(
    mau_cot = ifelse(name != "BID", "#006b68", "#fdb71a"),
    ytd_growth_rate = ytd_growth_rate * 100
  )

chart_tts <- highchart() %>%
  hc_yAxis_multiples(
    list(
      title = list(text = "nghìn tỷ đồng"),
      gridLineColor = "#e6e6e6"
    ),
    list(
      title = list(text = NULL),
      opposite = TRUE,
      gridLineWidth = 0, # Ẩn vạch kẻ ngang của trục thứ hai để tránh rối mắt
      labels = list(format = "{value:,.1f}%")
    )
  ) %>%
  hc_xAxis(
    categories = df_long$name,
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6"
  ) %>%
  hc_add_series(
    data = df_long,
    mapping = hcaes(x = name, y = value / 1000, color = mau_cot),
    type = "column",
    name = "Tổng tài sản",
    color = "#006b68",
    yAxis = 0,
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y:,.2f}", # Hiển thị nhãn giá trị với 2 chữ số thập phân
      style = list(fontSize = "10px")
    ),
    tooltip = list(
      valueSuffix = " nghìn tỷ đồng"
    )
  ) %>%
  hc_add_series(
    data = df_long,
    mapping = hcaes(x = name, y = ytd_growth_rate),
    type = "line",
    lineWidth = 0,
    marker = list(enabled = TRUE, radius = 5),
    name = "Tăng trưởng YTD",
    yAxis = 1,
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y:,.2f}%",
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
    text = "Tổng tài sản của 10 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = str_glue("Ngày số liệu: {strftime(max(df_long$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  ) %>% hc_export_menu()
