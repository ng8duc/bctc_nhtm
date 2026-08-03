df <- df_chenh_lech_thu_chi %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(nchar(name) == 3,
         month(yq) == month(max(yq))) %>% 
  group_by(name) %>% 
  arrange(yq) %>% 
  mutate(cltc_yoy = value/lag(value)*100-100) %>% 
  rename(cltc := value) %>% 
  ungroup() %>% 
  filter(yq == as.Date(max(yq))) %>% 
  mutate(mau_cot = ifelse(name != "BID", "#006b68", "#fdb71a")) %>% 
  arrange(desc(cltc))

chart_cltc_rieng <- highchart() %>%
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
  )%>%
  hc_xAxis(
    categories = df$name,
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6"
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = name, y = cltc / 1000, color = mau_cot),
    type = "column",
    name = "Chênh lệch thu chi",
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
    data = df,
    mapping = hcaes(x = name, y = cltc_yoy),
    type = "line",
    lineWidth = 0,
    marker = list(enabled = TRUE, radius = 5),
    name = "Tăng trưởng YOY",
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
    text = "Chênh lệch thu chi của 10 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = ifelse(
      month(max(df$yq)) == 10,
      "Lũy kế cả năm",
      str_glue("Lũy kế {month(max(df$yq)) + 2} tháng đầu năm {year(max(df$yq))}")
    ),
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  ) %>% hc_export_menu()
