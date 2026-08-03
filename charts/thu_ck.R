df <- bind_rows(df_thu_rong_tu_ck_dau_tu, df_thu_rong_tu_ck_kinh_doanh)

df <- df %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(nchar(name) == 3,
         yq == max(yq))

df <- df %>% 
  group_by(yq, name) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  mutate(mau_cot = ifelse(name == "BID", "#fdb71a", "#006b68")) %>% 
  arrange(desc(value))

chart_thu_ck <- highchart() %>% 
  hc_yAxis(
    title = list(text = "tỷ đồng"),
    gridLineColor = "#e6e6e6"
  ) %>% 
  hc_xAxis(
    categories = df$name,
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6"
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = name, y = value, color = mau_cot),
    type = "column",
    name = "Thu thuần từ kinh doanh chứng khoán",
    color = "#006b68",
    yAxis = 0,
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y:,.2f}", # Hiển thị nhãn giá trị với 2 chữ số thập phân
      style = list(fontSize = "10px")
    ),
    tooltip = list(
      valueSuffix = " tỷ đồng"
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
    text = "Thu thuần từ kinh doanh chứng khoán của 10 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = str_glue("Lũy kế {month(max(df$yq)) + 2} tháng đầu năm {year(max(df$yq))}"),
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  ) %>% hc_export_menu()
