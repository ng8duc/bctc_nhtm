df <- df_car %>% 
  mutate(date = as.Date(date))

df <- df %>% 
  filter(date == max(date)) %>% 
  pivot_longer(-c(date), names_to = "name", values_to = "value")

df <- df %>% 
  mutate(mau_cot = ifelse(name == "BID", "#fdb71a", "#006b68"))

df <- df %>% 
  arrange(desc(value))

chart_car <- highchart() %>% 
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
    mapping = hcaes(x = name, y = value, color = mau_cot),
    type = "column",
    name = "Tỷ lệ an toàn vốn (CAR)",
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
    text = "Tỷ lệ an toàn vốn (CAR) của 10 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  ) %>% 
  hc_subtitle(
    text = str_glue("Ngày số liệu: {strftime(max(df$date), format = '%d/%m/%Y')}"),
    style = list(fontStyle = "italic", color = "#666666")
  )
