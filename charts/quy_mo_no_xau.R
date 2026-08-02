df1 <- df_quy_mo_no_xau %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(nchar(name) == 3) %>% 
  filter(yq == max(yq)) %>%
  arrange(desc(value)) %>%
  mutate(
    mau_cot = ifelse(name != "BID", "#006b68", "#fdb71a"),
  ) %>% 
  rename(quy_mo_no_xau := value)

df2 <- df_ty_le_no_xau %>% 
  mutate(yq = as.Date(yq),
         value = value*100) %>% 
  filter(nchar(name) == 3) %>% 
  filter(yq == max(yq)) %>% 
  rename(ty_le_no_xau := value)

df <- full_join(df1, df2)

chart_quy_mo_no_xau <- highchart() %>%
  hc_yAxis_multiples(
    list(
      title = list(text = "Quy mô nợ xấu (nghìn tỷ đồng)"),
      gridLineColor = "#e6e6e6"
    ),
    list(
      title = list(text = "Tỷ lệ nợ xấu (%)"),
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
    mapping = hcaes(x = name, y = quy_mo_no_xau / 1000, color = mau_cot),
    type = "column",
    name = "Quy mô nợ xấu",
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
    mapping = hcaes(x = name, y = ty_le_no_xau),
    type = "line",
    lineWidth = 0,
    marker = list(enabled = TRUE, radius = 5),
    name = "Tỷ lệ nợ xấu",
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
  hc_add_theme(hc_theme_smpl()) %>%
  hc_legend(
    align = "center",
    verticalAlign = "top",
    layout = "horizontal",
    symbolRadius = 0
  ) %>%
  hc_chart(zoomType = "x") %>% 
  hc_title(
    text = "Quy mô nợ xấu của 10 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = str_glue("Ngày số liệu: {strftime(max(df$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  )
