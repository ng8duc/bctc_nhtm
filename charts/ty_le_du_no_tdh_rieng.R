df <- df_ty_trong_du_no_tdh %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(
    nchar(name) == 3,
    yq == max(yq)
  ) %>% 
  mutate(mau_cot = ifelse(name != "BID", "#006b68", "#fdb71a"))

chart_ty_le_du_no_tdh_rieng <- highchart() %>% 
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
    name = "Tỷ lệ dư nợ trung, dài hạn/tín dụng",
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
    text = "Tỷ lệ dư nợ trung, dài hạn/tín dụng của 10 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = str_glue("Ngày số liệu: {strftime(max(df$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  )
