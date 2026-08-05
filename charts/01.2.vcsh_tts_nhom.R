df <- df_ty_le_von_csh_tts %>% 
  mutate(yq = as.Date(yq))

df <- df %>% 
  filter(grepl("BQ ", name) | name == "BID",
         yq == max(yq)) %>% 
  mutate(mau_cot = ifelse(name != "BID", "#006b68", "#fdb71a"))

highchart() %>% 
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
    name = "Vốn chủ sở hữu/tổng tài sản",
    color = "#006b68",
    yAxis = 0,
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y:,.1f}",
      style = list(fontSize = "10px")
    ),
    tooltip = list(
      valueSuffix = "%"
    )
  ) %>% 
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE,
    valueDecimals = 1
  ) %>%
  hc_legend(
    align = "center",
    verticalAlign = "top",
    layout = "horizontal",
    symbolRadius = 0
  ) %>%
  hc_chart(zoomType = "x") %>% 
  hc_title(
    text = "Vốn chủ sở hữu/tổng tài sản theo nhóm ngân hàng",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = str_glue("Ngày số liệu: {strftime(max(df$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  ) %>% hc_export_menu()
