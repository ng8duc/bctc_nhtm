df <- df_ty_le_chi_phi_dprr_thu_nhap %>% 
  mutate(yq = as.Date(yq),
         value = abs(value))

df <- df %>% 
  filter(name == "BQ 27 NHTM",
         month(yq) == month(max(yq)))

chart_dprr <- highchart() %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = value * 100),
    type = "column",
    color = "#006b68",
    name = "Tỷ lệ chi phí DPRR/tổng thu nhập bình quân",
    tooltip = list(
      valueSuffix = "%"
    ),
    dataLabels = list(
      enabled = TRUE,
      format = "{point.y:,.2f}", # Hiển thị nhãn giá trị với 2 chữ số thập phân
      style = list(fontSize = "10px")
    )
  ) %>%
  hc_xAxis(
    type = "category",
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6"
  ) %>%
  hc_yAxis(
    title = list(text = "%"),
    gridLineColor = "#e6e6e6"
  ) %>%
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE,
    valueDecimals = 2,
    formatter = JS("function(tooltip) {
      var s = '<b>' + this.points[0].key + '</b><br/>';
      this.points.forEach(function(point) {
        s += '<span style=\"color:' + point.color + '\">●</span> ' + 
             point.series.name + ': <b>' + Highcharts.numberFormat(point.y, 2) + '%</b><br/>';
      });
      return s;
    }")
  ) %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_legend(
    align = "center",
    verticalAlign = "top",
    layout = "horizontal",
    symbolRadius = 0
  ) %>% 
  hc_title(
    text = "Tỷ lệ chi phí DPRR/tổng thu nhập bình quân của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  )
