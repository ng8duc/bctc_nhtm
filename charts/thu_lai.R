df <- df_thu_lai_rong %>% 
  mutate(yq = as.Date(yq))

df <- df %>% 
  filter(name == "Tổng 27 NHTM",
         month(yq) == month(max(yq)))

df <- df %>% 
  mutate(growth_rate_yoy = value/lag(value)*100-100)

chart_thu_lai <- highchart() %>%
  hc_yAxis_multiples(
    list(
      title = list(text = "Thu lãi thuần (nghìn tỷ đồng)"),
      gridLineColor = "#e6e6e6"
    ),
    list(
      title = list(text = "Tăng trưởng YOY (%)"),
      opposite = TRUE,
      gridLineWidth = 0, # Ẩn vạch kẻ ngang của trục thứ hai để tránh rối mắt
      labels = list(format = "{value:,.1f}%")
    )
  ) %>%
  hc_xAxis(
    type = "category",
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6"
  ) %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = value / 1000),
    type = "column",
    name = "Thu lãi thuần",
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
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = growth_rate_yoy),
    type = "line",
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
    valueDecimals = 2,
    formatter = JS("function() {
      var s = '<b>' + this.points[0].key + '</b><br/>';
      this.points.forEach(function(point) {
        // Áp dụng đơn vị tương ứng với từng series
        var suffix = point.series.name === 'Thu lãi thuần' ? ' nghìn tỷ đồng' : '%';
        s += '<span style=\"color:' + point.color + '\">●</span> ' + 
             point.series.name + ': <b>' + Highcharts.numberFormat(point.y, 2) + suffix + '</b><br/>';
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
  hc_chart(zoomType = "x") %>% 
  hc_title(
    text = "Tổng thu lãi thuần của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = "Trừ dự phòng rủi ro",
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  )
