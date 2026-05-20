df <- df_chenh_lech_thu_chi %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(name == "Tổng 27 NHTM",
         month(yq) == month(max(yq))) %>% 
  mutate(cltc_yoy = value/lag(value)*100-100) %>% 
  rename(cltc := value)

chart_cltc <- highchart() %>%
  hc_yAxis_multiples(
    list(
      title = list(text = "Chênh lệch thu chi (nghìn tỷ đồng)"),
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
    type = "datetime",
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6",
    labels = list(
      formatter = JS("function() {
        var date = new Date(this.value);
        var year = date.getUTCFullYear();
        var lastMonthOfQuarter = date.getUTCMonth() + 3;
        
        return lastMonthOfQuarter + 'T/' + year;
      }")
    )
  ) %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = cltc / 1000),
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
    mapping = hcaes(x = yq, y = cltc_yoy),
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
      var date = new Date(this.x);
      var year = date.getUTCFullYear();
      var lastMonthOfQuarter = date.getUTCMonth() + 3;
      var xLabel = lastMonthOfQuarter + 'T/' + year;
      
      var s = '<b>' + xLabel + '</b><br/>';
      this.points.forEach(function(point) {
        // Áp dụng đơn vị tương ứng với từng series
        var suffix = point.series.name === 'Chênh lệch thu chi' ? ' nghìn tỷ đồng' : '%';
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
    text = "Tổng chênh lệch thu chi của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  )
