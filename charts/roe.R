df <- df_roe %>%
  filter(name == "BQ 27 NHTM",
         !is.na(value)) %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(month(yq) == month(max(yq)))

chart_roe <- highchart() %>%
  hc_colors(colors = c("#006b68", "#fdb71a")) %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = value * 100),
    type = "column",
    name = "Tỷ lệ ROE bình quân (%)",
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
  hc_yAxis(
    title = list(text = "%"),
    gridLineColor = "#e6e6e6"
  ) %>%
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE,
    valueDecimals = 2,
    formatter = JS("function(tooltip) {
      var date = new Date(this.x);
      var year = date.getUTCFullYear();
      var lastMonthOfQuarter = date.getUTCMonth() + 3;
      var xLabel = lastMonthOfQuarter + 'T/' + year;
      
      var s = '<b>' + xLabel + '</b><br/>';
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
    text = "Tỷ lệ ROE quân của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  ) %>% 
  hc_subtitle(
    text = str_glue("Annualized"),
    style = list(fontStyle = "italic", color = "#666666")
  )
