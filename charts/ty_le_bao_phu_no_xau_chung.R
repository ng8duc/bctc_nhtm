df <- df_ty_le_bao_phu_no_xau %>%
  filter(name == "BQ 27 NHTM",
         !is.na(value)) %>% 
  mutate(yq = as.Date(yq))

chart_ty_le_bao_phu_no_xau_chung <- highchart() %>%
  hc_colors(colors = c("#006b68", "#fdb71a")) %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = value * 100),
    type = "column",
    name = "Tỷ lệ bao phủ nợ xấu (%)",
    tooltip = list(
      valueSuffix = "%"
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
        var lastDayOfQuarter = new Date(Date.UTC(year, lastMonthOfQuarter, 0));
        var day = lastDayOfQuarter.getUTCDate();
        var displayMonth = lastDayOfQuarter.getUTCMonth() + 1;
        
        return day + '/' + displayMonth + '/' + year;
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
      var lastDayOfQuarter = new Date(Date.UTC(year, lastMonthOfQuarter, 0));
      var day = lastDayOfQuarter.getUTCDate();
      var displayMonth = lastDayOfQuarter.getUTCMonth() + 1;
      var xLabel = day + '/' + displayMonth + '/' + year;
      
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
    text = "Tỷ lệ bao phủ nợ xấu bình quân của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  )
