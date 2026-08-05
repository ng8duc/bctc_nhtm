df <- df_ty_le_von_csh_tts %>% 
  mutate(yq = as.Date(yq))

df <- df %>% 
  filter(name == "BQ 27 NHTM")

chart_vcsh_tts <- highchart() %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = value * 100),
    type = "line",
    name = "Vốn chủ sở hữu/tổng tài sản",
    color = "#006b68",
    tooltip = list(
      valueSuffix = "%"
    ),
    marker = list(enabled = TRUE, radius = 4)
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
             point.series.name + ': <b>' + Highcharts.numberFormat(point.y, 1) + '%</b><br/>';
      });
      return s;
    }")
  ) %>%
  hc_legend(
    align = "center",
    verticalAlign = "top",
    layout = "horizontal",
    symbolRadius = 0
  ) %>% 
  hc_title(
    text = "Vốn chủ sở hữu/tổng tài sản bình quân của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% hc_export_menu()
