df_ty_le_no_xau <- df_ty_le_no_xau %>%
  mutate(chi_tieu = "Tỷ lệ nợ xấu")

df_ty_le_no_nhom_2 <- df_ty_le_no_nhom_2 %>%
  mutate(chi_tieu = "Tỷ lệ nợ nhóm 2")

df <- bind_rows(df_ty_le_no_xau, df_ty_le_no_nhom_2) %>%
  mutate(yq = as.Date(yq)) %>%
  filter(name == "BQ 27 NHTM") %>%
  filter(!is.na(value))

chart_ty_le_no_xau <- highchart() %>%
  hc_colors(colors = c("#006b68", "#fdb71a")) %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = value * 100, group = chi_tieu),
    type = "line",
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
    gridLineColor = "#e6e6e6",
    labels = list(format = "{value:,.1f}%")
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
    text = "Tỷ lệ nợ xấu và nợ nhóm 2 bình quân của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  )
