df <- df_roa %>%
  filter(name == "BQ 27 NHTM",
         !is.na(value)) %>% 
  mutate(yq = as.Date(yq)) %>% 
  filter(month(yq) == month(max(yq)))

chart_roa <- highchart() %>%
  hc_colors(colors = c("#006b68", "#fdb71a")) %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = value * 100),
    type = "column",
    name = "Tỷ lệ ROA bình quân",
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
  hc_legend(
    align = "center",
    verticalAlign = "top",
    layout = "horizontal",
    symbolRadius = 0
  ) %>% 
  hc_title(
    text = "Tỷ lệ ROA bình quân của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = str_glue("Annualized"),
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  ) %>% hc_export_menu()
