df_nim <- df_nim %>%
  mutate(chi_tieu = "NIM")

df_chenh_lech_lai_suat_dau_ra_dau_vao <- df_chenh_lech_lai_suat_dau_ra_dau_vao %>%
  mutate(chi_tieu = "CLLS đầu ra - đầu vào")

df <- bind_rows(df_nim, df_chenh_lech_lai_suat_dau_ra_dau_vao) %>%
  mutate(yq = as.Date(yq)) %>%
  filter(name == "BQ 27 NHTM") %>%
  filter(!is.na(value))

chart_nim <- highchart() %>%
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
        var month = date.getUTCMonth() + 1;
        var quarter = Math.ceil(month / 3);
        return (quarter * 3) + 'T/' + date.getUTCFullYear();
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
      var month = date.getUTCMonth() + 1;
      var quarter = Math.ceil(month / 3);
      var xLabel = (quarter * 3) + 'T/' + date.getUTCFullYear();
      
      var s = '<b>' + xLabel + '</b><br/>';
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
  hc_chart(zoomType = "x") %>% 
  hc_title(
    text = "Chênh lệch lãi suất và NIM bình quân của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% 
  hc_subtitle(
    text = "Annualized",
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  ) %>% hc_export_menu()