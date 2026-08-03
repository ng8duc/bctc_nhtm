df <- list(
  "Thu dịch vụ thuần" = df_thu_dich_vu_rong,
  "Thu thuần từ chứng khoán kinh doanh" = df_thu_rong_tu_ck_kinh_doanh,
  "Thu thuần từ chứng khoán đầu tư" = df_thu_rong_tu_ck_dau_tu,
  "Thu thuần từ kinh doanh ngoại hối" = df_thu_rong_tu_kd_ngoai_hoi,
  "Thu thuần từ góp vốn, mua cổ phần" = df_thu_rong_tu_von_gop_co_phan,
  "Thu thuần từ hoạt động khác" = df_thu_rong_tu_hd_khac
) %>% 
  bind_rows(.id = "category") %>% 
  mutate(yq = as.Date(yq))

df <- df %>% 
  filter(name == "Tổng 27 NHTM") %>% 
  pivot_wider(names_from = "category", values_from = "value")

df <- df %>% 
  filter(month(yq) == month(max(yq)))

df <- df %>% 
  mutate(total = rowSums(pick(starts_with("Thu"))))

df <- df %>% 
  mutate(growth_rate_yoy = total/lag(total)*100-100)

chart_co_cau_thu_ngoai_lai <- highchart() %>% 
  hc_yAxis_multiples(
    list(
      title = list(text = "nghìn tỷ đồng"),
      gridLineColor = "#e6e6e6"
    ),
    list(
      title = list(text = NULL),
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
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = `Thu dịch vụ thuần` / 1000),
    type = "column",
    name = "Dịch vụ",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = `Thu thuần từ chứng khoán kinh doanh` / 1000),
    type = "column",
    name = "Chứng khoán kinh doanh",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = `Thu thuần từ chứng khoán đầu tư` / 1000),
    type = "column",
    name = "Chứng khoán đầu tư",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = `Thu thuần từ kinh doanh ngoại hối` / 1000),
    type = "column",
    name = "Kinh doanh ngoại hối",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = `Thu thuần từ góp vốn, mua cổ phần` / 1000),
    type = "column",
    name = "Góp vốn, mua cổ phần",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = `Thu thuần từ hoạt động khác` / 1000),
    type = "column",
    name = "Hoạt động khác",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = glue('{month(yq)+2}T/{year(yq)}'), y = growth_rate_yoy),
    type = "line",
    marker = list(enabled = TRUE, radius = 5),
    name = "Tăng trưởng thu ngoài lãi YOY",
    color = "#006b68",
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
        var suffix = point.series.name === 'Tăng trưởng thu ngoài lãi YOY' ? '%' : ' nghìn tỷ đồng';
        s += '<span style=\"color:' + point.color + '\">●</span> ' + 
             point.series.name + ': <b>' + Highcharts.numberFormat(point.y, 2) + suffix + '</b><br/>';
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
  hc_plotOptions(
    column = list(stacking = "normal")
  ) %>% 
  hc_title(
    text = "Cơ cấu và tăng trưởng thu ngoài lãi của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% hc_export_menu()
