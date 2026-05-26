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
    mapping = hcaes(x = yq, y = `Thu dịch vụ thuần` / 1000),
    type = "column",
    name = "Thu dịch vụ thuần",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = `Thu thuần từ chứng khoán kinh doanh` / 1000),
    type = "column",
    name = "Thu thuần từ chứng khoán kinh doanh",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = `Thu thuần từ chứng khoán đầu tư` / 1000),
    type = "column",
    name = "Thu thuần từ chứng khoán đầu tư",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = `Thu thuần từ kinh doanh ngoại hối` / 1000),
    type = "column",
    name = "Thu thuần từ kinh doanh ngoại hối",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = `Thu thuần từ góp vốn, mua cổ phần` / 1000),
    type = "column",
    name = "Thu thuần từ góp vốn, mua cổ phần",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = `Thu thuần từ hoạt động khác` / 1000),
    type = "column",
    name = "Thu thuần từ hoạt động khác",
  ) %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = yq, y = growth_rate_yoy),
    type = "line",
    marker = list(enabled = TRUE, radius = 5),
    name = "Tăng trưởng YOY",
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
      var date = new Date(this.x);
      var year = date.getUTCFullYear();
      var lastMonthOfQuarter = date.getUTCMonth() + 3;
      var xLabel = lastMonthOfQuarter + 'T/' + year;
      
      var s = '<b>' + xLabel + '</b><br/>';
      this.points.forEach(function(point) {
        // Áp dụng đơn vị tương ứng với từng series
        var suffix = point.series.name === 'Tăng trưởng YOY' ? '%' : ' nghìn tỷ đồng';
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
  hc_plotOptions(
    column = list(stacking = "normal")
  ) %>% 
  hc_title(
    text = "Cơ cấu và tăng trưởng thu ngoài lãi của 27 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333")
  )
