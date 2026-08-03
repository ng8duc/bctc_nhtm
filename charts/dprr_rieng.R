df <- df_ty_le_chi_phi_dprr_thu_nhap %>% 
  mutate(yq = as.Date(yq),
         value = abs(value)*100)

df <- df %>% 
  filter(nchar(name) == 3) %>% 
  filter(
    year(yq) >= year(max(yq)) - 5,
    month(yq) == month(max(yq)),
  )

df <- df %>% 
  mutate(
    yq = str_glue("{month(yq)+2}T/{year(yq)}")
  )

chart_dprr_rieng <- highchart() %>% 
  hc_xAxis(gridLineWidth = 1,
           gridLineColor = "#e6e6e6",
           categories = df$name) %>% 
  hc_yAxis(title = list(text = "%"),
           gridLineColor = "#e6e6e6") %>% 
  hc_add_series(
    data = df,
    mapping = hcaes(x = name, y = value, group = yq),
    type = "column",
    tooltip = list(
      valueSuffix = "%"
    )
  )  %>% 
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE,
    valueDecimals = 2) %>% 
  hc_legend(
    align = "center",
    verticalAlign = "top",
    layout = "horizontal",
    symbolRadius = 0
  ) %>% 
  hc_title(
    text = "Chi phí DPRR/tổng thu nhập của 10 NHTM niêm yết",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>% hc_export_menu()
