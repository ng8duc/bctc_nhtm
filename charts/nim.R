df_nim <- df_nim %>%
  mutate(chi_tieu = "NIM")

df_chenh_lech_lai_suat_dau_ra_dau_vao <- df_chenh_lech_lai_suat_dau_ra_dau_vao %>%
  mutate(chi_tieu = "CLLS đầu ra - đầu vào")

df <- bind_rows(df_nim, df_chenh_lech_lai_suat_dau_ra_dau_vao) %>%
  mutate(yq = as.Date(yq)) %>%
  filter(name == "BQ 27 NHTM") %>%
  filter(!is.na(value))

df <- df %>%
  mutate(tick_labels = str_c(3 * quarter(yq), "T_", year(yq)))

chart_nim <- highchart() %>%
  hc_colors(colors = c("#006b68", "#fdb71a")) %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = tick_labels, y = value * 100, group = chi_tieu),
    type = "line",
    tooltip = list(
      valueSuffix = "%"
    ),
    marker = list(enabled = TRUE, radius = 4)
  ) %>%
  hc_xAxis(
    gridLineWidth = 1,
    gridLineColor = "#e6e6e6",
    categories = df$tick_labels
  ) %>%
  hc_yAxis(
    title = list(text = "%"),
    gridLineColor = "#e6e6e6"
  ) %>%
  hc_tooltip(
    shared = TRUE,
    crosshairs = TRUE,
    valueDecimals = 2
  ) %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_legend(
    align = "center",
    verticalAlign = "top",
    layout = "horizontal",
    symbolRadius = 0
  )