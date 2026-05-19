df_nim <- df_nim %>% 
  mutate(chi_tieu = "NIM")

df_chenh_lech_lai_suat_dau_ra_dau_vao <- df_chenh_lech_lai_suat_dau_ra_dau_vao %>% 
  mutate(chi_tieu = "CLLS đầu ra - đầu vào")

df <- bind_rows(df_nim, df_chenh_lech_lai_suat_dau_ra_dau_vao) %>% 
  filter(name == "BQ 27 NHTM") %>%
  filter(!is.na(value))

tick_vals <- df %>%
  distinct(yq) %>%
  arrange(yq) %>%
  filter(month(yq) == month(first(yq))) %>%
  pull(yq)

tick_labels <- str_c(3 * quarter(tick_vals), "T_", year(tick_vals))

chart_nim <- plot_ly(df, colors = c("#006b68", "#fdb71a")) %>%
  add_trace(
    x = ~yq,
    y = ~value,
    color = ~chi_tieu,
    type = "scatter",
    mode = "lines+markers",
    hoverlabel = list(align = "left"),
    hovertemplate = ~ str_c(
      "<b>Kỳ: ", 3 * quarter(yq), "T_", year(yq), "</b><br>",
      "Chỉ tiêu: ", chi_tieu, "<br>",
      "Giá trị: ", sprintf("%.1f%%", value * 100),
      "<extra></extra>"
    )
  ) %>%
  layout(
    xaxis = list(
      title = "",
      tickmode = "array",
      tickvals = tick_vals,
      ticktext = tick_labels
    ),
    yaxis = list(
      title = "",
      tickformat = ".1%"
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = 1.1,
      yanchor = "bottom"
    )
  )
