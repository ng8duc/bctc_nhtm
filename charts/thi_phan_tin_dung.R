library(tidyverse)
library(highcharter)
library(timetk)

df_du_no <- df_du_no %>%
  mutate(yq = as.Date(yq)) %>%
  filter(nchar(name) == 3)

df_tin_dung_total <- df_tin_dung_total %>%
  mutate(ym = as.Date(ym)) %>%
  filter(month(ym) %in% c(3, 6, 9, 12)) %>%
  summarise_by_time(
    .date_var = ym,
    .by = "quarter",
    tin_dung = max(tin_dung)
  ) %>%
  rename(yq := ym)

df1 <- df_du_no %>%
  mutate(value = -value) %>%
  select(-c(name)) %>%
  bind_rows(rename(df_tin_dung_total, value := tin_dung)) %>%
  group_by(yq) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(name = "Khác") %>%
  filter(value > 0)

df <- bind_rows(df1, df_du_no %>% filter(yq <= max(df1$yq)))

df <- df %>%
  group_by(yq) %>%
  mutate(share = value / sum(value)) %>%
  ungroup() %>%
  filter(yq == max(yq))

df <- bind_rows(filter(df, name != "Khác") %>% arrange(desc(share)), filter(df, name == "Khác"))

chart_thi_phan_tin_dung <- highchart() %>%
  hc_colors(colors = c(
    "#fdb71a", "#006b68", "#e65c00", "#0088cc",
    "#d9383a", "#22b14c", "#8e44ad", "#bdc3c7",
    "#16a085", "#e84393", "#2c3e50", "#f39c12"
  )) %>%
  hc_add_series(
    data = df,
    mapping = hcaes(x = name, y = share * 100),
    type = "pie",
    name = "Thị phần tín dụng"
  ) %>%
  hc_tooltip(
    valueDecimals = 2,
    valueSuffix = "%"
  ) %>%
  hc_plotOptions(
    pie = list(
      allowPointSelect = TRUE,
      cursor = "pointer",
      dataLabels = list(
        enabled = TRUE,
        format = "<b>{point.name}</b>: {point.percentage:.1f}%"
      )
    )
  ) %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_title(
    text = "Thị phần tín dụng của các NHTM",
    style = list(fontWeight = "bold", fontSize = "16px", color = "#333333"),
    align = 'center'
  ) %>%
  hc_subtitle(
    text = str_glue("Ngày số liệu: {strftime(max(df$yq) + months(3) - days(1), format = '%d/%m/%Y')}"),
    style = list(fontStyle = "italic", color = "#666666"),
    align = 'center'
  )
