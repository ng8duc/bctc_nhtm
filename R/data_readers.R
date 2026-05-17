# ===============================================================================
# data_readers.R — Functions đọc và transform dữ liệu từ Excel
# ===============================================================================

# Tổng hợp dữ liệu từ 1 file Excel
# Dùng wb_load() mở workbook 1 lần duy nhất nạp dữ liệu vào RAM
single_file_data <- function(folder, file_name, company_name) {
  file_path <- here(folder, file_name)

  # Mở workbook 1 LẦN
  wb <- wb_load(file_path)

  imap(SHEETS, ~ {
    sheet_name <- str_glue("{.x} - {company_name}")

    wb_to_df(wb, sheet = sheet_name, start_row = 9) %>%
      clean_names() %>%
      filter(!is.na(don_vi)) %>%
      rowid_to_column() %>%
      mutate(rowid = str_c(.y, "_", rowid))
  }) %>%
    list_rbind() %>%
    select(-c(don_vi)) %>%
    mutate(cong_ty = company_name)
}

# Merge nhiều file có chung pattern - tên công ty
# Dùng future_map() song song hóa + pivot thay vì reduce(full_join)
gen_full_data <- function(folder, company_name) {
  files <- list.files(path = here(folder), pattern = company_name)

  files %>%
    future_map(~ single_file_data(folder, .x, company_name),
      .options = furrr_options(seed = TRUE)
    ) %>%
    map(~ pivot_longer(.x, -c(rowid, chi_tieu, cong_ty),
      names_to = "period", values_to = "value"
    )) %>%
    bind_rows() %>%
    distinct(rowid, chi_tieu, cong_ty, period, .keep_all = TRUE) %>%
    pivot_wider(names_from = period, values_from = value)
}

# Trích cộng dồn theo năm
gen_yearly_agg <- function(df, year, quarter) {
  df <- df %>%
    select(rowid, chi_tieu, cong_ty, contains(str_glue("{year}")))

  quarter_list <- df %>%
    select(-c(rowid, chi_tieu, cong_ty)) %>%
    rename_with(.fn = ~ str_sub(.x, 2, 2)) %>%
    names() %>%
    as.numeric()

  if (1 %in% quarter_list && quarter %in% quarter_list) {
    quarter_list2 <- quarter_list[quarter_list <= quarter]

    quarter_list2 <- str_c("q", quarter_list2, str_glue("_{year}"))

    df <- df %>%
      select(rowid, chi_tieu, cong_ty, all_of(quarter_list2))

    # Đoạn này quan trọng
    notes_max <- str_c("NOTES_", 1:119)
    # notes_sum <- str_c('NOTES_', 120:195)

    df <- df %>%
      mutate(
        value = case_when(
          grepl("FS", rowid) ~ get(max(quarter_list2)),
          rowid %in% notes_max ~ get(max(quarter_list2)),
          rowid %in% c("CF_47") ~ get(min(quarter_list2)),
          rowid %in% c("CF_48") ~ get(max(quarter_list2)),
          .default = rowSums(pick(starts_with("q")), na.rm = F)
        )
      )

    df <- df %>%
      select(rowid, chi_tieu, cong_ty, value) %>%
      mutate(yq = str_glue("{quarter}q{year}"))

    return(df)
  } else {
    return(data.frame())
  }
}
