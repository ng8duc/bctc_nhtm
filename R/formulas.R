# ===============================================================================
# formulas.R — Functions tính toán và phân tích
# ===============================================================================

# ===============================================================================
# Hướng dẫn
#
# Ở đây có rất nhiều hàm, mỗi hàm làm 1 chức năng khác nhau
# Hàm summarise_by_group dùng để tính các giá trị tổng, ví dụ như tổng 27 NHTM

# Hàm get_data_by_formula để trích xuất dữ liệu theo công thức

# Hàm create_mean_df dùng để tạo ra giá trị trung bình, tính bằng (quý này + cuối quý trước)/2,
# sử dụng với dữ liệu bắt đầu bằng BS_ thì phù hợp hơn (sẽ update)
# hàm này chỉ có tính trung gian, thực hiện trước khi tính ROA, ROE, NIM (có thể bổ sung thêm)

# ===============================================================================

# ===============================================================================
# Công thức đơn giản
# ===============================================================================

# Tính các chỉ tiêu tổng hợp theo nhóm ngân hàng
summarise_by_group <- function(df, avg = T) {
  # 3 NHTM Nhà nước
  nhtm_nn <- df %>%
    filter(cong_ty %in% c("BID", "VCB", "CTG")) %>%
    group_by(yq) %>%
    summarise(
      across(.cols = -c(cong_ty), .fns = mean) # lấy bình quân
    ) %>%
    mutate(cong_ty = "BQ 3 NHTMNN") %>%
    ungroup()

  # 7 NHTM lớn
  nhtm_large <- df %>%
    filter(cong_ty %in% c("VPB", "SHB", "VIB", "MBB", "ACB", "HDB", "TCB")) %>%
    group_by(yq) %>%
    summarise(
      across(.cols = -c(cong_ty), .fns = mean) # lấy bình quân
    ) %>%
    mutate(cong_ty = "BQ 7 NHTMCP lớn") %>%
    ungroup()

  # 27 NHTM
  nhtm_mean <- df %>%
    group_by(yq) %>%
    summarise(
      across(.cols = -c(cong_ty), .fns = mean) # lấy bình quân
    ) %>%
    mutate(cong_ty = "BQ 27 NHTM") %>%
    ungroup()

  nhtm_sum <- df %>%
    group_by(yq) %>%
    summarise(
      across(.cols = -c(cong_ty), .fns = sum) # lấy tổng số
    ) %>%
    mutate(cong_ty = "Tổng 27 NHTM") %>%
    ungroup()

  df_result_T <- bind_rows(df, nhtm_nn, nhtm_large, nhtm_sum, nhtm_mean)
  df_result_F <- bind_rows(df, nhtm_sum)

  if (avg == T) {
    return(df_result_T)
  } else {
    return(df_result_F)
  }
}

# Nạp công thức dưới dạng string và trích xuất dữ liệu
get_data_by_formula <- function(df, formula_str, col_name = "new_col") {
  expr <- parse_expr(formula_str)

  df <- df %>%
    mutate("{col_name}" := !!expr)

  df_result <- df %>%
    select(cong_ty, yq, {{ col_name }}) %>%
    pivot_wider(names_from = cong_ty, values_from = {{ col_name }})

  # Sort dòng cuối cùng
  col_order <- df_result %>%
    filter(yq == max(yq)) %>%
    pivot_longer(-yq, names_to = "name", values_to = "value") %>%
    filter(name %in% list_cty) %>%
    arrange(desc(value))

  col_order1 <- col_order %>%
    filter(name %in% top10banks) %>%
    pull(name)

  col_order2 <- col_order %>%
    filter(!(name %in% top10banks)) %>%
    pull(name)

  df_result <- df_result %>%
    select(yq, all_of("Tổng 27 NHTM"), all_of(col_order1), everything()) %>%
    select(-all_of(col_order2)) %>%
    return(df_result)
}

# ===============================================================================
# Công thức phức tạp - ROA, ROE, NIM
# ===============================================================================

# Tạo giá trị trung bình
create_mean_df <- function(df) {
  df <- df %>%
    complete(cong_ty,
      yq = seq.Date(min(yq), max(yq), by = "3 months")
    )

  df_q4_prev <- df %>%
    filter(month(yq) == 10) %>%
    mutate(target_year = year(yq) + 1) %>%
    select(-yq) %>%
    rename_with(
      .fn = ~ str_c("q4_prev_", .x),
      .cols = -c(cong_ty, target_year)
    )

  df_result <- df %>%
    mutate(current_year = year(yq)) %>%
    left_join(
      df_q4_prev,
      by = c("cong_ty" = "cong_ty", "current_year" = "target_year")
    ) %>%
    mutate(
      across(
        .cols = -c(cong_ty, yq, current_year, starts_with("q4_prev_")),
        .fns = ~ (.x + get(paste0("q4_prev_", cur_column()))) / 2,
        .names = "avg_{.col}"
      )
    ) %>%
    select(-c(current_year, starts_with("q4_prev")))

  return(df_result)
}
