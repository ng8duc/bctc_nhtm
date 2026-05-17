# File: push_to_d1.R
# Tác giả: Antigravity
# Mục đích: Đẩy R Dataframe lên Cloudflare D1 thông qua Wrangler CLI (Hỗ trợ Ghi đè)

library(glue)

#' Đẩy dữ liệu từ R Dataframe lên Cloudflare D1
#' @param df Dataframe cần đẩy
#' @param db_name Tên database (ví dụ: "my-db")
#' @param table_name Tên table mục tiêu
#' @param chunk_size Số dòng đẩy mỗi lần (mặc định 500 để an toàn)
#' @param overwrite Nếu TRUE (mặc định), sẽ xóa bảng cũ và tạo lại bảng mới
push_to_d1 <- function(df, db_name, table_name, chunk_size = 500, overwrite = TRUE) {
  if (!inherits(df, "data.frame")) stop("Dữ liệu đầu vào phải là một Dataframe.")

  total_rows <- nrow(df)
  if (total_rows == 0) {
    message("Dataframe trống. Không có gì để đẩy.")
    return(NULL)
  }

  # --- Bước 1: Xử lý Ghi đè (Drop & Create Table) ---
  if (overwrite) {
    message(glue("Đang xóa và tạo lại table '{table_name}'..."))

    # Ánh xạ kiểu dữ liệu R -> SQLite
    col_types <- sapply(df, function(x) {
      if (is.integer(x)) {
        return("INTEGER")
      }
      if (is.numeric(x)) {
        return("REAL")
      }
      if (is.logical(x)) {
        return("INTEGER")
      } # SQLite dùng 0/1 cho boolean
      return("TEXT")
    })

    col_names <- names(df)
    col_defs <- paste(glue("\"{col_names}\" {col_types}"), collapse = ", ")

    init_sql <- glue("DROP TABLE IF EXISTS \"{table_name}\"; CREATE TABLE \"{table_name}\" ({col_defs});")

    temp_init_sql <- tempfile(fileext = ".sql")
    writeLines(init_sql, temp_init_sql, useBytes = TRUE)

    res_init <- system2("wrangler",
      args = c("d1", "execute", db_name, "--file", temp_init_sql, "--remote"),
      stdout = TRUE,
      stderr = TRUE
    )
    unlink(temp_init_sql)

    if (any(grepl("Error", res_init, ignore.case = TRUE))) {
      stop(glue("Lỗi khi khởi tạo bảng: {paste(res_init, collapse = '\n')}"))
    }
  }

  # --- Bước 2: Đẩy dữ liệu theo từng Chunk ---
  num_chunks <- ceiling(total_rows / chunk_size)
  message(glue("Bắt đầu đẩy {total_rows} dòng vào bảng '{table_name}' (chia làm {num_chunks} đợt)..."))

  for (i in 1:num_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, total_rows)
    df_chunk <- df[start_idx:end_idx, , drop = FALSE]

    # Chuyển chunk thành chuỗi SQL VALUES
    rows <- apply(df_chunk, 1, function(x) {
      vals <- sapply(x, function(v) {
        if (is.na(v) || is.null(v)) {
          return("NULL")
        }
        if (is.numeric(v)) {
          return(as.character(v))
        }
        if (is.logical(v)) {
          return(ifelse(v, "1", "0"))
        }
        # Escape dấu nháy đơn và bọc trong nháy đơn
        clean_val <- gsub("'", "''", as.character(v))
        return(paste0("'", clean_val, "'"))
      })
      paste0("(", paste(vals, collapse = ", "), ")")
    })

    # Sử dụng tên cột để đảm bảo tính nhất quán
    col_names_sql <- paste0("\"", names(df), "\"", collapse = ", ")
    sql_query <- glue("INSERT INTO \"{table_name}\" ({col_names_sql}) VALUES {paste(rows, collapse = ', ')};")

    # Ghi ra file tạm để thực thi
    temp_sql <- tempfile(fileext = ".sql")
    writeLines(sql_query, temp_sql, useBytes = TRUE)

    # Thực thi qua wrangler CLI
    # Mặc định dùng --remote để đẩy lên Cloudflare D1 thật
    # Nếu bạn muốn test local, hãy xóa '--remote' trong args
    res <- system2("wrangler",
      args = c("d1", "execute", db_name, "--file", temp_sql, "--remote"),
      stdout = TRUE,
      stderr = TRUE
    )

    unlink(temp_sql)

    if (any(grepl("Error", res, ignore.case = TRUE))) {
      warning(glue("Có lỗi xảy ra ở đợt {i}: {paste(res, collapse = '\n')}"))
    } else {
      message(glue("  [+] Đã xong đợt {i}/{num_chunks} (Dòng {start_idx}-{end_idx})"))
    }
  }

  message(glue("Đã đẩy xong dữ liệu vào bảng '{table_name}'!"))
}

# --- Hướng dẫn nhanh ---
# source("push_to_d1.R")
# push_to_d1(df, "db-name", "table-name", overwrite = TRUE)
