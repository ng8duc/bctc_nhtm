# ==============================================================================
# File: R/get_all_tables.R
# Tác giả: Antigravity
# Mục đích: Tải toàn bộ các bảng từ Cloudflare D1 và lưu thành các dataframe tương ứng
# ==============================================================================

# 1. Nạp thư viện và cấu hình dự án
library(tidyverse)
library(here)

# Tìm và nạp file config.R để có cấu hình D1_DATABASE và hàm pull_from_d1
# Sử dụng here() giúp định vị chính xác đường dẫn dù script được chạy từ đâu
config_path <- here("R", "config.R")
if (file.exists(config_path)) {
  source(config_path)
} else if (file.exists("R/config.R")) {
  source("R/config.R")
} else if (file.exists("config.R")) {
  # Fallback nếu thiết lập working directory nằm tại thư mục R/
  source("config.R")
} else {
  stop("Không tìm thấy file config.R. Vui lòng kiểm tra lại cấu hình thư mục làm việc.")
}

# 2. Truy vấn danh sách các bảng trong database (loại trừ các bảng hệ thống)
query_get_tables <- "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' AND name NOT LIKE '_cf_%' AND name NOT LIKE 'd1_%'"

message("--- Bắt đầu lấy danh sách bảng từ Cloudflare D1 ---")
tables_df <- pull_from_d1(db_name = D1_DATABASE, query = query_get_tables)

if (is.null(tables_df) || nrow(tables_df) == 0) {
  stop("Không tìm thấy bảng dữ liệu nào trong database D1.")
}

# Lấy danh sách tên bảng dạng vector
table_names <- tables_df %>% 
  pull(name)

message(glue("Tìm thấy {length(table_names)} bảng: {paste(table_names, collapse = ', ')}"))

# 3. Tải từng bảng về và gán vào môi trường toàn cục (Global Environment)
walk(table_names, function(tbl) {
  # Tạo tên biến tương ứng dạng df_{tên_bảng}
  var_name <- glue("df_{tbl}")
  
  message(glue("\n[+] Đang tải bảng: '{tbl}' -> Lưu vào biến: '{var_name}'"))
  
  # Tải dữ liệu từ database
  df_content <- pull_from_d1(db_name = D1_DATABASE, table_name = tbl)
  
  # Gán dataframe vào Global Environment (.GlobalEnv) để sử dụng ở môi trường ngoài
  assign(var_name, df_content, envir = .GlobalEnv)
})

message("\n--- Hoàn tất! Đã nạp tất cả các bảng vào Global Environment ---")
