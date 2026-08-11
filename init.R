# ==============================================================================
# File: init.R
# Mục đích: Tải dữ liệu giống targets pipeline (ưu tiên đọc từ cache nếu có)
# và xả toàn bộ bảng ra Global Environment để phát triển/thử nghiệm chart mới.
# Cách dùng: source("init.R")
# ==============================================================================

library(targets)
source("R/config.R")

if (tar_exist_objects("all_tables")) {
  message("Đã tìm thấy cache targets cho 'all_tables', đang tải từ cache...")
  all_tables <- tar_read(all_tables)
} else {
  message("Chưa có cache, đang tải dữ liệu mới từ D1...")
  source("R/get_all_tables.R")
  db_name <- Sys.getenv("D1_DATABASE")
  all_tables <- get_all_tables_list(db_name)
}

invisible(list2env(all_tables, envir = .GlobalEnv))

message(glue::glue("\nĐã xả {length(all_tables)} bảng ra môi trường: {paste(names(all_tables), collapse = ', ')}"))
