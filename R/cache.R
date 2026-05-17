# ===============================================================================
# cache.R — Cache helper functions (sử dụng qs)
# ===============================================================================

cache_dir <- here(".cache")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Trả về TRUE nếu cache hợp lệ:
#   1. Cache tồn tại và mới hơn toàn bộ source files (mtime)
#   2. Số lượng source files không thay đổi (phòng trường hợp copy file cũ vào)
is_cache_valid <- function(cache_file, source_dir) {
  if (!file.exists(cache_file)) return(FALSE)

  cache_mtime  <- file.mtime(cache_file)
  source_files <- list.files(here(source_dir), full.names = TRUE, recursive = TRUE)
  if (length(source_files) == 0) return(FALSE)

  # Kiểm tra 1: tất cả file nguồn đều cũ hơn cache
  mtime_ok <- all(file.mtime(source_files) < cache_mtime)

  # Kiểm tra 2: số lượng file không đổi (copy file mới vào sẽ trigger rebuild)
  count_file <- paste0(cache_file, ".count")
  if (!file.exists(count_file)) {
    writeLines(as.character(length(source_files)), count_file)
    return(mtime_ok)
  }
  saved_count <- as.integer(readLines(count_file, warn = FALSE))
  count_ok    <- (saved_count == length(source_files))

  mtime_ok && count_ok
}

# Ghi số lượng file vào .count sau mỗi lần save cache
save_cache <- function(data, cache_file, source_dir) {
  qs2::qs_save(data, cache_file)
  source_files <- list.files(here(source_dir), full.names = TRUE, recursive = TRUE)
  writeLines(as.character(length(source_files)), paste0(cache_file, ".count"))
}
