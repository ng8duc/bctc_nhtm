# ===============================================================================
# config.R — Packages, environment variables, constants
# ===============================================================================

pkgs <- c(
  "tidyverse", "here", "rio", "janitor", "zoo", "dotenv", "openxlsx2",
  "furrr", "qs2", "rlang", "glue"
)

lapply(pkgs, library, character.only = TRUE)

load_dot_env()

# Cấu hình D1 Database (Tên database do người dùng chọn)
D1_DATABASE <- Sys.getenv("D1_DATABASE")


# Danh sách mã công ty từ .env
list_cty <- str_split(Sys.getenv("TICKERS"), ",") %>% unlist()
top10banks <- str_split(Sys.getenv("TOP10BANKS"), ",") %>% unlist()

# Tên các sheet trong workbook
SHEETS <- c(
  FS    = "Cân đối kế toán",
  IS    = "Báo cáo thu nhập",
  CF    = "Lưu chuyển tiền tệ",
  NOTES = "Thuyết minh"
)
