# Read in data
m_path <- Sys.getenv("HOME_SAFE")

full_dat <- readxl::read_excel(paste(m_path, "/Quantitative/data/report_1_data/home_safe_data_upload_090623.xlsx", sep = ""),
                               sheet = "Sheet1")

full_dat <- full_dat %>%
  janitor::clean_names()

