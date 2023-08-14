# Read in data
m_path <- Sys.getenv("MKUSHEL_SHARED")

full_dat <- readxl::read_excel(paste(m_path, "/Home Safe Eval 23_25/june_2023_exploratory_data/7.12.23.Home Safe Data-FY 22-23 Q3.xlsx", sep = ""),
                               sheet = "Full Dataset")

full_dat <- full_dat %>%
  janitor::clean_names()
