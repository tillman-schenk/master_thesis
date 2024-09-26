# read data

national_accounts_data <- str_c(path$data_folder, "national accounts.csv") %>%
  read.csv()

growth_accounts_data <- str_c(path$data_folder, "growth accounts.csv") %>%
  read.csv()