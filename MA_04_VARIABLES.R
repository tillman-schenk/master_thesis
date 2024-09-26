######## Prepare the data
merged_data <- merged_data %>%
  
  # sort and arrange
  group_by(country, industry) %>%
  arrange(country, industry, year)


######## Calculate Variables

# Employment growth
merged_data <- merged_data %>%
  mutate(employment_gr = (log(employment) - dplyr::lag(log(employment)))*100)

# Nominal wage bill and its growth
merged_data <- merged_data %>%
  mutate(wagebill_cp_gr = (log(wagebill_cp) - dplyr::lag(log(wagebill_cp)))*100)

# Nominal value-added and its growth
merged_data <- merged_data %>%
  mutate(va_cp_gr = (log(VA_CP) - dplyr::lag(log(VA_CP)))*100)

# Aggregate hours growth
merged_data <- merged_data %>%
  mutate(agg_hours_gr = (log(agg_hours) - dplyr::lag(log(agg_hours)))*100)

# Labor share and its growth
merged_data <- merged_data %>%
  mutate(labor_share = wagebill_cp/VA_CP) %>% # Calculate the labor share as the wagebill's share in value-added.
  mutate(labor_share_gr = (log(labor_share) - dplyr::lag(log(labor_share)))*100)

# Real wagebill and its growth
merged_data <- merged_data %>%
  mutate(wagebill = labor_share*VA_Q) %>% # Calculate real wagebill using value-added as chain-linked volumes and the labor share.
  mutate(wagebill_gr = (log(wagebill) - dplyr::lag(log(wagebill)))*100)

# Average real hourly wages
merged_data <- merged_data %>%
  mutate(wages = wagebill/agg_hours) %>% # Calculate real wagebill using value-added as chain-linked volumes and the labor share.
  mutate(wages_gr = (log(wages) - dplyr::lag(log(wages)))*100)


### calculate leave out mean TFP growth.
merged_data <- merged_data %>%
  group_by(year, industry) %>%
  mutate(
    total_TFP_gr = sum(TFP_gr, na.rm = TRUE),
    non_na_count_TFP_gr = sum(!is.na(TFP_gr))   # number of observations for which TFP_gr is not NA
  ) %>%
  ungroup() %>%
  mutate(
    leave_out_mean_TFP_gr = ifelse(non_na_count_TFP_gr > 1, # If the amount of observations for which TFP growth is not NA is 1, the leave out mean is not defined.
                                  (total_TFP_gr - TFP_gr) / (non_na_count_TFP_gr - 1), # formula for leave out mean.
                                  NA)
  )



######## Calculate Productivity Lags
merged_data <- merged_data %>%
  
  # sort and arrange
  group_by(country, industry) %>%
  arrange(country, industry, year) %>%

  mutate(  # Leave-out-mean TFP Lags
    leave_out_mean_TFP_gr_lag1 = dplyr::lag(leave_out_mean_TFP_gr, n = 1),
    leave_out_mean_TFP_gr_lag2 = dplyr::lag(leave_out_mean_TFP_gr, n = 2),
    leave_out_mean_TFP_gr_lag3 = dplyr::lag(leave_out_mean_TFP_gr, n = 3),
    leave_out_mean_TFP_gr_lag4 = dplyr::lag(leave_out_mean_TFP_gr, n = 4),
    leave_out_mean_TFP_gr_lag5 = dplyr::lag(leave_out_mean_TFP_gr, n = 5)
  ) 


##### Sector Indicators

# Create a new factor variable for sector groups
merged_data <- merged_data %>%
  mutate(sector_group = case_when(
    industry %in% c("Mining and quarrying", 
                    "Electricity, gas, steam and air conditioning supply",  #                                                                    D
                    "Water supply; sewerage, waste management and remediation activities",  #                                                    E
                    "Construction") ~ "Mining, Utilities and Construction",
    industry %in% c("Manufacture of food products; beverages and tobacco products", #                                                            C10-C12
                    "Manufacture of textiles, wearing apparel, leather and related products",  #                                                 C13-C15
                    "Manufacture of wood, paper, printing and reproduction",#                                                                    C16-C18
                    "Manufacture of coke and refined petroleum products", #                                                                      C19
                    "Manufacture of chemicals and chemical products", #                                                                          C20
                    "Manufacture of basic pharmaceutical products and pharmaceutical preparations",   #                                          C21
                    "Manufacture of rubber and plastic products and other non-metallic mineral products", #                                      C22-C23
                    "Manufacture of basic metals and fabricated metal products, except machinery and equipment",  #                              C24-C25
                    "Manufacture of computer, electronic and optical products", #                                                                C26
                    "Manufacture of electrical equipment",  #                                                                                    C27
                    "Manufacture of machinery and equipment n.e.c.", #                                                                           C28
                    "Manufacture of motor vehicles, trailers, semi-trailers and of other transport equipment", #                                 C29-C30
                    "Manufacture of furniture; jewellery, musical instruments, toys; repair and installation of machinery and equipment") ~ "Manufacturing",
    industry %in% c("Wholesale and retail trade and repair of motor vehicles and motorcycles",  #                                                G45
                    "Wholesale trade, except of motor vehicles and motorcycles",  #                                                              G46
                    "Retail trade, except of motor vehicles and motorcycles", #                                                                  G47
                    "Land transport and transport via pipelines", #                                                                              H49
                    "Water transport",  #                                                                                                        H50
                    "Air transport",  #                                                                                                          H51
                    "Warehousing and support activities for transportation",  #                                                                  H52
                    "Postal and courier activities",    #                                                                                        H53
                    "Accommodation and food service activities", 
                    "Real estate activities",
                    "Administrative and support service activities",  #                                           Q87-Q88
                    "Arts, entertainment and recreation",  #                                                                                     R
                    "Other service activities",
                    "Residential care activities and social work activities without accommodation") ~ "Low-Tech Services",
    industry %in% c("Publishing, motion picture, video, television programme production; sound recording, programming and broadcasting activities", # J58-J60
                    "Telecommunications", #                                                                                                      J61
                    "Computer programming, consultancy, and information service activities",  #                                                  J62-J63
                    "Financial and insurance activities",
                    "Professional, scientific and technical activities") ~ "High-Tech Services",
    industry %in% c("Education",
                    "Human health activities") ~ "Education and Health",
    TRUE ~ "Other" # This handles any industries not specifically grouped
  ))


# Prepare for running panel data methods
merged_data <- merged_data %>%
  
  # A structural break indicator being equal to 1 before 2007 and 2 afterwards.
  mutate(t = if_else(year <= 2007, 1, 2)) %>% 
  
  # Turn those into factors to use as fixed effects.
  mutate( 
    country = factor(country),
    industry = factor(industry),
    sector_group = factor(sector_group),
    year = factor(year)
  )


# Replace all infinite and NaN values.
merged_data <- merged_data %>%
  mutate(across(everything(), ~ replace(.x, is.infinite(.x) | is.nan(.x), NA)))


#### CALCULATE WEIGHTS: FULL TIME PERIOD

# Step 1: Calculate the time-averaged shares for employment, aggregate hours, and value added for each country-industry pair
average_shares <- merged_data %>%
  group_by(country, industry) %>%
  summarise(
    mean_employment = mean(employment, na.rm = TRUE),
    mean_agg_hours = mean(agg_hours, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(
    share_employment = mean_employment / sum(mean_employment, na.rm = TRUE),
    share_agg_hours = mean_agg_hours / sum(mean_agg_hours, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Calculate the total and yearly shares for employment, aggregate hours, and value added per country and year
yearly_country_shares <- merged_data %>%
  group_by(country, year) %>%
  summarise(
    sum_employment = sum(employment, na.rm = TRUE),
    sum_agg_hours = sum(agg_hours, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    total_employment_year = sum(sum_employment, na.rm = TRUE),
    share_employment_2 = sum_employment / total_employment_year,
    
    total_agg_hours_year = sum(sum_agg_hours, na.rm = TRUE),
    share_agg_hours_2 = sum_agg_hours / total_agg_hours_year
  ) %>%
  ungroup() 

# Step 3: Merge the shares and calculate the final weights for each variable
merged_data <- merged_data %>%
  left_join(average_shares, by = c("country", "industry")) %>%
  left_join(yearly_country_shares, by = c("country", "year")) %>%
  mutate(
    weights_employment = share_employment * share_employment_2,
    weights_agg_hours = share_agg_hours * share_agg_hours_2
  )


#### CALCULATE WEIGHTS: PRE-2007 PERIOD

# Step 1: Calculate the time-averaged shares for employment, aggregate hours, and value added for each country-industry pair
average_shares_A <- merged_data %>%
  filter(t == 1) %>%
  group_by(country, industry) %>%
  summarise(
    mean_employment_A = mean(employment, na.rm = TRUE),
    mean_agg_hours_A = mean(agg_hours, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(
    share_employment_A = mean_employment_A / sum(mean_employment_A, na.rm = TRUE),
    share_agg_hours_A = mean_agg_hours_A / sum(mean_agg_hours_A, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Calculate the total and yearly shares for employment, aggregate hours, and value added per country and year
yearly_country_shares_A <- merged_data %>%
  filter(t == 1) %>%
  group_by(country, year) %>%
  summarise(
    sum_employment_A = sum(employment, na.rm = TRUE),
    sum_agg_hours_A = sum(agg_hours, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    total_employment_year_A = sum(sum_employment_A, na.rm = TRUE),
    share_employment_2_A = sum_employment_A / total_employment_year_A,
    
    total_agg_hours_year_A = sum(sum_agg_hours_A, na.rm = TRUE),
    share_agg_hours_2_A = sum_agg_hours_A / total_agg_hours_year_A
  ) %>%
  ungroup() 

# Step 3: Merge the shares and calculate the final weights for each variable
merged_data <- merged_data %>%
  left_join(average_shares_A, by = c("country", "industry")) %>%
  left_join(yearly_country_shares_A, by = c("country", "year")) %>%
  mutate(
    weights_employment_pre = share_employment_A * share_employment_2_A,
    weights_agg_hours_pre = share_agg_hours_A * share_agg_hours_2_A
  )


#### CALCULATE WEIGHTS: POST-2007 PERIOD

# Step 1: Calculate the time-averaged shares for employment, aggregate hours, and value added for each country-industry pair
average_shares_B <- merged_data %>%
  filter(t == 2) %>%
  group_by(country, industry) %>%
  summarise(
    mean_employment_B = mean(employment, na.rm = TRUE),
    mean_agg_hours_B = mean(agg_hours, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(
    share_employment_B = mean_employment_B / sum(mean_employment_B, na.rm = TRUE),
    share_agg_hours_B = mean_agg_hours_B / sum(mean_agg_hours_B, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2: Calculate the total and yearly shares for employment, aggregate hours, and value added per country and year
yearly_country_shares_B <- merged_data %>%
  filter(t == 2) %>%
  group_by(country, year) %>%
  summarise(
    sum_employment_B = sum(employment, na.rm = TRUE),
    sum_agg_hours_B = sum(agg_hours, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(
    total_employment_year_B = sum(sum_employment_B, na.rm = TRUE),
    share_employment_2_B = sum_employment_B / total_employment_year_B,
    
    total_agg_hours_year_B = sum(sum_agg_hours_B, na.rm = TRUE),
    share_agg_hours_2_B = sum_agg_hours_B / total_agg_hours_year_B
  ) %>%
  ungroup() 

# Step 3: Merge the shares and calculate the final weights for each variable
merged_data <- merged_data %>%
  left_join(average_shares_B, by = c("country", "industry")) %>%
  left_join(yearly_country_shares_B, by = c("country", "year")) %>%
  mutate(
    weights_employment_post = share_employment_B * share_employment_2_B,
    weights_agg_hours_post = share_agg_hours_B * share_agg_hours_2_B
  )

# A country-industry interaction term which allows me to cluster SEs at the industry-country level.
merged_data <- merged_data %>%
  mutate(country_industry = interaction(country, industry))


# List of all regression vars, i.e. vars required for the regressions.
summary_vars <- c("industry_code", "industry", "country_code", "country", "year", "t",
                  "employment", "agg_hours", "TFP_gr", "wages", "wagebill", "labor_share", "VA_Q", "VA_CP",
                  "VA_G", "employment_gr", "agg_hours_gr", "labor_share_gr", "wagebill_gr", "wages_gr", 
                  "weights_employment", "weights_agg_hours", "weights_employment_pre", "weights_agg_hours_pre", "weights_employment_post", "weights_agg_hours_post")

# Keep only regression vars.
summary_data <- merged_data %>%
  select(all_of(summary_vars))



# List of all regression vars, i.e. vars required for the regressions.
regression_vars <- c("industry_code", "industry", "country_code", "country", "year", "t", "sector_group", "country_industry",
                     "leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5", 
                     "VA_G", "employment_gr", "agg_hours_gr", "labor_share_gr", "wagebill_gr", "wages_gr", "wagebill_cp_gr", "va_cp_gr",
                     "weights_employment", "weights_agg_hours", "weights_employment_pre", "weights_agg_hours_pre", "weights_employment_post", "weights_agg_hours_post")


# Keep only regression vars.
regression_data <- merged_data %>%
  select(all_of(regression_vars))




