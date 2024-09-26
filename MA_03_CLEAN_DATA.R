# clean data


#################################################################
##                       HYPERPARAMETERS                       ##
#################################################################

# keep relevant countries
my_countries <- c("Austria", "Belgium", "Denmark", "Finland", "France", 
                  "Germany (until 1990 former territory of the FRG)",  "Ireland", 
                  "Italy", "Japan", "Luxembourg", "Netherlands",
                  "Portugal", "Spain", "Sweden", "United Kingdom", "United States")

# keep all industries
my_industries <- c("Mining and quarrying", #                                                                                                    B
                   "Manufacture of food products; beverages and tobacco products", #                                                            C10-C12
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
                   "Manufacture of furniture; jewellery, musical instruments, toys; repair and installation of machinery and equipment", #      C31-C33
                   "Electricity, gas, steam and air conditioning supply",  #                                                                    D
                   "Water supply; sewerage, waste management and remediation activities",  #                                                    E
                   "Construction", #                                                                                                            F
                   "Wholesale and retail trade and repair of motor vehicles and motorcycles",  #                                                G45
                   "Wholesale trade, except of motor vehicles and motorcycles",  #                                                              G46
                   "Retail trade, except of motor vehicles and motorcycles", #                                                                  G47
                   "Land transport and transport via pipelines", #                                                                              H49
                   "Water transport",  #                                                                                                        H50
                   "Air transport",  #                                                                                                          H51
                   "Warehousing and support activities for transportation",  #                                                                  H52
                   "Postal and courier activities",    #                                                                                        H53
                   "Accommodation and food service activities",  #                                                                              I
                   "Publishing, motion picture, video, television programme production; sound recording, programming and broadcasting activities", # J58-J60
                   "Telecommunications", #                                                                                                      J61
                   "Computer programming, consultancy, and information service activities",  #                                                  J62-J63
                   "Financial and insurance activities",   #                                                                                    K
                   "Real estate activities", #                                                                                                  L
                   "Professional, scientific and technical activities",   #                                                                     M
                   "Administrative and support service activities",   #                                                                         N
                   "Education",  #                                                                                                              P
                   "Human health activities",  #                                                                                                Q86
                   "Residential care activities and social work activities without accommodation",  #                                           Q87-Q88
                   "Arts, entertainment and recreation",  #                                                                                     R
                   "Other service activities") #                                                                                                S


# notice that this list excludes the following industries:
    # c("Agriculture, forestry and fishing", #                                                                A
    # "Public administration and defence; compulsory social security", #                                    O
    # "Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use", # T
    # "Activities of extraterritorial organisations and bodies") # U


# All essential variables in growth accounts.
essential_vars_ga <- c("VAConTFP", # TFP Growth
                       "VA_G", # Value added growth at constant prices
                       "VA_CP", # Value-added at current prices.
                       "VA_Q", # Value-added in chain-linked volumes using 2015 as base.
                       "LAB") # Essentially the wagebill in current prices.

# All essential variables in national accounts.
essential_vars_na <- c("EMP", # Number of persons employed, th.
                       "H_EMP", # Total hours worked by persons engaged, th.
                       "COMP") # Compensation of employees, current prices, millions of national currency




#################################################################
##                       GROWTH ACCOUNTS                       ##
#################################################################

growth_accounts_data <- growth_accounts_data %>%
  
  # Remove this index variable
  select(-X) %>%                       

  # Keep only countries in my_countries. Rename germany.
  filter(geo_name %in% my_countries) %>%  
  mutate(geo_name = str_replace(geo_name, "Germany \\(until 1990 former territory of the FRG\\)", "Germany")) %>% 

  # Keep only industries in my_industries.
  filter(nace_r2_name %in% my_industries) %>% 
  
  # keep only variables we are interested in
  filter(var %in% essential_vars_ga) %>%   

  # Change format to pivot wider
  pivot_wider(                        
    names_from = var,
    values_from = value
  ) %>%
  
  # Rename a bunch of variables.
  rename(                             
    country = geo_name,
    country_code = geo_code,
    industry = nace_r2_name,
    industry_code = nace_r2_code,
    TFP_gr = VAConTFP,
    wagebill_cp = LAB
  ) 



#################################################################
##                      NATIONAL ACCOUNTS                      ##
#################################################################

national_accounts_data <- national_accounts_data %>%
  
  # Remove this index variable
  select(-X) %>%           
  
  # Keep only countries in my_countries. Rename Germany
  filter(geo_name %in% my_countries) %>% 
  mutate(geo_name = str_replace(geo_name, "Germany \\(until 1990 former territory of the FRG\\)", "Germany")) %>% 
  
  # Keep only industries in my_industries
  filter(nace_r2_name %in% my_industries) %>% 

  # Select essentials vars only.
  select(year, geo_name, geo_code, nace_r2_name, nace_r2_code, all_of(essential_vars_na)) %>%
  
  # Rename a bunch of variables.
  rename(
    country = geo_name,
    country_code = geo_code,
    industry = nace_r2_name,
    industry_code = nace_r2_code,
    employment = EMP,
    agg_hours = H_EMP
  ) %>%
  
  # There is an error that we need to fix.
  mutate(
    employment = if_else(country %in% c("Japan", "United States"), employment / 1000, employment)
  ) %>%
  
  # Get rid of country and industry. Not required due to merging.
  select(-any_of(c("country", "industry")))



##################################################################
##                       MERGE DATARFAMES                       ##
##################################################################

# Merge that data
merged_data <- growth_accounts_data %>%
  left_join(national_accounts_data, by = c("year", "country_code", "industry_code"))


      