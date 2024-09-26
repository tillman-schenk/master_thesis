## regressions baby

# Some lists for regressions
all <- list()
t1 <- list()
t2 <- list()
rea <- list()
reb <- list()
rep <- list()
rob <- list()

# 1  Employment
# 2. Aggregate Hours
# 3. Hourly Wages
# 4. Wagebill
# 5. Labor Share
# 6. Value-Added

# A function to obtain clustered standard errors.
get_clustered_se <- function(model, cluster_var) {
  
  # Compute the clustered standard errors using vcovCR
  clustered_se <- vcovCR(model, cluster = cluster_var, type = "CR2")
  
  # Extract the standard errors
  se_clustered <- sqrt(diag(clustered_se))
  
  return(se_clustered)
}


#################################################################
##       FULL SAMPLE - REPRODUCING TABLE 5 IN A&S (2018)       ##
##            EMPLOYMENT; HOURS AND NOMINAL WAGEBILL           ##
#################################################################


### Employment: without sector group
rea$emp <- lm(employment_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_employment) 
summary(rea$emp)$coe[2:7,]

# sum of coeffs
coef(rea$emp)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_emp <- get_clustered_se(rea$emp, regression_data$country_industry)



### Employment: with sector group
rea$emp_sec <- lm(employment_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_employment) 
summary(rea$emp_sec)$coe[2:7,]

# sum of coeffs
coef(rea$emp_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_emp_sec <- get_clustered_se(rea$emp_sec, regression_data$country_industry)



### Aggregate Hours: without sector group
rea$agh <- lm(agg_hours_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_agg_hours) 
summary(rea$agh)$coe[2:7,]

# sum of coeffs
coef(rea$agh)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_agh <- get_clustered_se(rea$agh, regression_data$country_industry)



### Aggregate Hours: with sectorgroup
rea$agh_sec <- lm(agg_hours_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_agg_hours) 
summary(rea$agh_sec)$coe[2:7,]

# sum
coef(rea$agh_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_agh_sec <- get_clustered_se(rea$agh_sec, regression_data$country_industry)



### Nominal Wagebill: without sectorgroup
rea$cwb <- lm(wagebill_cp_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rea$cwb)$coe[2:7,]

# sum
coef(rea$cwb)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_cwb <- get_clustered_se(rea$cwb, regression_data$country_industry)



### Nominal Wagebill: with sectorgroup
rea$cwb_sec <- lm(wagebill_cp_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(rea$cwb_sec)$coe[2:7,]

# sum
coef(rea$cwb_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_cwb_sec <- get_clustered_se(rea$cwb_sec, regression_data$country_industry)



### The table
stargazer(rea$emp, rea$emp_sec, rea$agh, rea$agh_sec, rea$cwb, rea$cwb_sec,
          type = "latex",
          se = list(cse_emp, cse_emp_sec, cse_agh, cse_agh_sec, cse_cwb, cse_cwb_sec),
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Nom. Wage \\\\ Bill}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}'$",
                               "$\\Delta \\ln \\text{TFP}'_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Industry f.e.", "No", "No", "No", "No", "No", "No"),
            c("Sector group f.e.", "No", "Yes", "No", "Yes", "No", "Yes"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes")
          ))




#################################################################
##       FULL SAMPLE - REPRODUCING TABLE 5 IN A&S (2018)       ##
##            NOMINAL VA, REAL VA AND THE LABORSHARE           ##
#################################################################


### Nominal Value-Added: without sectogroup fe
reb$cva <- lm(va_cp_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(reb$cva)$coe[2:7,]

#sum
coef(reb$cva)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_cva <- get_clustered_se(reb$cva, regression_data$country_industry)



### Nominal Value-Added: with sectorgroup fe
reb$cva_sec <- lm(va_cp_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(reb$cva_sec)$coe[2:7,]

#sum
coef(reb$cva_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_cva_sec <- get_clustered_se(reb$cva_sec, regression_data$country_industry)



### Real Value-Added: without sectorgroup fe
reb$vad <- lm(VA_G ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(reb$vad)$coe[2:7,]

#sum
coef(reb$vad)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_vad <- get_clustered_se(reb$vad, regression_data$country_industry)



### Real Value-Added: with sectorgroup fe
reb$vad_sec <- lm(VA_G ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(reb$vad_sec)$coe[2:7,]

# sum
coef(reb$vad_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_vad_sec <- get_clustered_se(reb$vad_sec, regression_data$country_industry)



### Labor Share: without sectorgroup fes
reb$las <- lm(labor_share_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(reb$las)$coe[2:7,]

# sum
coef(reb$las)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_las <- get_clustered_se(reb$las, regression_data$country_industry)



### Labor Share: with sectorgroup fes
reb$las_sec <- lm(labor_share_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(reb$las_sec)$coe[2:7,]

# sum
coef(reb$las_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_las_sec <- get_clustered_se(reb$las_sec, regression_data$country_industry)



### The table
stargazer(reb$cva, reb$cva_sec, reb$vad, reb$vad_sec, reb$las, reb$las_sec,
          type = "latex",
          se = list(cse_cva, cse_cva_sec, cse_vad, cse_vad_sec, cse_las, cse_las_sec),
          dep.var.labels = c("\\shortstack{Nominal \\\\ Value-Added}", 
                             "\\shortstack{Real \\\\ Value-Added}", 
                             "\\shortstack{Labor- \\\\ share}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}'$",
                               "$\\Delta \\ln \\text{TFP}'_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Sector group f.e.", "No", "Yes", "No", "Yes", "No", "Yes"),
            c("Industry f.e.", "No", "No", "No", "No", "No", "No")
          ))




#################################################################
##        FULL SAMPLE - ROBUSTNESS CHECK W. INDUSTRY FE        ##
#################################################################


### Employment
rob$emp <- lm(employment_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
              data = regression_data,
              weights = weights_employment) 
summary(rob$emp)$coe[1:6,]

# sum
coef(rob$emp)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_emp_rob <- get_clustered_se(rob$emp, regression_data$country_industry)



### Aggregate Hours
rob$agh <- lm(agg_hours_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
              data = regression_data,
              weights = weights_agg_hours) 
summary(rob$agh)$coe[1:6,]

#sum
coef(rob$agh)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_agh_rob <- get_clustered_se(rob$agh, regression_data$country_industry)



### Nominal Wagebill
rob$cwb <- lm(wagebill_cp_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rob$cwb)$coe[1:6,]

#sum
coef(rob$cwb)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_cwb_rob <- get_clustered_se(rob$cwb, regression_data$country_industry)



### Nominal Value-Added
rob$cva <- lm(va_cp_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rob$cva)$coe[1:6,]

#sum
coef(rob$cva)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_cva_rob <- get_clustered_se(rob$nva, regression_data$country_industry)



### Real Value-Added
rob$vad <- lm(VA_G ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rob$vad)$coe[1:6,]

#sum
coef(rob$vad)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_vad_rob <- get_clustered_se(rob$vad, regression_data$country_industry)



### Labor Share
rob$las <- lm(labor_share_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rob$las)$coe[1:6,]

#sum
coef(rob$las)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_las_rob <- get_clustered_se(rob$las, regression_data$country_industry)



### The table
stargazer(rob$emp, rob$agh, rob$cwb, rob$cva, rob$vad, rob$las,
          type = "latex",
          se = list(cse_emp_rob, cse_agh_rob, cse_cwb_rob, cse_cva_rob, cse_vad_rob, cse_las_rob),
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Nom. Wage \\\\ Bill}", 
                             "\\shortstack{Nom. \\\\ VA}",
                             "\\shortstack{Real \\\\ VA}",
                             "\\shortstack{Labor-\\\\ share}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}'$",
                               "$\\Delta \\ln \\text{TFP}'_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}'_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Sector group f.e.", "No", "No", "No", "No", "No"),
            c("Industry f.e.", "Yes", "Yes", "Yes", "Yes", "Yes")
          ))




#################################################################
##        FULL SAMPLE - REAL WAGE BILL AND HOURLY WAGES        ##
#################################################################


### Real Wagebill - No sec, no ind
rob$rwb <- lm(wagebill_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rob$rwb)$coe[1:6,]

#sum
coef(rob$rwb)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_rwb_rob <- get_clustered_se(rob$rwb, regression_data$country_industry)



### Real Wagebill - with sec
rob$rwb_sec <- lm(wagebill_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rob$rwb_sec)$coe[1:6,]

#sum
coef(rob$rwb_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_rwb_rob_sec <- get_clustered_se(rob$rwb_sec, regression_data$country_industry)



### Real Wagebill - with ind
rob$rwb_ind <- lm(wagebill_gr ~ leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(rob$rwb_ind)$coe[1:6,]

#sum
coef(rob$rwb_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_rwb_rob_ind <- get_clustered_se(rob$rwb_ind, regression_data$country_industry)



### Hourly Wages - No sec, no ind
rob$wag <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rob$wag)$coe[1:6,]

# sum
coef(rob$wag)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_wag_rob <- get_clustered_se(rob$wag, regression_data$country_industry)


### Hourly Wages - With sec
rob$wag_sec <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rob$wag_sec)$coe[1:6,]

# sum
coef(rob$wag_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_wag_rob_sec <- get_clustered_se(rob$wag_sec, regression_data$country_industry)


### Hourly Wages - With ind
rob$wag_ind <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(rob$wag_ind)$coe[1:6,]

# sum
coef(rob$wag_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# clustered SEs
cse_wag_rob_ind <- get_clustered_se(rob$wag_ind, regression_data$country_industry)



a <- rob$rwb
b <- rob$rwb_sec
c <- rob$rwb_ind
d <- rob$wag
e <- rob$wag_sec
f <- rob$wag_ind


### The table
stargazer(a, b, c, d, e, f,
          type = "latex",
          se = list(cse_rwb_rob, cse_rwb_rob_sec, cse_rwb_rob_ind, cse_wag_rob, cse_wag_rob_sec, cse_wag_rob_ind),
          dep.var.labels = c("\\shortstack{Real \\\\ Wage Bill}", 
                             "\\shortstack{Real \\\\ Wages}"),
          covariate.labels = c("$\\Delta \\log \\text{TFP}'$",
                               "$\\Delta \\log \\text{TFP}'_{t-1}$",
                               "$\\Delta \\log \\text{TFP}'_{t-2}$",
                               "$\\Delta \\log \\text{TFP}'_{t-3}$",
                               "$\\Delta \\log \\text{TFP}'_{t-4}$",
                               "$\\Delta \\log \\text{TFP}'_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Sector group f.e.", "No", "Yes", "No", "No", "Yes", "No"),
            c("Industry f.e.", "No", "No", "Yes", "No", "No", "Yes")
          ))








#################################################################
##           FULL SAMPLE - w. INDUSTRY FIXED EFFECTS           ##
#################################################################

### Employment
all$emp_ind <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data,
                  weights = weights_employment) 
summary(all$emp_ind)$coe[1:6,]

coef(all$emp_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Aggregate Hours
all$agh_ind <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data,
                  weights = weights_agg_hours) 
summary(all$agh_ind)$coe[1:6,]

coef(all$agh_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
all$wag_ind <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$wag_ind)$coe[1:6,]

coef(all$wag_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Wagebill
all$wab_ind <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$wab_ind)$coe[1:6,]

coef(all$wab_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Labor Share
all$las_ind <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$las_ind)$coe[1:6,]

coef(all$las_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Value Added
all$vad_ind <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$vad_ind)$coe[1:6,]

coef(all$vad_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


# Full sample regressions 
stargazer(all$emp_ind, all$agh_ind,all$wab_ind, all$las_ind, all$vad_ind,
          type = "latex",
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Wage \\\\ Bill}", 
                             "\\shortstack{Labor \\\\ Share}",
                             "\\shortstack{Real \\\\ VA}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}$",
                               "$\\Delta \\ln \\text{TFP}_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Industry f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Sector group f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes")
          ))



##################################################################
##             FULL SAMPLE - STANDARD SPECIFICATION             ##
##################################################################


### Employment
all$emp <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                         data = regression_data,
                         weights = weights_employment) 
summary(all$emp)$coe[1:6,]

coef(all$emp)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# linearHypothesis(all$emp, "leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 = 0")


### Aggregate Hours
all$agh <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                        data = regression_data,
                        weights = weights_agg_hours) 
summary(all$agh)$coe[1:6,]

coef(all$agh)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
all$wag <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                    data = regression_data,
                    weights = weights_agg_hours)
summary(all$wag)$coe[1:6,]

coef(all$wag)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Wagebill
all$wab <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                       data = regression_data,
                       weights = weights_agg_hours)
summary(all$wab)$coe[1:6,]

coef(all$wab)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Labor Share
all$las <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                          data = regression_data,
                          weights = weights_agg_hours)
summary(all$las)$coe[1:6,]

coef(all$las)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Value Added
all$vad <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                          data = regression_data,
                          weights = weights_agg_hours)
summary(all$vad)$coe[1:6,]

coef(all$vad)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


# Full sample regressions 
stargazer(all$emp, all$agh, all$wag, all$wab, all$las, all$vad,
          type = "latex",
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Real \\\\ Wages}", 
                             "\\shortstack{Wage \\\\ Bill}", 
                             "\\shortstack{Labor \\\\ Share}",
                             "\\shortstack{Real \\\\ VA}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}$",
                               "$\\Delta \\ln \\text{TFP}_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Industry f.e.", "No", "No", "No", "No", "No", "No"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
          ))



##################################################################
##                  FULL SAMPLE - ONLY TIME FE                  ##
##################################################################

### Employment
all$emp_tim <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + year,
                  data = regression_data,
                  weights = weights_employment) 
summary(all$emp_tim)$coe[1:6,]

coef(all$emp_tim)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Aggregate Hours
all$agh_tim <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + year,
                  data = regression_data,
                  weights = weights_agg_hours) 
summary(all$agh_tim)$coe[1:6,]

coef(all$agh_tim)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
all$wag_tim <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$wag_tim)$coe[1:6,]

coef(all$wag_tim)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Wagebill
all$wab_tim <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$wab_tim)$coe[1:6,]

coef(all$wab_tim)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Labor Share
all$las_tim <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$las_tim)$coe[1:6,]

coef(all$las_tim)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Value Added
all$vad_tim <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$vad_tim)$coe[1:6,]

coef(all$vad_tim)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


# Full sample regressions 
stargazer(all$emp_tim, all$agh_tim, all$wab_tim, all$las_tim, all$vad_tim,
          type = "latex",
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Wage \\\\ Bill}", 
                             "\\shortstack{Labor \\\\ Share}",
                             "\\shortstack{Real \\\\ VA}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}$",
                               "$\\Delta \\ln \\text{TFP}_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "No", "No", "No", "No", "No"),
            c("Industry f.e.", "No", "No", "No", "No", "No"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes")
          ))




#################################################################
##                     FULL SAMPLE - NO FE                     ##
#################################################################


### Employment
all$emp_no <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5,
                  data = regression_data,
                  weights = weights_employment) 
summary(all$emp_no)$coe[1:6,]

coef(all$emp_no)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Aggregate Hours
all$agh_no <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5,
                  data = regression_data,
                  weights = weights_agg_hours) 
summary(all$agh_no)$coe[1:6,]

coef(all$agh_no)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
all$wag_no <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$wag_no)$coe[1:6,]

coef(all$wag_no)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Wagebill
all$wab_no <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$wab_no)$coe[1:6,]

coef(all$wab_no)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Labor Share
all$las_no <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$las_no)$coe[1:6,]

coef(all$las_no)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Value Added
all$vad_no <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$vad_no)$coe[1:6,]

coef(all$vad_no)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


# Full sample regressions 
stargazer(all$emp_no, all$agh_no,all$wab_no, all$las_no, all$vad_no,
          type = "latex",
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Wage \\\\ Bill}", 
                             "\\shortstack{Labor \\\\ Share}",
                             "\\shortstack{Real \\\\ VA}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}$",
                               "$\\Delta \\ln \\text{TFP}_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "No", "No", "No", "No", "No"),
            c("Industry f.e.", "No", "No", "No", "No", "No"),
            c("Year f.e.", "No", "No", "No", "No", "No")
          ))



##################################################################
##           FULL SAMPLE - SECTOR GROUP FIXED EFFECTS           ##
##################################################################

### Employment
all$emp_sec <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_employment) 
summary(all$emp_sec)$coe[1:6,]

coef(all$emp_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Aggregate Hours
all$agh_sec <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_agg_hours) 
summary(all$agh_sec)$coe[1:6,]

coef(all$agh_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
all$wag_sec <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$wag_sec)$coe[1:6,]

coef(all$wag_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Wagebill
all$wab_sec <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$wab_sec)$coe[1:6,]

coef(all$wab_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Labor Share
all$las_sec <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$las_sec)$coe[1:6,]

coef(all$las_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Value Added
all$vad_sec <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$vad_sec)$coe[1:6,]

coef(all$vad_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


# Full sample regressions 
stargazer(all$emp_sec, all$agh_sec,all$wab_sec, all$las_sec, all$vad_sec,
          type = "latex",
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Wage \\\\ Bill}", 
                             "\\shortstack{Labor \\\\ Share}",
                             "\\shortstack{Real \\\\ VA}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}$",
                               "$\\Delta \\ln \\text{TFP}_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "sector_group", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Industry f.e.", "No", "No", "No", "No", "No"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Sector Group f.e.", "Yes", "Yes", "Yes", "Yes", "Yes")
          ))



#################################################################
##             FULL SAMPLE - NOMINAL WAGEBILL ONLY             ##
#################################################################


### Wagebill
all$cwb <- lm(wagebill_cp_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(all$cwb)$coe[1:6,]

coef(all$cwb)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### No fixed effects.
all$cwb_no <- lm(wagebill_cp_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5,
                 data = regression_data,
                 weights = weights_agg_hours)
summary(all$cwb_no)$coe[1:6,]

coef(all$cwb_no)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

### Sector group regression
all$cwb_sec <- lm(wagebill_cp_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$cwb_sec)$coe[1:6,]

coef(all$cwb_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

### with industry f.e.
all$cwb_ind <- lm(wagebill_cp_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$cwb_ind)$coe[1:6,]

coef(all$cwb_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


##################################################################
##            FULL SAMPLE - NOMINAL VALUE-ADDED ONLY            ##
##################################################################

### standard
all$cva <- lm(va_cp_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data,
              weights = weights_agg_hours)
summary(all$cva)$coe[1:6,]

coef(all$cva)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

### only time f.e.
all$cva_tim <- lm(va_cp_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$cva_tim)$coe[1:6,]

coef(all$cva_tim)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

### no f.e.
all$cva_no <- lm(va_cp_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5,
                 data = regression_data,
                 weights = weights_agg_hours)
summary(all$cva_no)$coe[1:6,]

coef(all$cva_no)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

### sector group
all$cva_sec <- lm(va_cp_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + sector_group + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$cva_sec)$coe[1:6,]

coef(all$cva_sec)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

### w industry f.e.
all$cva_ind <- lm(va_cp_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data,
                  weights = weights_agg_hours)
summary(all$cva_ind)$coe[1:6,]

coef(all$cva_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()




#################################################################
##              PRE-2007 - STANDARD SPECIFICATION              ##
#################################################################

### Employment
t1$emp <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                         data = regression_data %>%
                           filter(t == 1),
                        weights = weights_employment)
summary(t1$emp)$coe[1:6,]

coef(t1$emp)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Aggregate Hours
t1$agh <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                        data = regression_data %>%
                          filter(t == 1),
                        weights = weights_agg_hours) 
summary(t1$agh)$coe[1:6,]

coef(t1$agh)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
t1$wag <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                    data = regression_data %>%
                      filter(t == 1),
                   weights = weights_agg_hours)
summary(t1$wag)$coe[1:6,]

coef(t1$wag)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Wagebill
t1$wab <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                       data = regression_data %>%
                         filter(t == 1),
                      weights = weights_agg_hours)
summary(t1$wab)$coe[1:6,]

coef(t1$wab)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Labor Share
t1$las <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                          data = regression_data %>%
                            filter(t == 1),
                         weights = weights_agg_hours)
summary(t1$las)$coe[1:6,]

coef(t1$las)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Value Added
t1$vad <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                          data = regression_data %>%
                            filter(t == 1),
                         weights = weights_agg_hours)
summary(t1$vad)$coe[1:6,]

coef(t1$vad)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

# Pre-2007 table
stargazer(t1$emp, t1$agh, t1$wag, t1$wab, t1$las, t1$vad,
          type = "latex",
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Real \\\\ Wages}", 
                             "\\shortstack{Wage \\\\ Bill}", 
                             "\\shortstack{Labor \\\\ Share}",
                             "\\shortstack{Real \\\\ VA}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}$",
                               "$\\Delta \\ln \\text{TFP}_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Industry f.e.", "No", "No", "No", "No", "No", "No"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
          ))



##################################################################
##             PRE-2007 - INDUSTRY FE SPECIFICATION             ##
##################################################################



### Employment
t1$emp_ind <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data %>%
                   filter(t == 1),
                  weights = weights_employment_pre) 
summary(t1$emp_ind)$coe[1:6,]

coef(t1$emp_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Aggregate Hours
t1$agh_ind <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data %>%
                   filter(t == 1),
                  weights = weights_agg_hours_pre) 
summary(t1$agh_ind)$coe[1:6,]

coef(t1$agh_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
t1$wag_ind <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data %>%
                   filter(t == 1),
                  weights = weights_agg_hours_pre)
summary(t1$wag_ind)$coe[1:6,]

coef(t1$wag_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Wagebill
t1$wab_ind <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data %>%
                   filter(t == 1),
                  weights = weights_agg_hours_pre)
summary(t1$wab_ind)$coe[1:6,]

coef(t1$wab_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Labor Share
t1$las_ind <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data %>%
                   filter(t == 1),
                  weights = weights_agg_hours_pre)
summary(t1$las_ind)$coe[1:6,]

coef(t1$las_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Value Added
t1$vad_ind <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                  data = regression_data %>%
                   filter(t == 1),
                  weights = weights_agg_hours_pre)
summary(t1$vad_ind)$coe[1:6,]

coef(t1$vad_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


# Full sample regressions 
stargazer(t1$emp_ind, t1$agh_ind,t1$wab_ind, t1$las_ind, t1$vad_ind,
          type = "latex",
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Wage \\\\ Bill}", 
                             "\\shortstack{Labor \\\\ Share}",
                             "\\shortstack{Real \\\\ VA}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}$",
                               "$\\Delta \\ln \\text{TFP}_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Industry f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Sector Group f.e.", "No", "No", "No", "No", "No")
          ))



#################################################################
##           PRE-2007 - PARSIMONOUS FE SPECIFICATION           ##
#################################################################


### Employment
t1$emp_pin <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + industry + year,
                 data = regression_data %>%
                   filter(t == 1),
                 weights = weights_employment_pre) 
summary(t1$emp_ind)$coe[1:6,]

coef(t1$emp_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2")] %>% sum()


### Aggregate Hours
t1$agh_pin <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + industry + year,
                 data = regression_data %>%
                   filter(t == 1),
                 weights = weights_agg_hours_pre) 
summary(t1$agh_ind)$coe[1:6,]

coef(t1$agh_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
t1$wag_pin <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + industry + year,
                 data = regression_data %>%
                   filter(t == 1),
                 weights = weights_agg_hours_pre)
summary(t1$wag_ind)$coe[1:6,]

coef(t1$wag_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2")] %>% sum()


### Wagebill
t1$wab_pin <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + industry + year,
                 data = regression_data %>%
                   filter(t == 1),
                 weights = weights_agg_hours_pre)
summary(t1$wab_ind)$coe[1:6,]

coef(t1$wab_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2")] %>% sum()


### Labor Share
t1$las_ind <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + industry + year,
                 data = regression_data %>%
                   filter(t == 1),
                 weights = weights_agg_hours_pre)
summary(t1$las_ind)$coe[1:6,]

coef(t1$las_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2")] %>% sum()


### Value Added
t1$vad_ind <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + industry + year,
                 data = regression_data %>%
                   filter(t == 1),
                 weights = weights_agg_hours_pre)
summary(t1$vad_ind)$coe[1:6,]

coef(t1$vad_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2")] %>% sum()




##################################################################
##              POST-2007 - STANDARD SPECIFICATION              ##
##################################################################

### Employment
t2$emp <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data %>%
                filter(t == 2),
              weights = weights_employment) # 3 lags
summary(t2$emp)$coe[1:6,]

coef(t2$emp)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Aggregate Hours
t2$agh <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data %>%
                filter(t == 2),
             weights = weights_agg_hours) # 3 lags
summary(t2$agh)$coe[1:6,]

coef(t2$agh)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
t2$wag <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data %>%
                filter(t == 2),
             weights = weights_agg_hours)
summary(t2$wag)$coe[1:6,]

coef(t2$wag)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Wagebill
t2$wab <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data %>%
                filter(t == 2),
             weights = weights_agg_hours)
summary(t2$wab)$coe[1:6,]

coef(t2$wab)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Labor Share
t2$las <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
              data = regression_data %>%
                filter(t == 2),
             weights = weights_agg_hours)
summary(t2$las)$coe[1:6,]

coef(t2$las)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Value Added
t2$vad <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
             data = regression_data %>%
               filter(t == 2),
             weights = weights_agg_hours)
summary(t2$vad)$coe[1:6,]

coef(t2$vad)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()



# Post-2007 table
stargazer(t2$emp, t2$agh, t2$wag, t2$wab, t2$las, t2$vad,
          type = "latex",
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Real \\\\ Wages}", 
                             "\\shortstack{Wage \\\\ Bill}", 
                             "\\shortstack{Labor \\\\ Share}",
                             "\\shortstack{Real \\\\ VA}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}$",
                               "$\\Delta \\ln \\text{TFP}_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Industry f.e.", "No", "No", "No", "No", "No", "No"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")
          ))



##################################################################
##             POST-2007 - INDUSTRY FE SPECIFICATION            ##
##################################################################


### Employment
t2$emp_ind <- lm(employment_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                 data = regression_data %>%
                   filter(t == 2),
                 weights = weights_employment_post) 
summary(t2$emp_ind)$coe[1:6,]

coef(t2$emp_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Aggregate Hours
t2$agh_ind <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                 data = regression_data %>%
                   filter(t == 2),
                 weights = weights_agg_hours_post) 
summary(t2$agh_ind)$coe[1:6,]

coef(t2$agh_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Hourly Wages
t2$wag_ind <- lm(wages_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                 data = regression_data %>%
                   filter(t == 2),
                 weights = weights_agg_hours_post)
summary(t2$wag_ind)$coe[1:6,]

coef(t2$wag_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Wagebill
t2$wab_ind <- lm(wagebill_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                 data = regression_data %>%
                   filter(t == 2),
                 weights = weights_agg_hours_post)
summary(t2$wab_ind)$coe[1:6,]

coef(t2$wab_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Labor Share
t2$las_ind <- lm(labor_share_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                 data = regression_data %>%
                   filter(t == 2),
                 weights = weights_agg_hours_post)
summary(t2$las_ind)$coe[1:6,]

coef(t2$las_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### Value Added
t2$vad_ind <- lm(VA_G ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                 data = regression_data %>%
                   filter(t == 2),
                 weights = weights_agg_hours_post)
summary(t2$vad_ind)$coe[1:6,]

coef(t2$vad_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


# Full sample regressions 
stargazer(t2$emp_ind, t2$agh_ind,t2$wab_ind, t2$las_ind, t2$vad_ind,
          type = "latex",
          dep.var.labels = c("\\shortstack{Employ- \\\\ ment}", 
                             "\\shortstack{Aggregate \\\\ Hours}", 
                             "\\shortstack{Wage \\\\ Bill}", 
                             "\\shortstack{Labor \\\\ Share}",
                             "\\shortstack{Real \\\\ VA}"),
          covariate.labels = c("$\\Delta \\ln \\text{TFP}$",
                               "$\\Delta \\ln \\text{TFP}_{t-1}$",
                               "$\\Delta \\ln \\text{TFP}_{t-2}$",
                               "$\\Delta \\ln \\text{TFP}_{t-3}$",
                               "$\\Delta \\ln \\text{TFP}_{t-4}$",
                               "$\\Delta \\ln \\text{TFP}_{t-5}$"),
          keep = c("leave_out_mean_TFP_gr", 
                   "leave_out_mean_TFP_gr_lag1", 
                   "leave_out_mean_TFP_gr_lag2", 
                   "leave_out_mean_TFP_gr_lag3", 
                   "leave_out_mean_TFP_gr_lag4", 
                   "leave_out_mean_TFP_gr_lag5"),
          omit = c("country", "industry", "year"),
          omit.stat = c("f", "ser"), 
          digits = 2,
          out = str_c(path$table_folder, "reg_tfp_fullsample.tex"),
          add.lines = list(
            c("Country f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Industry f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Year f.e.", "Yes", "Yes", "Yes", "Yes", "Yes"),
            c("Sector Group f.e.", "No", "No", "No", "No", "No")
          ))



##################################################################
##               FULL SAMPLE AGG HOUR - BY SECTOR               ##
##################################################################

sec <- list()


########### LOW TECH

### STANDARD
sec$agh_lts_sta <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                      data = regression_data %>%
                        filter(sector_group == "Low-Tech Services"),
                      weights = weights_employment) 
summary(sec$agh_lts_sta)$coe[1:6,]

coef(sec$agh_lts_sta)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()

 
### W INDUSTRY
sec$agh_lts_ind <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                 data = regression_data %>%
                   filter(sector_group == "Low-Tech Services"),
                 weights = weights_employment) 
summary(sec$agh_lts_ind)$coe[1:6,]

coef(sec$agh_lts_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()



########## HIGH TECH

### STANDARD
sec$agh_hts_sta <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                      data = regression_data %>%
                        filter(sector_group == "High-Tech Services"),
                      weights = weights_employment) 
summary(sec$agh_hts_sta)$coe[1:6,]

coef(sec$agh_hts_sta)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### W INDUSTRY
sec$agh_hts_ind <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                      data = regression_data %>%
                        filter(sector_group == "High-Tech Services"),
                      weights = weights_employment) 
summary(sec$agh_hts_ind)$coe[1:6,]

coef(sec$agh_hts_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


########### "Mining, Utilities and Construction"

### STANDARD
sec$agh_muc_sta <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                      data = regression_data %>%
                        filter(sector_group == "Mining, Utilities and Construction"),
                      weights = weights_employment) 
summary(sec$agh_muc_sta)$coe[1:6,]

coef(sec$agh_muc_sta)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### W INDUSTRY
sec$agh_muc_ind <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                      data = regression_data %>%
                        filter(sector_group == "Mining, Utilities and Construction"),
                      weights = weights_employment) 
summary(sec$agh_muc_ind)$coe[1:6,]

coef(sec$agh_muc_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


########### "Education and Health" 

### STANDARD
sec$agh_edh_sta <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                      data = regression_data %>%
                        filter(sector_group == "Education and Health" ),
                      weights = weights_employment) 
summary(sec$agh_edh_sta)$coe[1:6,]

coef(sec$agh_edh_sta)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### W INDUSTRY
sec$agh_edh_ind <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                      data = regression_data %>%
                        filter(sector_group == "Education and Health" ),
                      weights = weights_employment) 
summary(sec$agh_edh_ind)$coe[1:6,]

coef(sec$agh_edh_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()



################ "Manufacturing" 

### STANDARD
sec$agh_mfg_sta <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + country + year,
                      data = regression_data %>%
                        filter(sector_group == "Manufacturing"),
                      weights = weights_employment) 
summary(sec$agh_mfg_sta)$coe[1:6,]

coef(sec$agh_mfg_sta)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()


### W INDUSTRY
sec$agh_mfg_ind <- lm(agg_hours_gr ~ 0 + leave_out_mean_TFP_gr + leave_out_mean_TFP_gr_lag1 + leave_out_mean_TFP_gr_lag2 + leave_out_mean_TFP_gr_lag3 + leave_out_mean_TFP_gr_lag4 + leave_out_mean_TFP_gr_lag5 + industry + country + year,
                      data = regression_data %>%
                        filter(sector_group == "Manufacturing"),
                      weights = weights_employment) 
summary(sec$agh_mfg_ind)$coe[1:6,]

coef(sec$agh_mfg_ind)[c("leave_out_mean_TFP_gr", "leave_out_mean_TFP_gr_lag1", "leave_out_mean_TFP_gr_lag2", "leave_out_mean_TFP_gr_lag3", "leave_out_mean_TFP_gr_lag4", "leave_out_mean_TFP_gr_lag5")] %>% sum()



