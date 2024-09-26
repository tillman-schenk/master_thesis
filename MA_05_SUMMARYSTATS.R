# summary stats



##################################################################
##                  Table: Summary per Country                  ##
##################################################################

summary_table_1 <- summary_data %>%
  
  # get aggregate values per country and year.
  group_by(year, country) %>%
  summarise(
    employment = sum(employment, na.rm = TRUE),
    agg_hours = sum(agg_hours, na.rm = TRUE),
    wagebill = sum(wagebill, na.rm = TRUE), 
    VA_Q = sum(VA_Q, na.rm = TRUE)
  ) %>%
  
  # Now get the mean across the years.
  group_by(country) %>%
  summarise(
    mean_employment = mean(employment, na.rm = TRUE)/1000,
    mean_agg_hours = mean(agg_hours, na.rm = TRUE)/1000000,
    mean_wagebill = mean(wagebill, na.rm = TRUE)/1000000,
    mean_VA_Q = mean(VA_Q, na.rm = TRUE)/1000000
  ) %>%
  
  # Calculate the labor share.
  mutate(
    mean_labor_share = mean_wagebill/mean_VA_Q
  ) %>%
  
  # Round to two digits.
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  
  # Country should no longer be a factor.
  mutate(country = as.character(country))

# Now output that as a table.
stargazer(summary_table_1, 
          type = "latex", 
          summary = FALSE, 
          digits = 2,
          title = "Time-averaged labor market indicators per country.", 
          label = "tab:summary_per_country")





#################################################################
##                 Table: Summary per Industry                 ##
#################################################################

summary_table_2 <- summary_data %>%
  
  # get aggregate hours per industry.
  group_by(industry, industry_code) %>%
  summarise(
    agg_hours = sum(agg_hours, na.rm = TRUE)
  ) %>%
  
  # Calculate the total hours across the dataset and obtain industry-share.
  ungroup() %>%
  mutate(
    sum_agg_hours = sum(agg_hours, na.rm = TRUE),
    share = (agg_hours/sum_agg_hours)*100
  ) %>%
  
  # Round to two digits.
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  
  # Turn into a string and add the percentage sign.
  mutate(across(where(is.numeric), ~ str_c(as.character(.x), "%"))) %>%
  
  # Get rid of aggregate hours and the sum.
  select(-any_of(c("agg_hours", "sum_agg_hours"))) %>%
  
  # Country should no longer be a factor.
  mutate(industry = as.character(industry)) %>%
  
  # Turn the industry into rownames.
  column_to_rownames(var = "industry") %>%
  
  # And now sort by industry code.
  arrange(industry_code)
  

# Now output that as a table.
stargazer(summary_table_2, 
          type = "latex", 
          summary = FALSE, 
          digits = 2,
          title = "Overview over industries and shares of aggregate employment.", 
          label = "tab:summary_per_industry")




#################################################################
##               Table: Summary of Growth Rates.               ##
#################################################################

basic <- list()

t1 <- list()
t2 <- list()

# Basic regressions
basic$emp <- lm(employment_gr ~ 1, data = summary_data, weights = weights_employment)
basic$agh <- lm(agg_hours_gr ~ 1, data = summary_data, weights = weights_agg_hours)
basic$wag <- lm(wages_gr ~ 1, data = summary_data, weights = weights_agg_hours)
basic$las <- lm(labor_share_gr ~ 1, data = summary_data, weights = weights_agg_hours)
basic$vad <- lm(VA_G ~ 1, data = summary_data, weights = weights_agg_hours)
basic$tfp <- lm(TFP_gr ~ 1, data = summary_data, weights = weights_agg_hours)


stargazer(basic$emp, basic$agh, basic$wag, basic$las, basic$vad, basic$tfp,
          type = "latex",  # Use "html" or "latex" for other formats
          title = "Mean Estimates with Standard Errors.",
          align = TRUE,  # Align the columns
          dep.var.labels.include = FALSE,
          column.labels = c("Employment", "Aggregate Hours", "Real Wages", "Labor Share", "Real Value-Added", "TFP"),
          covariate.labels = c("100 $\times$ Mean Annual Log Change"),
          out = "regression_table.tex")


# Pre-2007 regressions
t1$emp <- lm(employment_gr ~ 1, data = summary_data %>%
               filter(t==1), weights = weights_employment_pre)
t1$agh <- lm(agg_hours_gr ~ 1, data = summary_data %>%
               filter(t==1), weights = weights_agg_hours_pre)
t1$wag <- lm(wages_gr ~ 1, data = summary_data %>%
               filter(t==1), weights = weights_agg_hours_pre)
t1$las <- lm(labor_share_gr ~ 1, data = summary_data %>%
               filter(t==1), weights = weights_agg_hours_pre)
t1$vad <- lm(VA_G ~ 1, data = summary_data %>%
               filter(t==1), weights = weights_agg_hours_pre)
t1$tfp <- lm(TFP_gr ~ 1, data = summary_data %>%
               filter(t==1), weights = weights_agg_hours_pre)


stargazer(t1$emp, t1$agh, t1$wag, t1$las, t1$vad, t1$tfp,
          type = "latex",  # Use "html" or "latex" for other formats
          title = "Mean Estimates with Standard Errors.",
          align = TRUE,  # Align the columns
          dep.var.labels.include = FALSE,
          column.labels = c("Employment", "Aggregate Hours", "Real Wages", "Labor Share", "Real Value-Added", "TFP"),
          covariate.labels = c("100 $\times$ Mean Annual Log Change"),
          out = "regression_table.tex")


# Post-2007 regressions
t2$emp <- lm(employment_gr ~ 1, data = summary_data %>%
               filter(t==2), weights = weights_employment_post)
t2$agh <- lm(agg_hours_gr ~ 1, data = summary_data %>%
               filter(t==2), weights = weights_agg_hours_post)
t2$wag <- lm(wages_gr ~ 1, data = summary_data %>%
               filter(t==2), weights = weights_agg_hours_post)
t2$las <- lm(labor_share_gr ~ 1, data = summary_data %>%
               filter(t==2), weights = weights_agg_hours_post)
t2$vad <- lm(VA_G ~ 1, data = summary_data %>%
               filter(t==2), weights = weights_agg_hours_post)
t2$tfp <- lm(TFP_gr ~ 1, data = summary_data %>%
               filter(t==2), weights = weights_agg_hours_post)


stargazer(t2$emp, t2$agh, t2$wag, t2$las, t2$vad, t2$tfp,
          type = "latex",  # Use "html" or "latex" for other formats
          title = "Mean Estimates with Standard Errors.",
          align = TRUE,  # Align the columns
          dep.var.labels.include = FALSE,
          column.labels = c("Employment", "Aggregate Hours", "Real Wages", "Labor Share", "Real Value-Added", "TFP"),
          covariate.labels = c("100 $\times$ Mean Annual Log Change"),
          out = "regression_table.tex")


stargazer(model1, model2, 
          type = "latex", 
          title = "Mean Estimates with Standard Errors",
          dep.var.labels.include = FALSE,  # Remove dependent variable labels
          column.labels = c("Employment Growth", "Aggregate Hours Growth"),  # Column labels
          covariate.labels = "Intercept (Mean)",  # Label for the intercept
          omit.stat = c("rsq", "adj.rsq", "f", "ser"),  # Omit unnecessary statistics
          digits = 2)

stargazer(test_model, type = 'latex', summary = FALSE, rownames = FALSE,
          title = "A test")


selected_data <- merged_data %>%
  ungroup() %>%
  select(employment_gr, agg_hours_gr) %>%
  na.omit() %>% 
  as.data.frame()

str(selected_data)

stargazer(selected_data, 
          type = "latex", 
          summary.stat = c("mean", "se"), 
          title = "Summary Statistics", 
          digits = 2)


class(merged_data$employment_gr)