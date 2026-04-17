##################################################################################################
### 3 MINIMUM WAGES, UNIONS & NON COMPLIANCE######################################################
##################################################################################################

### 3.1 Estimation
#Prepare indicators for Employment and Non-compliance
analysis_mw <- exam_data5 %>%
  mutate(
    log_wage = log(wage),
    non_compliant = ifelse(employed == 1 & wage < min_wage, 1, 0)
  )

### 3.1.1 Minimum wage only 
### a) No FE
# Outcome 1: Log Wages
reg_wage <- feols(log_wage ~ min_wage + age + female + factor(education), 
                  data = analysis_mw %>% filter(employed == 1))
# Outcome 2: Employment (Run on the WHOLE dataset)
reg_emp <- feols(employed ~ min_wage + age + female + factor(education), 
                 data = analysis_mw)
# Outcome 3: Non-compliance (Indicator for earning < Min Wage)
reg_noncomp <- feols(non_compliant ~ min_wage + age + female + factor(education), 
                     data = analysis_mw %>% filter(employed == 1))
# Results
etable(reg_wage, reg_emp, reg_noncomp)


### b) Firm FE
# Outcome 1: Log Wages
wage_firm_fe <- feols(log_wage ~ min_wage + female + age + factor(education) | firm_id, 
                      data = analysis_mw %>% filter(employed == 1))
# Outcome 2: Employment
emp_firm_fe <- feols(employed ~ min_wage + age + female + factor(education) | firm_id, 
                     data = analysis_mw)
# Outcome 3: Non-compliance
noncomp_firm_fe <- feols(non_compliant ~ min_wage + age + female + factor(education) | firm_id, 
                         data = analysis_mw %>% filter(employed == 1))
# View Results
etable(wage_firm_fe, emp_firm_fe, noncomp_firm_fe)


### c) Worker and Firm FE
# Outcome 1: Log Wages
wage_double_fe <- feols(log_wage ~ min_wage + age + female + factor(education) | worker_id + firm_id, 
                        data = analysis_mw %>% filter(employed == 1))
# Outcome 2: Employment
# Note: This measures if the worker stays employed within that worker-firm match.
emp_double_fe <- feols(employed ~ min_wage + age + female + factor(education) | worker_id + firm_id, 
                       data = analysis_mw)
# Outcome 3: Non-compliance
noncomp_double_fe <- feols(non_compliant ~ min_wage + age + female + factor(education) | worker_id + firm_id, 
                           data = analysis_mw %>% filter(employed == 1))
# View results side-by-side
etable(wage_double_fe, emp_double_fe, noncomp_double_fe)


### d) Worker, Firm and State*Year FE
# Outcome 1: Log Wages (Interaction with Gender)
wage_st_yr <- feols(log_wage ~ min_wage + age + female + factor(education) | worker_id + firm_id + state^year, 
                    data = analysis_mw %>% filter(employed == 1))
# Outcome 2: Employment
emp_st_yr <- feols(employed ~ min_wage + age + female + factor(education) | worker_id + firm_id + state^year, 
                   data = analysis_mw)
# Outcome 3: Non-compliance
noncomp_st_yr <- feols(non_compliant ~ min_wage + age + female + factor(education) | 
                         worker_id + firm_id + state^year, data = analysis_mw %>% filter(employed == 1))
# View results
etable(wage_st_yr, emp_st_yr, noncomp_st_yr)

## since we got some interesting results, I'm asking how many unique min_wage values exist per state-year?
analysis_mw %>%
  group_by(state, year) %>%
  summarise(n_distinct_mw = n_distinct(min_wage)) %>%
  summary()
### this means that all the variation in min_wage can be explained by state*year FE

##################################################################################################


### 3.1.2 Unions and Interaction
# A firm is 'union' if the current year is >= the year it unionized; if a firm never unionizes (NA), union is 0.
analysis_union <- analysis_mw %>%
  mutate(
    union = ifelse(!is.na(union_jump_year) & year >= union_jump_year, 1, 0)
  )

## a) No FE
# Outcome 1: Log Wages
wage_u <- feols(log_wage ~ min_wage * union + age + female + factor(education), 
                data = analysis_union %>% filter(employed == 1))
# Outcome 2: Employment
emp_u <- feols(employed ~ min_wage * union + age + female + factor(education), 
               data = analysis_union)
# Outcome 3: Non-compliance
noncomp_u <- feols(non_compliant ~ min_wage * union + age + female + factor(education), 
                   data = analysis_union %>% filter(employed == 1))
# Results
etable(wage_u, emp_u, noncomp_u)


## b) Firm FE
# Outcome 1: Log Wages
wage_u_fe <- feols(log_wage ~ min_wage * union + age + female + factor(education) | firm_id, 
                   data = analysis_union %>% filter(employed == 1))
# Outcome 2: Employment
emp_u_fe <- feols(employed ~ min_wage * union + age + female + factor(education) | firm_id, 
                  data = analysis_union)
# Outcome 3: Non-compliance
noncomp_u_fe <- feols(non_compliant ~ min_wage * union + age + female + factor(education) | firm_id, 
                      data = analysis_union %>% filter(employed == 1))
# Results
etable(wage_u_fe, emp_u_fe, noncomp_u_fe)


## c) Worker & Firm FE
# Outcome 1: Log Wages
wage_u_double <- feols(log_wage ~ min_wage * union + age + female + factor(education) | worker_id + firm_id, 
                       data = analysis_union %>% filter(employed == 1))
# Outcome 2: Employment
emp_u_double <- feols(employed ~ min_wage * union + age + female + factor(education)| worker_id + firm_id, 
                      data = analysis_union)
# Outcome 3: Non-compliance
noncomp_u_double <- feols(non_compliant ~ min_wage * union + age + female + factor(education)| 
                            worker_id + firm_id, data = analysis_union %>% filter(employed == 1))
# results
etable(wage_u_double, emp_u_double, noncomp_u_double)


### d) Worker, Firm and State*Year FE
# Outcome 1: Log Wages 
wage_u_st_yr <- feols(log_wage ~ min_wage*union + age + female + factor(education)| 
                        worker_id + firm_id + state^year, data = analysis_union %>% filter(employed == 1))
# Outcome 2: Employment
emp_u_st_yr <- feols(employed ~ min_wage*union + age + female + factor(education) | 
                       worker_id + firm_id + state^year, data = analysis_union)
# Outcome 3: Non Compliance
noncomp_u_st_yr <- feols(non_compliant ~ min_wage*union + age + female + factor(education) | 
                           worker_id + firm_id + state^year, data = analysis_union %>% filter(employed == 1))
# Results
etable(wage_u_st_yr, emp_u_st_yr, noncomp_u_st_yr)

##################################################################################################

### 3.1.2.2 total marginal effect of union coverage 
# Find the mean minimum wage
avg_mw <- mean(analysis_union$min_wage, na.rm = TRUE)
max_mw <- max(analysis_union$min_wage, na.rm = TRUE)
# Extract coefficients from your Double FE model
beta1 <- coef(wage_u_double)["min_wage"]
beta2 <- coef(wage_u_double)["union"]
beta3 <- coef(wage_u_double)["min_wage:union"]
# Compute the Total Marginal Effect at the Mean
total_effect_mean <- beta2 + (beta3 * avg_mw)
# Compute the Total Marginal Effect at the Max
total_effect_max <- beta2 + (beta3 * max_mw)
# Results
cat("Marginal Effect at Mean MW:", total_effect_mean, "\n")
cat("Marginal Effect at Max MW:", total_effect_max, "\n")

##################################################################################################
##################################################################################################
## CHECKING THE SIGNFICNACE OF THIS THING ABOVE
# Define the 'meaningful value' of MW, here it is 10$ 
# mw_val <- 10 
# Run the hypothesis test for Non-Compliance
# Equation: union + (min_wage:union * 10) = 0 and we use the double FE model as the base
#hyp_test <- glht(noncomp_u_double, 
#    linfct = c(paste0("union + `min_wage:union` * ", mw_val, " = 0")))
# summary(hyp_test)
##################################################################################################
##################################################################################################


### 3.2 Non Compliance
# Calculate non-compliance rate by Year and State
noncomp_summary <- analysis_mw %>%
  group_by(year, state) %>%
  summarise(
    noncomp_rate = mean(non_compliant, na.rm = TRUE),
    avg_mw = mean(min_wage, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate National average for comparison
national_summary <- noncomp_summary %>%
  group_by(year) %>%
  summarise(national_rate = mean(noncomp_rate))

# graph 1: Non-Compliance over Time (National)
ggplot(national_summary, aes(x = year, y = national_rate)) +
  geom_line(color = "palevioletred1", linewidth = 1.5) +
  geom_point() +
  labs(title = "Share of workers earning below MW",
       x = "Year", y = "Non-Compliance Rate") +
  theme_minimal() +   theme(legend.position = "bottom")

# graph 2: Non-Compliance by State (Top 10 most recent year)
latest_year <- max(noncomp_summary$year)
noncomp_summary %>%
  filter(year == latest_year) %>%
  ggplot(aes(x = reorder(state, noncomp_rate), y = noncomp_rate)) +
  geom_bar(stat = "identity", fill = "mediumorchid3", alpha = .5) +
  coord_flip() +
  labs(title = paste("Non-Compliance Rate by State in", latest_year),
       x = "State", y = "Non-Compliance Rate") +
  theme_minimal()
# FOR THE LINE GRAPH: 
# first we summarize by State and Year
state_year_comp <- analysis_mw %>%
  group_by(year, state) %>%
  summarise(noncomp_rate = mean(non_compliant, na.rm = TRUE))
# and we plot 
ggplot(state_year_comp, aes(x = year, y = noncomp_rate, color = state, group = state)) +
  geom_line(alpha = 0.5, size = 0.8) + 
  geom_point(size = 1) +
  labs(title = "Evolution of Non-Compliance Rates by State",
       subtitle = "Percentage of workers earning below the minimum wage threshold",
       x = "Year", 
       y = "Non-Compliance Rate",
       color = "State") +
  theme_minimal() + theme(legend.position = "bottom")