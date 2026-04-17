###### 2 DO FIRMS MATTER FOR INEQUALITY ##########################################################
##################################################################################################

## 2.1 Firm Wage Premia

### function to find the connected set and run the AKM decomposition
run_akm <- function(data_subset) {
  
  # A. Find Largest Connected Set using igraph
  edges <- data_subset %>% 
    distinct(worker_id, firm_id)
  g <- graph_from_data_frame(edges, directed = FALSE)
  comps <- components(g)
  # Get the largest component members
  largest_comp <- which.max(comps$csize)
  nodes_in_lcs <- names(comps$membership[comps$membership == largest_comp])
  # Keep only observations where BOTH worker and firm are in the LCS
  connected_data <- data_subset %>%
    filter(worker_id %in% nodes_in_lcs | firm_id %in% nodes_in_lcs)
  
  # B. Estimate the AKM model
  model <- feols(log(wage) ~ age + experience | worker_id + firm_id + year, 
                 data = connected_data)
  
  # C. Extract Fixed Effects
  fixed_effects <- fixef(model)
  connected_data$alpha_i <- fixed_effects$worker_id[as.character(connected_data$worker_id)]
  connected_data$psi_j   <- fixed_effects$firm_id[as.character(connected_data$firm_id)]
  
  # D. Variance Decomposition
  var_total  <- var(log(connected_data$wage))
  var_worker <- var(connected_data$alpha_i, na.rm = TRUE)
  var_firm   <- var(connected_data$psi_j,   na.rm = TRUE)
  cov_wf     <- cov(connected_data$alpha_i, connected_data$psi_j, use = "complete.obs")
  
  return(tibble(
    share_worker = var_worker / var_total,
    share_firm   = var_firm   / var_total,
    share_cov    = (2 * cov_wf) / var_total
  ))
}

### Run for the two sub-periods
results_2001_2010 <- run_akm(exam_data5 %>% filter(year <= 2010))
results_2011_2020 <- run_akm(exam_data5 %>% filter(year >= 2011))

### Compare Results
comparison <- bind_rows(
  results_2001_2010 %>% mutate(period = "2001-2010"),
  results_2011_2020 %>% mutate(period = "2011-2020")
)

print(comparison)

#### plotting
plot_comparison <- comparison %>%
  pivot_longer(cols = c(share_worker, share_firm, share_cov), 
               names_to = "Component", 
               values_to = "Share") %>%
  mutate(Component = case_when(
    Component == "share_worker" ~ "Worker Effects (Alpha)",
    Component == "share_firm"   ~ "Firm Effects (Psi)",
    Component == "share_cov"    ~ "Sorting (2*Cov)"
  ))
#### Create the Bar Chart
ggplot(plot_comparison, aes(x = Component, y = Share, fill = period)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(Share, 3)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  scale_fill_manual(values = c("violet", "maroon")) +
  labs(
    title = "Wage Variance Decomposition (AKM)",
    subtitle = "Comparing 2001-2010 vs 2011-2020",
    y = "Share of Total Variance",
    x = "",
    fill = "Period"
  ) +
  theme_minimal() + theme(legend.position = "bottom") 

##################################################################################################

### 2.1.2a Compute the correlation between estimated worker and firm fixed effects among movers.

calculate_mover_corr <- function(data_subset) {
  
  ## A. Identify the LCS
  edges <- data_subset %>% distinct(worker_id, firm_id)
  g <- graph_from_data_frame(edges, directed = FALSE)
  comps <- components(g)
  largest_comp <- which.max(comps$csize)
  nodes_in_lcs <- names(comps$membership[comps$membership == largest_comp])
  connected_data <- data_subset %>%
    filter(worker_id %in% nodes_in_lcs, firm_id %in% nodes_in_lcs)
  
  ## B. Estimate the AKM model
  model <- feols(log(wage) ~ age + experience | worker_id + firm_id + year, 
                 data = connected_data)
  
  ## C. Identifyinf movers: Workers with more than 1 distinct firm_id
  movers <- connected_data %>%
    group_by(worker_id) %>%
    summarize(n_firms = n_distinct(firm_id)) %>%
    filter(n_firms > 1) %>%
    pull(worker_id)
  
  ## D. Extract effects specifically for these movers
  fe <- fixef(model)
  mover_effects <- connected_data %>%
    filter(worker_id %in% movers) %>%
    mutate(
      alpha_i = fe$worker_id[as.character(worker_id)],
      psi_j   = fe$firm_id[as.character(firm_id)]
    )
  
  ## E. Return the correlation
  return(cor(mover_effects$alpha_i, mover_effects$psi_j, use = "complete.obs"))
}

# Run for both periods
corr_2001_2010 <- calculate_mover_corr(exam_data5 %>% filter(year <= 2010))
corr_2011_2020 <- calculate_mover_corr(exam_data5 %>% filter(year >= 2011))

# Print results
cat("Correlation (Movers only) 2001-2010:", round(corr_2001_2010, 4), "\n")
cat("Correlation (Movers only) 2011-2020:", round(corr_2011_2020, 4), "\n")


### 2.1.2b  correlation between observable worker characteristics and firm effects.

calculate_obs_firm_corr <- function(data_subset) {
  
  ## A. Identify the LCS (Mirroring your previous logic)
  edges <- data_subset %>% distinct(worker_id, firm_id)
  g <- graph_from_data_frame(edges, directed = FALSE)
  comps <- components(g)
  nodes_in_lcs <- names(comps$membership[comps$membership == which.max(comps$csize)])
  connected_data <- data_subset %>%
    filter(worker_id %in% nodes_in_lcs, firm_id %in% nodes_in_lcs)
  
  ## B. Estimate the AKM model
  model <- feols(log(wage) ~ age + experience | worker_id + firm_id + year, 
                 data = connected_data)
  
  ## C. Extract Firm Effects (psi_j) and Observable Scores (X*beta)
  fe <- fixef(model)
  # Calculate X*beta manually: age_coef * age + exp_coef * experience
  betas <- coef(model)
  connected_data$obs_score <- betas["age"] * connected_data$age + 
    betas["experience"] * connected_data$experience
  connected_data$psi_j <- fe$firm_id[as.character(connected_data$firm_id)]
  
  ## D. Return Correlation
  return(cor(connected_data$obs_score, connected_data$psi_j, use = "complete.obs"))
}

# Run for both periods
obs_firm_corr_p1 <- calculate_obs_firm_corr(exam_data5 %>% filter(year <= 2010))
obs_firm_corr_p2 <- calculate_obs_firm_corr(exam_data5 %>% filter(year >= 2011))

# Print results
cat("Corr(Observables, Firm Effects) 2001-2010:", round(obs_firm_corr_p1, 4), "\n")
cat("Corr(Observables, Firm Effects) 2011-2020:", round(obs_firm_corr_p2, 4), "\n")

##################################################################################################

### 2.1.3 Regressing estimated firm fixed effects on firm-level union presence 

get_union_impact <- function(data_subset) {
  
  ## A. Estimate AKM to get firm effects
  model <- feols(log(wage) ~ age + experience | worker_id + firm_id + year, 
                 data = data_subset)
  fe <- fixef(model)
  firm_effects <- tibble(
    firm_id = names(fe$firm_id),
    psi_j = as.numeric(fe$firm_id)
  )
  
  ## B. Merge with firm-level union share
  firm_level_data <- data_subset %>%
    group_by(firm_id) %>%
    summarize(avg_union = mean(firm_union_share, na.rm = TRUE)) %>%
    inner_join(firm_effects, by = "firm_id")
  
  ## C. Regress Firm Effect on Union Presence
  reg <- lm(psi_j ~ avg_union, data = firm_level_data)
  # Return the coefficient for the union effect
  return(coef(reg)["avg_union"])
}

# Run for both
union_effect_p1 <- get_union_impact(exam_data5 %>% filter(year <= 2010))
union_effect_p2 <- get_union_impact(exam_data5 %>% filter(year >= 2011))
# Print results
cat("Union Effect on Firm Premium (2001-2010):", round(union_effect_p1, 4), "\n")
cat("Union Effect on Firm Premium (2011-2020):", round(union_effect_p2, 4), "\n")


### 2.1.4. Decomposing total wage dispersion into between-firm and within-firm components

disp_decomp <- function(data_subset) {
  
  ## A. Calculate the overall variance of log wages
  total_var <- var(data_subset$log_wage, na.rm = TRUE)
  
  ## B. Calculate Firm-Level stats
  firm_stats <- data_subset %>%
    group_by(firm_id) %>%
    summarize(
      firm_mean_wage = mean(log_wage, na.rm = TRUE),
      firm_var_wage = var(log_wage, na.rm = TRUE),
      n_workers = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(firm_var_wage)) # Need at least 2 workers to have a variance
  
  ## C. Between-Firm Variance: Variance of the firm means (weighted by firm size)
  # We use the mean of the means, weighted by n_workers
  grand_mean <- mean(data_subset$log_wage, na.rm = TRUE)
  between_var <- sum(firm_stats$n_workers * (firm_stats$firm_mean_wage - grand_mean)^2) / 
    (sum(firm_stats$n_workers) - 1)
  
  ## D. Within-Firm Variance: Average of the internal firm variances (weighted by firm size)
  within_var <- sum(firm_stats$n_workers * firm_stats$firm_var_wage) / sum(firm_stats$n_workers)
  return(tibble(
    total_variance = total_var,
    between_share  = between_var / total_var,
    within_share   = within_var / total_var
  ))
}

# Run for both periods
disp_p1 <- disp_decomp(exam_data5 %>% mutate(log_wage = log(wage)) %>% filter(year <= 2010))
disp_p2 <- disp_decomp(exam_data5 %>% mutate(log_wage = log(wage)) %>% filter(year >= 2011))

# Compare
disp_comp <- bind_rows(
  disp_p1 %>% mutate(period = "2001-2010"),
  disp_p2 %>% mutate(period = "2011-2020")
)
print(disp_comp)