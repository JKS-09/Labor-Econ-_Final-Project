library(tidyverse)
library(readr)
library(dplyr)
library(fixest)
library(igraph)
library(multcomp)
library(vtable)
library(ggplot2)

## load data 
exam_data5 <- read_csv("Desktop/Labor Econ/exam_data5.csv")

##################################################################################################
###### 1 WAGE INEQUALITY #########################################################################
##################################################################################################

### 1.1 AGGREGATE INEQUALITY 
## 1.1.1 Calculate the percentiles by year
inequality_trends <- exam_data5 %>%
  filter(wage > 0) %>% # Ensures no non-positive wages for log calculations
  group_by(year) %>%
  summarize(
    p10 = quantile(wage, 0.10, na.rm = TRUE),
    p50 = quantile(wage, 0.50, na.rm = TRUE),
    p90 = quantile(wage, 0.90, na.rm = TRUE)
  ) %>%
  ## Compute the log differences
  mutate(
    overall_inequality = log(p90) - log(p10),
    bottom_inequality  = log(p50) - log(p10),
    top_inequality     = log(p90) - log(p50)
  )
## View the results
print(inequality_trends)

#Pivot the data for plotting
plot_inequality <- inequality_trends %>%
  dplyr::select(year, overall_inequality, bottom_inequality, top_inequality) %>%
  pivot_longer(cols = -year, names_to = "Measure", values_to = "Log_Difference")
#Create the Plot
ggplot(plot_inequality, aes(x = year, y = Log_Difference, color = Measure)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(values = c("overall_inequality" = "navy", "bottom_inequality" = "firebrick",
                                "top_inequality" = "violetred3"
  ), 
  labels = c("overall_inequality" = "Overall (90/10)", "bottom_inequality" = "Bottom (50/10)",
             "top_inequality" = "Top (90/50)"
  )) +
  labs( title = "Evolution of Wage Inequality (Top, Middle and Bottom)",
        x = "Year",
        y = "Log Points",
        color = "Inequality Measure"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


### 1.1.3 Excluding workers paid below MW
ineq_no_min <- exam_data5 %>%
  filter(wage >= min_wage) %>% 
  group_by(year) %>%
  summarize(
    p10 = quantile(wage, 0.10, na.rm = TRUE),
    p50 = quantile(wage, 0.50, na.rm = TRUE),
    p90 = quantile(wage, 0.90, na.rm = TRUE)
  ) %>%
  mutate(
    overall_9010 = log(p90) - log(p10),
    bottom_5010  = log(p50) - log(p10),
    top_9050     = log(p90) - log(p50),
    Type = "Excluding below MW"
  )

## Combine with your original data for comparison
ineq_trends_lab <- inequality_trends %>%
  mutate(Type = "Full Sample") %>%
  rename(overall_9010 = overall_inequality, 
         bottom_5010 = bottom_inequality, 
         top_9050 = top_inequality)
comp_df <- bind_rows(ineq_trends_lab, ineq_no_min)

## Pivot for plotting
plot_comp <- comp_df %>%
  dplyr::select(year, Type, overall_9010, bottom_5010, top_9050) %>%
  pivot_longer(cols = c(overall_9010, bottom_5010, top_9050), 
               names_to = "Measure", values_to = "Log_Difference")

ggplot(plot_comp, aes(x = year, y = Log_Difference, color = Type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~Measure, scales = "free_y", 
             labeller = as_labeller(c("overall_9010" = "90-10 (Overall)", "bottom_5010" = "50-10 (Bottom)", 
                                      "top_9050" = "90-50 (Top)"))) +
  labs(title = "Inequality Trends: Full Sample vs. Above Min Wage",,
       y = "Log Points", x = "Year") +
  theme_minimal() +   theme(legend.position = "bottom") +
  scale_color_manual(values = c("Full Sample" = "seagreen4", "Excluding below MW" = "violetred2"))

print(comp_df, n= 50)

##################################################################################################

### 1.2 BETWEEN AND WITHIN GROUP INEQUALITY 
## checking the age groups in the data
unique(exam_data5$age) 
summary(exam_data5$age)

# 1.2.1 Decomposing Wage Dispersion (Raw)
## data prep
analysis_data <- exam_data5 %>%
  filter(wage > 0) %>%
  mutate(
    log_wage = log(wage),
    age_bin = case_when(
      age >= 25 & age <= 39 ~ "Young",
      age >= 40 & age <= 59 ~ "Middle",
      age >= 60              ~ "Old"
    )
  )

#### Between-Group Gap 
between_raw <- analysis_data %>%
  filter(education %in% c("BAplus", "HSorLess")) %>%
  group_by(year, female, education) %>%
  summarize(mean_log_w = mean(log_wage, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = education, values_from = mean_log_w) %>%
  mutate(between_gap = BAplus - HSorLess)

#### Within-Group Gap 
within_raw <- analysis_data %>%
  group_by(year, female) %>%
  group_modify(~ {
    fit <- lm(log_wage ~ factor(education), data = .x)
    tibble(within_gap = quantile(residuals(fit), 0.90) - quantile(residuals(fit), 0.10))
  }) %>%
  ungroup()

#### Plot
plot_data_p1 <- between_raw %>%
  left_join(within_raw, by = c("year", "female")) %>%
  mutate(Gender = factor(female, labels = c("Men", "Women")))
ggplot(plot_data_p1, aes(x = year)) +
  geom_line(aes(y = between_gap, color = "Between-Edu (BA-HS Gap)"), linewidth = 1) +
  geom_line(aes(y = within_gap, color = "Within-Edu (90/10)"), linewidth = 1) +
  facet_wrap(~Gender) +
  labs(title = "Wage Dispersion Decomposition", y = "Log Points", color = "Component") +
  theme_minimal() +  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Within-Edu (90/10)" = "steelblue3", 
                                "Between-Edu (BA-HS Gap)" = "rosybrown3")) 


#### 1.2.2
# (i) Computing age-bin shares in the base year (2001)
base_year <- min(analysis_data$year)

base_weights <- analysis_data %>%
  filter(year == base_year) %>%
  group_by(female, education, age_bin) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(share = count / sum(count)) %>%
  dplyr::select(female, education, age_bin, share)

# (ii) computing age-bin mean log-wages in each subsequent year 
# Compute age-bin mean log wages in each year
age_bin_means <- analysis_data %>%
  group_by(year, female, education, age_bin) %>%
  summarize(mean_log_w = mean(log_wage, na.rm = TRUE), .groups = "drop")
## View(age_bin_means) -- for personal use

# (iii) Reweighting  means using base-year shares
adjusted_between <- age_bin_means %>%
  inner_join(base_weights, by = c("female", "education", "age_bin")) %>%
  group_by(year, female, education) %>%   # Calculate the weighted average: (Wage * Base_Year_Share)
  summarize(adj_mean_log_w = sum(mean_log_w * share), .groups = "drop") %>%
  filter(education %in% c("BAplus", "HSorLess")) %>%   # Focus on our two education groups
  pivot_wider(names_from = education, values_from = adj_mean_log_w) %>%   # Pivot to calculate the gap
  mutate(between_gap_adj = BAplus - HSorLess)

#### Merge the ADJUSTED between-gap with the existing within-gap
plot_data_p2 <- adjusted_between %>%
  left_join(within_raw, by = c("year", "female")) %>%
  mutate(Gender = factor(female, labels = c("Men", "Women")))

#### Generating the Adjusted Graph 
ggplot(plot_data_p2, aes(x = year)) +
  geom_line(aes(y = between_gap_adj, color = "Between-Edu (Age-Adj)"), linewidth = 1) +
  geom_line(aes(y = within_gap, color = "Within-Edu (90/10)"), linewidth = 1) +
  
  facet_wrap(~Gender) +
  labs(
    title = "Wage Dispersion (Fixed Age Comp at 2001)",
    y = "Log Points", 
    color = "Component"
  ) +
  theme_minimal() + theme(legend.position = "bottom") +
  scale_color_manual(values = c("Between-Edu (Age-Adj)" = "orchid3", 
                                "Within-Edu (90/10)" = "steelblue3"))

## since these graphs look basically identical to me, lez see what their means have to say about this
mean(plot_data_p1$between_gap)
mean(plot_data_p2$between_gap_adj)

#### they clearly have different means so we can overlay the graphs together and create a master dataset for it
# Merge the raw and adjusted education gaps into one table
comparison_data <- between_raw %>%
  dplyr::select(year, female, between_gap) %>%
  left_join(adjusted_between %>% 
              dplyr::select(year, female, between_gap_adj), 
            by = c("year", "female")) %>%
  mutate(Gender = factor(female, labels = c("Men", "Women")))
### A. Create the Consolidated Dataset
final_master_data <- comparison_data %>%
  # Bring in the within-group (residual) data we calculated earlier
  left_join(within_raw %>% 
              mutate(Gender = factor(female, labels = c("Men", "Women"))) %>%
              dplyr::select(year, Gender, within_gap), 
            by = c("year", "Gender")) %>%
  # Pivot to long format for ggplot
  pivot_longer(cols = c(between_gap, between_gap_adj, within_gap),
               names_to = "Inequality_Type", 
               values_to = "Log_Points") %>%
  # Give them clean labels for the legend
  mutate(Inequality_Type = case_when(
    Inequality_Type == "between_gap" ~ "Between-Ed (Raw)",
    Inequality_Type == "between_gap_adj" ~ "Between-Ed (Age-Adj)",
    Inequality_Type == "within_gap" ~ "Within-Group (90/10)"
  ))

### B. Plot the Master Comparison
ggplot(final_master_data, aes(x = year, y = Log_Points, 
                              color = Inequality_Type, 
                              linetype = Inequality_Type)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~Gender) +
  scale_linetype_manual(values = c("Between-Ed (Raw)" = "solid", 
                                   "Between-Ed (Age-Adj)" = "solid", 
                                   "Within-Group (90/10)" = "solid")) +
  scale_color_manual(values = c("Between-Ed (Raw)" = "rosybrown3", 
                                "Between-Ed (Age-Adj)" = "orchid3", 
                                "Within-Group (90/10)" = "steelblue3")) +
  labs(
    title = "Wage Inequality: Men vs. Women (Raw vs. Age-Adj vs (90/10))",
    x = "Year",
    y = "Inequality (lp)",
    color = "Measure",
    linetype = "Measure"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

### C. Summary Table of growth for the write-up --- mostly for us and not the paper
# summary_table <- final_master_data %>%
# filter(year %in% c(min(year), max(year))) %>% # Keep only the start and end years
# pivot_wider(names_from = year, values_from = Log_Points) %>%   # Spread years into columns
# mutate(Total_change = `2020` - `2001`) 
# print(summary_table)


##################################################################################################

#### 1.3 ROLE OF UNIONS IN INEQUALITY 
## 1.3.1 comparing wage dispersion across firms with diff union presence
## Creating Union Terciles based on firm_union_share
union_analysis <- exam_data5 %>%
  filter(wage > 0) %>%
  mutate(log_wage = log(wage)) %>%
  mutate(union_group = ntile(firm_union_share, 3)) %>%
  mutate(union_group = factor(union_group, labels = c("Low Union", "Mid Union", "High Union")))

## calculating inequality measures per group
union_inequality <- union_analysis %>%
  group_by(union_group) %>%
  summarize(
    p90_p10 = quantile(log_wage, 0.9) - quantile(log_wage, 0.1),
    p90_p50 = quantile(log_wage, 0.9) - quantile(log_wage, 0.5),
    p50_p10 = quantile(log_wage, 0.5) - quantile(log_wage, 0.1)
  )
print(union_inequality)

## 1.3.2 examining dispersion
# Preparing Event Study Data
event_study_data <- exam_data5 %>%
  filter(wage > 0) %>%
  mutate(relative_year = year - union_jump_year) %>%
  filter(relative_year >= -5 & relative_year <= 5) %>%
  mutate(log_wage = log(wage))

# calculating inequality for each relative year
event_trends <- event_study_data %>%
  group_by(relative_year) %>%
  summarize(
    p90_p10 = quantile(log_wage, 0.9) - quantile(log_wage, 0.1),
    p90_p50 = quantile(log_wage, 0.9) - quantile(log_wage, 0.5),
    p50_p10 = quantile(log_wage, 0.5) - quantile(log_wage, 0.1)
  )

# plotting -- year 0 is event year and not calendar year. it means the year the union was formed
event_trends_long <- event_trends %>%
  pivot_longer(cols = starts_with("p"), names_to = "measure", values_to = "value")

ggplot(event_trends_long, aes(x = relative_year, y = value, color = measure)) +
  geom_line(size = 1) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "slateblue4") + # The Jump Year
  labs(title = "Impact of Unionization on Wage Dispersion",
       x = "Years Relative to Union Jump",
       y = "Log Difference") + 
  theme_minimal() +   theme(legend.position = "bottom")

## 1.3.4 unionization compression
union_effects <- exam_data5 %>%
  filter(wage > 0) %>%
  mutate(log_wage = log(wage)) %>%
  mutate(union_group = ntile(firm_union_share, 3)) %>%
  mutate(union_group = factor(union_group, labels = c("Low Union", "Mid Union", "High Union"))) %>%
  group_by(union_group) %>%
  summarize(
    # A. Within Firm --- inequality within the group
    within_dispersion = {
      fit <- lm(log_wage ~ factor(education) + age + female)
      quantile(residuals(fit), 0.9) - quantile(residuals(fit), 0.1)
    },
    # B. Between group -- Education Premium -- Mean of BAplus - mean of HSorLess within Union layers
    edu_gap = mean(log_wage[education == "BAplus"], na.rm = TRUE) - 
      mean(log_wage[education == "HSorLess"], na.rm = TRUE),
    .groups = "drop"
  )

print(union_effects)

## Comparison
union_eff_long <- union_effects %>%
  pivot_longer(cols = c(within_dispersion, edu_gap), 
               names_to = "Mechanism", values_to = "Value")
## Plots 
ggplot(union_eff_long, aes(x = union_group, y = Value, fill = Mechanism, group = Mechanism)) +
  geom_col(position = "identity", show.legend = FALSE) +
  geom_line(aes(color = Mechanism), linewidth = 1.2) +
  geom_point(aes(color = Mechanism), size = 3) +
  facet_wrap(~Mechanism, 
             labeller = as_labeller(c("within_dispersion" = "Within-Group (Residual 90/10)", 
                                      "edu_gap" = "Between-Ed (BA vs HS Gap)"))) +
  scale_fill_manual(values = c("within_dispersion" = "slateblue3", "edu_gap" = "slategray2")) +
  scale_color_manual(values = c("within_dispersion" = "orchid1", "edu_gap" = "slategray")) +
  # Bar height represents log points; line shows the compression gradient"
  labs(title = "Union Compression: Magnitude and Trend",
       x = "Firm Union Density",
       y = "Log Points") +  theme(legend.position = "bottom") +
  theme_minimal() 