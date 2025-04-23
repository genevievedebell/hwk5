# 1. Plot the share of the adult population with direct purchase health insurance over time.

library(ggplot2)
library(dplyr)

# Calculate the share of direct-purchase insurance
plot_data <- final.insurance %>%
  group_by(year) %>%
  summarise(
    total_direct = sum(ins_direct, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE)
  ) %>%
  mutate(direct_share = total_direct / total_adult_pop)

# Plot it
ggplot(plot_data, aes(x = year, y = direct_share)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Share of Adult Population with Direct Purchase Insurance (2012–2019)",
       x = "Year",
       y = "Direct Purchase Insurance Share") +
  theme_minimal()

#2. Discuss the reduction in direct purchase health insurance in later years. Can you list a couple of policies that might have affected the success of the direct purchase insurance market?

## Since 2016, the share of adults with direct purchase insurance has decreased. The Tax Cuts and Jobs Act of 2017 eliminated the penalty for not having insurance starting in 2019. Without this mandate, fewer healthy individuals opted into coverage because there was no penalty. 

# 3. Plot the share of the adult population with Medicaid over time.

# Calculate Medicaid share nationally by year
medicaid_plot_data <- final.insurance %>%
  group_by(year) %>%
  summarise(
    total_medicaid = sum(ins_medicaid, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE)
  ) %>%
  mutate(medicaid_share = total_medicaid / total_adult_pop)

# Plot
ggplot(medicaid_plot_data, aes(x = year, y = medicaid_share)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Share of Adult Population with Medicaid Coverage (2012–2019)",
       x = "Year",
       y = "Medicaid Coverage Share") +
  theme_minimal()

# 4. Plot the share of uninsured over time, separately by states that expanded Medicaid in 2014 versus those that did not. Drop all states that expanded after 2014.

### states that expanded in 2014
expanded <- final.data %>%
  group_by(State) %>%
  summarize(first_expand_year = unique(year(date_adopted))) %>%
  mutate(
    expand_group = case_when(
      is.na(first_expand_year) ~ "Never Expanded",
      first_expand_year == 2014 ~ "Expanded in 2014",
      TRUE ~ NA_character_  # Drop states with other expansion years
    )
  ) %>%
  filter(!is.na(expand_group))

### join to main data
final.data.exp <- final.data %>%
  inner_join(expanded, by = "State")

### calculate uninsured share by year and expansion group
uninsured.share <- final.data.exp %>%
  group_by(year, expand_group) %>%
  summarize(
    total_uninsured = sum(uninsured, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE),
    share_uninsured = total_uninsured / total_adult_pop,
    .groups = "drop"
  )

### plot
uninsured.plot <- ggplot(uninsured.share, aes(x = year, y = share_uninsured, color = expand_group)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Uninsured Rate by Medicaid Expansion Status (2012–2019)",
    x = "Year",
    y = "Share Uninsured",
    color = "Expansion Status"
  ) +
  theme_minimal()

print(uninsured.plot)

# 5. Calculate the average percent of uninsured individuals in 2012 and 2015, separately for expansion and non-expansion states. Present your results in a basic 2x2 DD table.

library(dplyr)
library(tidyr)

dd_2x2 <- final.data.exp %>%
  filter(year %in% c(2012, 2015)) %>%
  group_by(expand_group, year) %>%
  summarise(
    total_uninsured = sum(uninsured, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE),
    avg_uninsured_rate = total_uninsured / total_adult_pop,
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = avg_uninsured_rate,
    names_prefix = "year_"
  )

# View final DiD table
print(dd_2x2)

# 6. Estimate the effect of Medicaid expansion on the uninsurance rate using a standard DD regression estimator, again focusing only on states that expanded in 2014 versus those that never expanded.

library(fixest)
library(dplyr)

# Create treatment and post indicators
final.data.exp <- final.data.exp %>%
  mutate(
    treat = ifelse(expand_group == "Expanded in 2014", 1, 0),
    post = ifelse(year >= 2014, 1, 0),
    uninsured_rate = uninsured / adult_pop
  )

  # DiD regression without fixed effects
did_model <- feols(uninsured_rate ~ treat * post, data = final.data.exp)

# View results
summary(did_model)

# 7. Include state and year fixed effects in your estimates. Try using the lfe or fixest package to estimate this instead of directly including the fixed effects.

library(fixest)

# Estimate DiD model with state and year fixed effects
did_fe_model <- feols(
  uninsured_rate ~ treat * post | State + year,
  data = final.data.exp
)

# View the summary
summary(did_fe_model)
