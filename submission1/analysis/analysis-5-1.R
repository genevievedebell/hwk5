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

## Step 1: Load and clean KFF Medicaid expansion data
kff.dat <- read_csv('data/input/KFF_medicaid_expansion_2019.csv')

kff.final <- kff.dat %>%
  mutate(
    expanded = (`Expansion Status` == 'Adopted and Implemented'),
    Description = str_replace_all(Description, c("\n" = "", "\"" = "")),
    # Extract date from text like "1/1/2014"
    date_extracted = str_extract(Description, "\\d{1,2}/\\d{1,2}/\\d{4}"),
    date_adopted = mdy(date_extracted)
  ) %>%
  select(State, expanded, date_adopted)

## Step 2: Create expansion groups
expanded_2014_states <- kff.final %>%
  filter(expanded == TRUE,
         date_adopted >= as.Date("2014-01-01"),
         date_adopted <= as.Date("2014-12-31")) %>%
  pull(State)

not_expanded_by_2014_states <- kff.final %>%
  filter(expanded == FALSE) %>%
  pull(State)

## Step 3: Add Medicaid expansion group to insurance data
final.insurance <- final.insurance %>%
  mutate(medicaid_expansion_group = case_when(
    State %in% expanded_2014_states ~ "Expanded in 2014",
    State %in% not_expanded_by_2014_states ~ "Not Expanded by 2014",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(medicaid_expansion_group))  # Drop states that expanded after 2014

## Step 4: Summarize uninsured share by group and year
uninsured_plot_data <- final.insurance %>%
  group_by(year, medicaid_expansion_group) %>%
  summarise(
    total_uninsured = sum(uninsured, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE)
  ) %>%
  mutate(uninsured_share = total_uninsured / total_adult_pop)

## Step 5: Plot
ggplot(uninsured_plot_data, aes(x = year, y = uninsured_share, color = medicaid_expansion_group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Uninsured Share by Medicaid Expansion Status (2012–2019)",
    x = "Year",
    y = "Share Uninsured",
    color = "Medicaid Expansion Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray50")

length(expanded_2014_states)  # Should be 23
expanded_2014_states

length(expanded_2014_states)  # Should now be ~23
expanded_2014_states

kff.final %>% 
  filter(expanded == TRUE) %>% 
  select(State, date_adopted) %>% 
  arrange(date_adopted) %>% 
  print(n = 30)

summary(kff.final$date_adopted)
