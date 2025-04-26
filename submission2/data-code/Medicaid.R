
# Meta --------------------------------------------------------------------

## Title:  Medicaid Expansion
## Author: Genevieve DeBell 
## Date Created: 4/16/2025
## Date Edited:  4/16/2025



# Preliminaries -----------------------------------------------------------
kff.dat <- read_csv('data/input/KFF_medicaid_expansion_2019.csv')

# Clean KFF data -------------------------------------------------------

kff.final <- kff.dat %>%
  mutate(expanded = (`Expansion Status` == 'Adopted and Implemented'),
         Description = str_replace_all(Description,c("\n"='','"'='')))

kff.final$splitvar <- kff.final %>% select(Description) %>% as.data.frame() %>%
  separate(Description, sep=" ", into=c(NA, NA, NA, "date"))

kff.final <- kff.final %>%
  mutate(date_adopted = mdy(splitvar$date)) %>%
  select(State, expanded, date_adopted)
colnames(kff.dat)
write_tsv(kff.final,'data/output/medicaid_expansion.txt')

names(kff.final)
