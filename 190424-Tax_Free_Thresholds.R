library("tidyverse")
library("readxl")
library("janitor")

# Local path to Excel file containing 2017 Tax Stats individuals table 2B
file_name <- "C:/Users/Dominic/Downloads/ts17individual02lodgmentmethodsextaxablestatusstateageyear.xlsx"

# Importing annual (2010-11 to 2016-17) file with janitor name repair 
indiv_tax_stats <- read_excel(path = file_name, sheet = "Individuals Table 2B", skip = 2, .name_repair = make_clean_names)


# Adding personal income tax schedule for the 2018-19 income year
personal_income_tax <- tibble(
  income = seq(from = 0, to = 200000, by = 1),
  pit = case_when(
    income <= 18000 ~ 0,
    income <= 37000 ~ ((income - 18201) * 0.1900),
    income <= 90000 ~ ((income - 37001) * 0.3250) + 3572,
    income <= 18000 ~ ((income - 90001) * 0.3700) + 20797,
    TRUE ~ ((income - 180001) * 0.4500) + 54097
  )
)

# Calculating __average taxable income__ and __total income or loss__ per cohort
indiv_tax_stats <- indiv_tax_stats %>% 
  mutate(
    avg_sal_and_wage = salary_or_wages / salary_or_wages_no,
    avg_tot_y_or_l   = total_income_or_loss3 / total_income_or_loss3_no,
    pit = case_when(
      avg_tot_y_or_l <= 18000 ~ 0,
      avg_tot_y_or_l <= 37000 ~ ((avg_tot_y_or_l - 18201) * 0.1900),
      avg_tot_y_or_l <= 90000 ~ ((avg_tot_y_or_l - 37001) * 0.3250) + 3572,
      avg_tot_y_or_l <= 18000 ~ ((avg_tot_y_or_l - 90001) * 0.3700) + 20797,
      TRUE ~ ((avg_tot_y_or_l - 180001) * 0.4500) + 54097
    )) %>% 
  select(1:9, avg_sal_and_wage, everything())

# Graph of incomes for taxpayers with "Non Taxable" designation 
indiv_tax_stats %>% 
  filter(taxable_status == "Non Taxable") %>% 
  ggplot(aes(x = avg_sal_and_wage)) + 
  geom_histogram()

# It turns out that the Baby Boomers are the ones breaking the $20,000 threshold
# of "Non Taxable"
indiv_tax_stats %>% 
  filter(taxable_status == "Non Taxable", avg_sal_and_wage >20000) %>% 
  distinct(age_range2)



df <- left_join(indiv_tax_stats, personal_income_tax, by = c("avg_tot_y_or_l" = "income")) %>% 
  select(1:9, avg_sal_and_wage, avg_tot_y_or_l, pit)
