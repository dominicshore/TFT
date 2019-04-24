library("tidyverse") # requires dplyr ver 0.8.0.1
library("readxl")    # requires readxl ver 1.3.1
library("janitor")   # requires janitor ver 1.2.0

# Local path to Excel file containing 2017 Tax Stats individuals table 2B
file_name <- "./ts17individual02lodgmentmethodsextaxablestatusstateageyear.xlsx"

# Importing annual (2010-11 to 2016-17) file with janitor name repair
indiv_tax_stats <- read_excel(path = file_name, sheet = "Individuals Table 2B", skip = 2, .name_repair = make_clean_names)

names(indiv_tax_stats) <- names(indiv_tax_stats) %>%
  str_remove_all(pattern = "[0-9]+")

# ##-- ?REMOVE --## Adding personal income tax schedule for the 2018-19 income year
# personal_income_tax <- tibble(
#   income = seq(from = 0, to = 200000, by = 1),
#   pit = case_when(
#     income <= 18000 ~ 0,
#     income <= 37000 ~ ((income - 18201) * 0.1900),
#     income <= 90000 ~ ((income - 37001) * 0.3250) + 3572,
#     income <= 18000 ~ ((income - 90001) * 0.3700) + 20797,
#     TRUE ~ ((income - 180001) * 0.4500) + 54097
#   )
# )

# Calculating __average taxable income__ and __total income or loss__ per cohort
# Adding personal income tax payable with (pit_tft) and without (pit_no_tft) the tax free threshold
indiv_tax_stats <- indiv_tax_stats %>%
  mutate(
    avg_sal_and_wage = salary_or_wages / salary_or_wages_no,
    avg_tot_y_or_l   = total_income_or_loss / total_income_or_loss_no,
    pit_tft = case_when(
      avg_tot_y_or_l <= 18000 ~ 0,
      avg_tot_y_or_l <= 37000 ~ ((avg_tot_y_or_l - 18201) * 0.1900),
      avg_tot_y_or_l <= 90000 ~ ((avg_tot_y_or_l - 37001) * 0.3250) + 3572,
      avg_tot_y_or_l <= 18000 ~ ((avg_tot_y_or_l - 90001) * 0.3700) + 20797,
      TRUE ~ ((avg_tot_y_or_l - 180001) * 0.4500) + 54097),
    pit_no_tft = case_when(
      avg_tot_y_or_l <= 37000 ~ (avg_tot_y_or_l * 0.1900),
      avg_tot_y_or_l <= 90000 ~ ((avg_tot_y_or_l - 37001) * 0.3250) + 3572,
      avg_tot_y_or_l <= 18000 ~ ((avg_tot_y_or_l - 90001) * 0.3700) + 20797,
      TRUE ~ ((avg_tot_y_or_l - 180001) * 0.4500) + 54097),
    age_range = factor(str_remove_all(age_range, pattern = "[a-z][.] "), ordered = TRUE) %>% fct_shift(n = -1L)
    ) %>%
  select(1, 3:6, pit_tft, pit_no_tft, avg_sal_and_wage, avg_tot_y_or_l)

  # # Graph of incomes for taxpayers with "Non Taxable" designation. Interesting to see some of the
  # # distribution lies beyond $20,000; I wonder what's going on there?
  # indiv_tax_stats %>%
  #   filter(taxable_status == "Non Taxable") %>%
  #   ggplot(aes(x = avg_sal_and_wage)) +
  #   geom_histogram()
  #
  # # It turns out that the Baby Boomers are at the tail of that distrubtion
  # # ( >$20,000 threshold of "Non Taxable").
  # indiv_tax_stats %>%
  #   filter(taxable_status == "Non Taxable", avg_sal_and_wage >20000) %>%
  #   distinct(age_range2)

# Creating a summary of gross income tax liability __(prior to deductions and offsets)__
# Also, cleaning the names of the age cohorts by removing prefixes
indiv_summary_YoY <- indiv_tax_stats %>%
  group_by(income_year, sex, age_range, taxable_status) %>%
  summarise(
    pit_tft_mean               = mean(pit_tft, na.rm = TRUE),
    pit_no_tft_mean            = mean(pit_no_tft, na.rm = TRUE)) %>%
  mutate(
    tft_diff_non_taxable_excld = case_when(
      taxable_status == "Taxable" ~ pit_no_tft_mean - pit_tft_mean,
      TRUE                        ~ 0
    ),
    tft_diff_non_taxable_incld =  pit_no_tft_mean - pit_tft_mean
  )


# Graphing the results ##-- I don't think that the addition of
# __tft_diff_non_taxable_excld__ is adding value to the analysis here
p1_data <- indiv_summary_YoY %>%
  select(1:3, 6:7) %>%
  gather("var", "val", -1, -2, -3)

ggplot(p1_data, aes(x = income_year, y = val, fill = var)) +
  geom_bar(stat = 'identity') +
  facet_grid(rows = vars(sex), cols = vars(age_range)) +
  scale_y_continuous(labels = scales::dollar,
                     expand = expand_scale(add = c(0, 500)),
                     breaks = c(5000, 10000, 15000, 20000)) +
  scale_x_discrete() +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90)
  )+
  labs(
    x = "",
    y = ""
  )


# p2 shows the additional tax per person per gender per age-cohort per annum
# if the tax free threshold were to be abolished
p2_data <- indiv_summary_YoY %>%
  group_by(income_year, sex, age_range) %>%
  summarise(difference_bt_tft_and_no_tft = mean(tft_diff_non_taxable_incld)) %>%
  select(1:3, difference_bt_tft_and_no_tft)

# INTERSTING ASIDE ONE: This plot shows the gender distribution of the elimination of the tft
# START HERE - change this from a difference between men and women to show total values N.B. needs to flow through to the next interesting aside (below)
p2_data %>%
  spread(sex, difference_bt_tft_and_no_tft) %>%
  mutate(gender_diff = Male - Female,
         colour1 = case_when(
           gender_diff <= 0 ~ "deeppink3",
           TRUE             ~ "royalblue3"
         )) %>%
  ggplot(aes(x = income_year, y = gender_diff, fill = colour1)) +
    geom_bar(stat = 'identity') +
    facet_grid(cols = vars(age_range)) +
  scale_y_continuous(labels = scales::dollar) +
    theme_light() +
    theme(
    legend.position = 'none',
    axis.text.x = element_text(angle = 90, vjust = 0.4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
    ) +
  labs(
    x = "",
    y = "",
    title = "Gender impact of TFT removal",
    subtitle = "Women benefit much more than men and pink greater claims by women"
    ) +
  scale_fill_manual(values = c("deeppink1", "royalblue3"))

# INTERSTING ASIDE TWO: The presence of the tft reduces Net
p2_data %>%
  spread(sex, difference_bt_tft_and_no_tft) %>%
  mutate(gender_diff = Male - Female,
         colour1 = case_when(
           gender_diff <= 0 ~ "deeppink3",
           TRUE             ~ "royalblue3"
         )) %>%
  select(-Female, -Male, -colour1) %>%
  group_by(income_year) %>%
  summarise(
    life_time_benefit = mean(gender_diff)
  ) %>%
  adorn_totals("row")


ggplot(p2_data, aes(x = income_year, y = difference_bt_tft_and_no_tft)) +
  geom_bar(stat = 'identity') +
  facet_grid(rows = vars(sex), cols = vars(age_range)) +
  scale_y_continuous(labels = scales::dollar,
                     expand = expand_scale(add = c(0, 500)),
                     breaks = seq(from = 500, to = 2000, by = 500),
                     ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.4),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = "",
    y = "",

  )

