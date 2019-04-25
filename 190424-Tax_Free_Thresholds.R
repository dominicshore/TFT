library("tidyverse") # requires dplyr ver 0.8.0.1
library("readxl")    # requires readxl ver 1.3.1
library("janitor")   # requires janitor ver 1.2.0
library("bannerCommenter") # not required

#-----------------------------------------------------------------------------
#  Local path to Excel file containing 2017 Tax Stats individuals table 2B   -
#      Import annual (2010-11 to 2016-17) file with janitor name repair      -
#             Remove residual footnote numbers in variable names             -
#-----------------------------------------------------------------------------

file_name <- "./ts17individual02lodgmentmethodsextaxablestatusstateageyear.xlsx"

indiv_tax_stats_raw <- read_excel(path = file_name, sheet = "Individuals Table 2B", skip = 2, .name_repair = make_clean_names)

names(indiv_tax_stats_raw) <- names(indiv_tax_stats_raw) %>%
  str_remove_all(pattern = "[0-9]+")

#-----------------------------------------------------------------------------------------------------
#              Calculate _average taxable income_ and _total income or loss_ per cohort              -
#   Add personal income tax payable with (pit_tft) and without (pit_no_tft) the tax free threshold   -
#                           Remove letters preceding age cohort descriptors                          -
#-----------------------------------------------------------------------------------------------------

indiv_tax_stats <- indiv_tax_stats_raw %>%
  mutate(
    avg_sal_and_wage = salary_or_wages / salary_or_wages_no,
    #adding variables showing with and without tft
    pit_tft = case_when(
      avg_sal_and_wage <= 18000 ~ 0,
      avg_sal_and_wage <= 37000 ~ ((avg_sal_and_wage - 18201) * 0.1900),
      avg_sal_and_wage <= 90000 ~ ((avg_sal_and_wage - 37001) * 0.3250) + 3572,
      avg_sal_and_wage <= 180000 ~ ((avg_sal_and_wage - 90001) * 0.3700) + 20797,
      TRUE ~ ((avg_sal_and_wage - 180001) * 0.4500) + 54097),
    pit_no_tft = case_when(
      avg_sal_and_wage <= 37000 ~ (avg_sal_and_wage * 0.1900),
      avg_sal_and_wage <= 90000 ~ ((avg_sal_and_wage - 37001) * 0.3250) + 7030,
      avg_sal_and_wage <= 180000 ~ ((avg_sal_and_wage - 90001) * 0.3700) + 24255,
      TRUE ~ ((avg_sal_and_wage - 180001) * 0.4500) + 57555),
    age_range = factor(str_remove_all(age_range, pattern = "[a-z][.] "), ordered = TRUE) %>% fct_shift(n = -1L),
    pit_tft_diff = pit_no_tft - pit_tft
    ) %>%
  select(1, 3:6, avg_sal_and_wage, pit_tft, pit_no_tft, pit_tft_diff)

#----------------------------------------------------------------
#    What does the distribution of 'Non Taxable' look like?     -
#    Why are there taxpayers with >$18,200 income in the histogram?    -
#     Turns out its predominately the superannuation system     -
#----------------------------------------------------------------
# Graph of incomes for taxpayers with "Non Taxable" designation. Interesting to see some of the
# distribution lies beyond $20,000; I wonder what's going on there?
p0_non_taxable_dist <- indiv_tax_stats %>%
  filter(taxable_status == "Non Taxable") %>%
  ggplot(aes(x = avg_sal_and_wage)) +
  geom_histogram(bins = 30, fill = "lightblue") +
  geom_vline(xintercept = c(indiv_tax_stats %>%
                              filter(taxable_status == "Non Taxable") %>%
                              summarise(mean(avg_sal_and_wage, na.rm = T)) %>%
                              pluck(1), 18200), colour = c("black", "red"), linetype = "dashed") +
  theme_light() +
  scale_x_continuous(labels = scales::dollar, expand = expand_scale(add = c(0, 0)), breaks = c(5000, indiv_tax_stats %>%
                                                                                                 filter(taxable_status == "Non Taxable") %>%
                                                                                                 summarise(mean(avg_sal_and_wage, na.rm = T)) %>%
                                                                                                 pluck(1), 18200)) +
  scale_y_continuous(expand = expand_scale(add = c(0, 20))) +
  labs(
    x = "",
    y = "",
    title = "Average income of people who are 'Non Taxable'",
    subtitle = "Over the 2010-11 to 2016-17 income years the average was roughly $10,500 with some superannuants exceeding the TFT",
    caption = "Source: Tax Stats (2017) - Individuals table 2B"
  ) +
  theme(
    panel.grid = element_blank()
  )

# It turns out that a small number of Baby Boomers are at the tail of that distrubtion
# ( >$18,200 threshold of "Non Taxable").
indiv_tax_stats %>%
  filter(taxable_status == "Non Taxable", avg_sal_and_wage >18200) %>%
  group_by(state_territory, age_range) %>%
  tally()

#---------------------------------------------------------------------------------------------------------------------------
#                    Create a summary of gross income tax liability _(prior to deductions and offsets)_                    -
#                                  Plot 1 (p1) shows the [NEED TO SEE IF PIT_NO_TFT_MEAN                                   -
#   IS THE ADDITIONAL TAX PAYABLE IN THE ABSENCE OF A TFT OR THE TOTAL AVERAGE TAX (WHICH INCLUDES WITH ABSENCE OF A TFT]  -
#---------------------------------------------------------------------------------------------------------------------------
indiv_summary_YoY <- indiv_tax_stats %>%
  group_by(income_year, sex, age_range, taxable_status) %>%
  summarise(
    pit_tft_mean      = mean(pit_tft, na.rm = TRUE),
    pit_no_tft_mean   = mean(pit_no_tft, na.rm = TRUE),
    pit_tft_diff_mean = mean(pit_tft_diff, na.rm = TRUE))
  # mutate(
  #   tft_diff_non_taxable_excld = case_when(
  #     taxable_status == "Taxable" ~ pit_no_tft_mean - pit_tft_mean,
  #     TRUE                        ~ 0
  #   ),
  #   tft_diff_non_taxable_incld =  pit_no_tft_mean - pit_tft_mean
  # )


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


#---------------------------------------------------------------------------------------------------------------------------------
#  Plot 2 (p2) shows the _additional_ gross income that would be included in the income tax calculation if the tft were removed  -
#---------------------------------------------------------------------------------------------------------------------------------
p2_data <- indiv_summary_YoY %>%
  group_by(income_year, sex, age_range) %>%
  summarise(difference_bt_tft_and_no_tft = mean(tft_diff_non_taxable_incld))

# INTERSTING ASIDE ONE: Females benefit more than males from the presence of the tft. On average, over the period 2010-11 to 2016-17,
# when the additional gross income from the elimination of the tft is assessed by sex, females would have
# $511.30 _more income than men_ (this is a relative proposition) included in their income tax calculation.
p2_data %>%
  spread(sex, difference_bt_tft_and_no_tft) %>%
  mutate(gender_diff = Male - Female) %>%
  select(-Female, -Male) %>%
  group_by(income_year) %>%
  summarise(
    life_time_benefit = mean(gender_diff)
  ) %>%
  adorn_totals("row") %>%
  as_tibble()

#
p2_data %>%
  mutate(
    difference_bt_tft_and_no_tft = case_when(sex == "Female" ~ difference_bt_tft_and_no_tft * -1L,
                                             TRUE            ~ difference_bt_tft_and_no_tft)
  ) %>%
  ggplot(aes(x = income_year, y = difference_bt_tft_and_no_tft)) +
  geom_bar(stat = 'identity', aes(fill = sex, alpha = 0.2)) +
  geom_point(data = p2_data %>%
               spread(sex, difference_bt_tft_and_no_tft) %>%
               mutate(gender_diff = Male - Female), aes(y = gender_diff), shape = 95, stroke = 3) +
  facet_grid(cols = vars(age_range)) +
  scale_y_continuous(labels = c("$2,000", "$1,000", "$0", "$1,000", "$2,000")) +
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
    title = "Gender impact of removing the $18,200 tax free threshold",
    subtitle = "On average, women would have brought an average of $511.30 more gross income to tax than men",
    caption = "Source: Tax Stats (2017) - Individuals table 2B"
  ) +
  scale_fill_manual(values = c("deeppink1", "royalblue3"))

# INTERSTING ASIDE ONE: This plot shows the gender distribution of the elimination of the tft
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
