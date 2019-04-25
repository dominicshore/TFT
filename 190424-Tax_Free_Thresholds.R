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
    age_range           = factor(str_remove_all(age_range, pattern = "[a-z][.] "), ordered = TRUE) %>% fct_shift(n = -1L),
    pit_tft_diff        = pit_no_tft - pit_tft,
    total_pit_tft       = pit_tft * salary_or_wages_no,
    total_pit_no_tft    = pit_no_tft * salary_or_wages_no,
    total_pit_tft_diff  = pit_tft_diff * salary_or_wages_no
    ) %>%
  select(1, 3:6, avg_sal_and_wage, pit_tft, pit_no_tft, pit_tft_diff, salary_or_wages, total_pit_tft, total_pit_no_tft, total_pit_tft_diff)



#----------------------------------------------------------------
#    What does the distribution of 'Non Taxable' look like?     -
#    Why are there taxpayers with >$18,200 income in the histogram?    -
#     Turns out its predominately the superannuation system     -
#----------------------------------------------------------------
# Graph of all incomes in the 2B sample file. The income outliers all show their 'state_territory' variable as 'Overseas'...
p0_total_dist <- indiv_tax_stats %>%
  ggplot(aes(x = avg_sal_and_wage)) +
  geom_histogram(binwidth = 1000, fill = "lightblue") +
  geom_vline(xintercept = c(18200), colour = c("red"), linetype = "dashed") +
  theme_light() +
  scale_x_continuous(labels = scales::dollar, expand = expand_scale(add = c(0, 0)), breaks = c(18200, 50000, 100000)) +
  scale_y_continuous(expand = expand_scale(add = c(0, 20))) +
  labs(
    x = "",
    y = "",
    title = "Distribution of income in Tax Stats sample file",
    subtitle = "Over the 2010-11 to 2016-17 income years",
    caption = "Source: Tax Stats (2017) - Individuals table 2B"
  ) +
  theme(
    panel.grid = element_blank()
  )

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
#---------------------------------------------------------------------------------------------------------------------------
indiv_summary_YoY <- indiv_tax_stats %>%
  group_by(income_year, sex, age_range, taxable_status) %>%
  summarise(
    pit_tft_mean      = mean(pit_tft, na.rm = TRUE),
    pit_no_tft_mean   = mean(pit_no_tft, na.rm = TRUE),
    pit_tft_diff_mean = mean(pit_tft_diff, na.rm = TRUE))


# Graphing the results ##-- I don't think that the addition of
# __tft_diff_non_taxable_excld__ is adding value to the analysis here
p1_data <- indiv_summary_YoY %>%
  select(1:3, 5:7) %>%
  gather("var", "val", -1, -2, -3)

p1 <- ggplot(p1_data, aes(x = income_year, y = val, fill = var)) +
  geom_bar(stat = 'identity') +
  facet_grid(rows = vars(var), cols = vars(age_range)) +
  scale_y_continuous(labels = scales::dollar,
                     expand = expand_scale(add = c(0, 500))) +
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
  summarise(difference_bt_tft_and_no_tft = mean(pit_tft_diff_mean))

# INTERSTING ASIDE ONE: Males benefit more than females from the presence of the tft. On average, over the period 2010-11 to 2016-17,
# when the additional gross income from the elimination of the tft is assessed by sex, males would have paid
# $583 more tax _than women_.
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
p2 <- p2_data %>%
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
    subtitle = "On average, men would have paid an $583 more than women over the 2010-11 - 2016-17 period without the TFT",
    caption = "Source: Tax Stats (2017) - Individuals table 2B"
  ) +
  scale_fill_manual(values = c("deeppink1", "royalblue3"))

# INTERSTING ASIDE ONE: This plot shows the _net_ gender distribution of the elimination of the tft
p3 <- p2_data %>%
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
    title = "Tax paid in excess of the other sex after TFT removal",
    subtitle = "Additional tax paid by men (blue) or women (pink) without the TFT"
    ) +
  scale_fill_manual(values = c("deeppink1", "royalblue3"))


##################################################################
##          How much income would need to be returned?          ##
##                 Low-income variously defined                 ##
##################################################################

p4 <- indiv_tax_stats %>%
  group_by(taxable_status) %>%
  summarise(
    "Total Additional Tax" = sum(total_pit_tft_diff, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = taxable_status, y = `Total Additional Tax`)) +
  geom_col() +
  scale_y_continuous(labels = scales::dollar)
