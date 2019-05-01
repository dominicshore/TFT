###########################################################################
###########################################################################
###                                                                     ###
###                THE TAX FREE THRESHOLD: PROGRESSIVITY                ###
###                                                                     ###
###########################################################################
###########################################################################
source("./190424-Tax_Free_Thresholds.R")
library(ggrepel)

##--------------------------------------------------------------
##  How does the presence of the TFT affect Average Tax Rates?
##--------------------------------------------------------------
indiv_tax_stats %>%
  group_by(bracket) %>%
  summarise(
    avg_tax_paid_with_tft = mean(pit_tft, na.rm = T),
    avg_tax_paid_without_tft = mean(pit_no_tft, na.rm = T)
  )

avg_rates_data <- tibble(
  income = seq(from = 1, to = 200000, by = 100),
  pit_with_tft = case_when(
    income <= 18200  ~ 0,
    income <= 37000  ~ ((income - 18201) * 0.1900),
    income <= 90000  ~ ((income - 37001) * 0.3250) + 3572,
    income <= 180000 ~ ((income - 90001) * 0.3700) + 20797,
    TRUE ~ ((income - 180001) * 0.4500) + 54097),
  pit_without_tft = case_when(
    income <= 37000 ~ (income * 0.1900),
    income <= 90000 ~ ((income - 37001) * 0.3250) + 7030,
    income <= 180000 ~ ((income - 90001) * 0.3700) + 24255,
    TRUE ~ ((income - 180001) * 0.4500) + 57555),
  ATR_with_tft    = pit_with_tft / income,
  ATR_without_tft = pit_without_tft / income
)

avg_rates_data %>%
  select(
    income,
    ATR_with_tft,
    ATR_without_tft
  ) %>%
  gather(key = ATR, value = value, - income) %>%
  mutate(ATR_label = case_when(income == 100001 & ATR == "ATR_with_tft" ~ "with TFT",
                               income == 125001 & ATR == "ATR_without_tft" ~ "without TFT",
                               TRUE ~ NA_character_)
         ) %>%
  ggplot(aes(x = income, y = value, colour = ATR)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(size = 1.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expand_scale(c(0, 0.1))) +
  scale_x_continuous(labels = scales::dollar, expand = expand_scale(c(0, 0))) +
  theme_light() +
  labs(
    x = "",
    y = "",
    title = "Average Tax Rates",
    caption = "Source: Tax Framework Division"
  ) +
  theme(
  legend.position = 'none'
  ) +
  geom_label_repel(aes(label = ATR_label), point.padding = 1, force = 30, segment.size = 0)


scale_colour_manual(
    name   = "",
    values = c("ATR_with_tft" = "blue", "ATR_without_tft" = "red"),
    breaks = c("ATR_with_tft", "ATR_without_tft"),
    labels = c("with TFT", "without TFT")
                      )
