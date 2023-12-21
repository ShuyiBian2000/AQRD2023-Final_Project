library(tidyverse)
library(fixest)
library(ggplot2)
library(panelView)
library(glue)
library(haven)
library(broom)
library(dplyr)

# Load data
subway_analysis_use <- read_dta("subway_analysis_use.dta")
# Creating leads and lags from paper -----------
subway_lag_leads <- subway_analysis_use |>
  group_by(City_Code) |>
  mutate(
    mpprior1 = (Mayor_plan == 0 & dplyr::lead(Mayor_plan, 1) == 1),
    mpprior2 = (
      Mayor_plan == 0 &
        dplyr::lead(Mayor_plan, 1) == 0 &
        dplyr::lead(Mayor_plan, 2) == 1
    ),
    mpprior3 = (
      Mayor_plan == 0 &
        dplyr::lead(Mayor_plan, 1) == 0 &
        dplyr::lead(Mayor_plan, 2) == 0 &
        dplyr::lead(Mayor_plan, 3) == 1
    ),
    mpprior4 = (
      Mayor_plan == 0 &
        dplyr::lead(Mayor_plan, 1) == 0 &
        dplyr::lead(Mayor_plan, 2) == 0 &
        dplyr::lead(Mayor_plan, 3) == 0 &
        dplyr::lead(Mayor_plan, 4) == 1
    ),
    mpprior5 = (
      Mayor_plan == 0 &
        dplyr::lead(Mayor_plan, 1) == 0 &
        dplyr::lead(Mayor_plan, 2) == 0 &
        dplyr::lead(Mayor_plan, 3) == 0 &
        dplyr::lead(Mayor_plan, 4) == 0 &
        dplyr::lead(Mayor_plan, 5) == 1
    ),
    mpconn1 = (Mayor_plan == 1 & dplyr::lag(Mayor_plan, 1) == 0),
    mpconn2 = (
      Mayor_plan == 1 &
        dplyr::lag(Mayor_plan, 1) == 1 &
        dplyr::lag(Mayor_plan, 2) == 0
    ),
    mpconn3 = (
      Mayor_plan == 1 &
        dplyr::lag(Mayor_plan, 1) == 1 &
        dplyr::lag(Mayor_plan, 2) == 1 &
        dplyr::lag(Mayor_plan, 3) == 0
    ),
    mpconn4 = (
      Mayor_plan == 1 &
        dplyr::lag(Mayor_plan, 1) == 1 &
        dplyr::lag(Mayor_plan, 2) == 1 &
        dplyr::lag(Mayor_plan, 3) == 1 &
        dplyr::lag(Mayor_plan, 4) == 0
    ),
    mpconn5 = (Mayor_plan == 1 &
                 mpconn1 == 0 &
                 mpconn2 == 0 & mpconn3 == 0 & mpconn4 == 0)
    
  )


# getting the dplyr::lag and dplyr::leads column to use in formula
lag_leads <- subway_lag_leads |>
  select(starts_with("mpprior"), starts_with("mpconn")) |>
  colnames()

# Removing the lead of 1 year before the approval and City_code
lag_leads = lag_leads[- which(lag_leads %in% c("City_Code", "mpprior1"))]


# xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 
#mpprior2 mpconn1-mpconn5 i.Year if fsj2 == 0, fe cluster(City_Code)

lag_leads_form <-
  glue(
    "Mayor_promotion3y ~ {str_c(lag_leads, collapse = ' + ')} | 
    City_Code + Year"
  )


fit <- feols(
  as.formula(lag_leads_form),
  data = subway_lag_leads |>
    filter(fsj2 == 0),
  cluster = "City_Code"
)


#plotting
fit |>
  tidy() |>
  mutate(
    term = recode_factor(
      term,
      mpprior5TRUE = -5,
      mpprior4TRUE = -4,
      mpprior3TRUE = -3,
      mpprior2TRUE = -2,
      mpconn1TRUE = 0,
      mpconn2TRUE = 1,
      mpconn3TRUE = 2,
      mpconn4TRUE = 3,
      mpconn5TRUE = 4
    )
  ) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 *
                      std.error),
                width = 0) +
  geom_vline(xintercept = -1, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "Effects of Approval on Mayoral Promotion in 3 years", x = "") +
  theme_classic()
