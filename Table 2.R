library(tidyverse)
library(haven)
library(fixest)
library(glue)
library(gt)
library(broom)
library(dplyr)
library(stringr)

# Load data
subway_analysis_use <- read_dta("subway_analysis_use.dta")

# Filter dataset for fsj2 == 0
sub_novice <- subset(subway_analysis_use, fsj2 == 0) 

# Define control variables
mayor_cont <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu", "Mayor_c_central_exp",
                "Mayor_c_prov_exp", "Mayor_c_county_exp", "Mayor_c_soe_exp", "Mayor_c_univ_exp",
                "Mayor_c_league", "Mayor_connection_work")
mayor_cont2 <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu",
                 "Mayor_c_central_exp", "Mayor_c_prov_exp", "Mayor_c_county_exp",
                 "Mayor_c_soe_exp", "Mayor_c_univ_exp", "Mayor_c_league")
base_cont <- c("lpop_1", "lgdp_1", "lrev_1", "GRP_growth_1")
PS_cont <- c("PS_age", "PS_gender2", "PS_race8", "PS_connection_work",
             "PS_c_2currentsec2", "PS_c_prov_exp", "PS_c_central_exp", "PS_c_edu",
             "PS_c_soe_exp", "PS_c_univ_exp", "PS_c_league") 

# Estimating the models
# Column One
column_one <- feols(
  Mayor_promotion3y ~ Mayor_plan  | City_Code + Year,
  data = sub_novice,
  cluster = "City_Code"
)

# Column Two
coltwo_form <- glue(
  "Mayor_promotion3y ~ Mayor_plan + {str_c(mayor_cont, collapse = ' + ')} | City_Code + Year"
)
column_two <- feols(as.formula(coltwo_form), 
                    sub_novice,
                    cluster = "City_Code")

# Column Three
colthree_form <- glue(
  "Mayor_promotion3y ~ Mayor_plan + {str_c(mayor_cont, collapse = ' + ')} + {str_c(base_cont, collapse = ' + ')} | City_Code + Year"
)
column_three <- feols(as.formula(colthree_form), 
                      sub_novice,
                      cluster = "City_Code")

# Column Four
colfour_form <- glue(
  "Mayor_promotion3y ~ Mayor_plan + {str_c(mayor_cont, collapse = ' + ')} + {str_c(base_cont, collapse = ' + ')} | City_Code + Year + Year:pro_code"
)
column_four <- feols(as.formula(colfour_form), 
                     sub_novice,
                     cluster = "City_Code")



# Create a list of your models
model_list <- list("Model 1" = column_one,
                   "Model 2" = column_two,
                   "Model 3" = column_three,
                   "Model 4" = column_four)

summary_row <- tribble(
  ~ term, ~ "Model 1", ~ "Model 2", ~ "Model 3", ~ "Model 4",
  "City FE", "✓", "✓", "✓", "✓",
  "Year FE", "✓", "✓", "✓", "✓",
  "Mayor Controls", "", "✓", "✓", "✓",
  "City Controls", "", "", "✓", "✓",
  "Province-year FE", "", "", "", "✓")

msummary(
  model_list,
  coef_omit = "Mayor_c_*|lpop_1|lgdp_1|lrev_1|GRP_growth_1|gender2|race6|Mayor_age",
  output = "gt",
  add_rows = summary_row,
  #add_rows = data.frame(intercept = c("Mayor controls","City controls"), "(1)" = c("",""), "(2)" = c("X",""), "(3)" = c("X","X"), "(4)" = c("X","X")),
  coef_rename = c("Mayor_plan" = "Subway approval"),
  title = "Table 2. Subway Approval and Mayoral Promotion",
  gof_omit = "R2|R2 Adj.|R2 Within|R2 Within Adj.|AIC|BIC|RMSE|Std.Errors|FE: City_Code|FE: Year|FE: Year:pro_code",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  notes = c("Note: Standard errors clustered at the city level are reported in parentheses. Control variables: (1) mayor controls include gender, ethnicity, age, education level, political connection with provincial party secretary, and previous work experience in county government, provincial government, central government, state-owned enterprises, university, and the Communist Youth League; (2) city controls include population, GDP size, GDP growth rate, and fiscal revenue in the previous year. FE = fixed effects."),
) |> tab_spanner(label = "Mayor Promoted within Three Years", columns = 2:5)

