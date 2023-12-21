  library(tidyverse)
  library(haven)
  library(fixest)
  library(glue)
  library(gt)
  library(broom)
  library(dplyr)
  library(stringr)
  
  load("/Users/happyshuyiyi/Library/CloudStorage/OneDrive-YaleUniversity/Fall 2023/AQRD/Final Project/AQRD Final Project/dataverse_files/ppp_contract.RData")
  load("/Users/happyshuyiyi/Library/CloudStorage/OneDrive-YaleUniversity/Fall 2023/AQRD/Final Project/AQRD Final Project/dataverse_files/ppp_panel.RData")
  subway_analysis_use <- read_dta("subway_analysis_use.dta")
  
  #environmental and water conservancy data set filtered
  contract_ew <- contract |>
    select("year", "distCity", starts_with("invest"), starts_with("mayor"), "type",
           "lgdp_pc", "lpopul", "lfiscal") |>
    filter(type == "Environment" | type == "Water Conservancy") |>
    group_by(year, distCity, type) |>
    arrange(year, distCity) |>
    summarise(sum_invest = sum(invest),
              sum_invest1 = sum(invest_l),
              sum_gpd = sum(lgdp_pc),
              mean_pop = mean(lpopul),
              sum_fiscal = sum(lfiscal))
  
  
  #Public Transportation data set filtered
  contract_T <- contract |>
    select("year", "distCity", starts_with("invest"), starts_with("mayor"), "type",
           "lgdp_pc", "lpopul", "lfiscal") |>
    filter(type == "Public Transportation") |>
    group_by(year, distCity, type) |>
    arrange(year, distCity) |>
    summarise(sum_invest = sum(invest),
              sum_invest1 = sum(invest_l),
              sum_gpd = sum(lgdp_pc),
              mean_pop = mean(lpopul),
              sum_fiscal = sum(lfiscal))
  
  #contract data set filtered
  contract_all <- contract |>
    select("year", "distCity", starts_with("invest"), starts_with("mayor"), "type",
           "lgdp_pc", "lpopul", "lfiscal") |>
    group_by(year, distCity, type) |>
    arrange(year, distCity) |>
    summarise(sum_invest = sum(invest),
              sum_invest1 = sum(invest_l),
              sum_gpd = sum(lgdp_pc),
              mean_pop = mean(lpopul),
              sum_fiscal = sum(lfiscal))
  
  
  # merging contract and ppp
  ppp_contract <- contract_all |>
    left_join(y = ppp, by = c("distCity", "year")) |> 
    mutate(City_Code = distCity/100)|>  
    rename(Year = year)
  
  ppp_contract_subway <- ppp_contract |> 
    left_join(y = subway_analysis_use, by = "City_Code","Year")

  # merging contract_T and ppp
  ppp_contract_T <- contract_T |>
    left_join(y = ppp, by = c("distCity", "year")) |> 
    mutate(City_Code = distCity/100)|>  
    rename(Year = year)
  
  ppp_contract_T_subway <- ppp_contract_T |> 
    left_join(y = subway_analysis_use, by = "City_Code","Year")

  #  merging contract_ew and ppp
  ppp_contract_ew <- contract_ew |>
    left_join(y = ppp, by = c("distCity", "year")) |> 
    mutate(City_Code = distCity/100)|>  
    rename(Year = year)
  
  ppp_contract_ew_subway <- ppp_contract_ew |> 
    left_join(y = subway_analysis_use, by = "City_Code","Year")
  
  mayor_cont <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu", "Mayor_c_central_exp",
                  "Mayor_c_prov_exp", "Mayor_c_county_exp", "Mayor_c_soe_exp", "Mayor_c_univ_exp",
                  "Mayor_c_league", "Mayor_connection_work")
  base_cont <- c("lpop_1", "lgdp_1", "lrev_1", "GRP_growth_1")
  
  sub_novice_all <- subset(ppp_contract_subway, fsj2 == 0) 
  sub_novice_T <- subset(ppp_contract_T_subway, fsj2 == 0) 
  sub_novice_ew <- subset(ppp_contract_ew_subway, fsj2 == 0) 
  

  #除了地铁之外的其他公共交通项目并不能给市长的晋升带来提升，我找到了同时期的同样的大型公共交通项目，
  #这些项目在同样的模型的显示下面并不能对与市长晋升有显著帮助
  #地铁是特殊的
  # ppp in OLS 
  control_vars <- c(mayor_cont, base_cont, "City_Code", "Year.x")
  formula_str <- paste("Mayor_promotion3y ~ invest_l +", paste(control_vars, collapse = " + "))
  
  ppp_promotion_FE <- lm(formula_str, data = sub_novice_all)

  T_promotion_FE <- lm(formula_str, data = sub_novice_T)
  
  ew_promotion_FE <- lm(formula_str, data = sub_novice_ew)
  
  # ppp in did
#ppp project
  ppp_promotion <- glue(
    "Mayor_promotion3y ~ invest_l + {str_c(mayor_cont, collapse = ' + ')} + {str_c(base_cont, collapse = ' + ')} | City_Code + Year.x + Year.x:pro_code"
  )
  ppp_promotion <- feols(as.formula(ppp_promotion), 
                        sub_novice_all,
                        cluster = "City_Code")

#Transportation project
  T_promotion <- glue(
    "Mayor_promotion3y ~ invest_l + {str_c(mayor_cont, collapse = ' + ')} + {str_c(base_cont, collapse = ' + ')} | City_Code + Year.x + Year.x:pro_code"
  )
  T_promotion <- feols(as.formula(ppp_promotion), 
                         sub_novice_T,
                         cluster = "City_Code")
  
# environmental and water conservancy project
  ew_promotion <- glue(
    "Mayor_promotion3y ~ invest_l + {str_c(mayor_cont, collapse = ' + ')} + {str_c(base_cont, collapse = ' + ')} | City_Code + Year.x + Year.x:pro_code"
  )
  ew_promotion <- feols(as.formula(ppp_promotion), 
                       sub_novice_ew,
                       cluster = "City_Code")
  
  # Create a list of your models
  model_list <- list("All PPP (DiD)" = ppp_promotion,
                     "Transportation PPP (DiD)" = T_promotion,
                     "Enviromental PPP (DiD)" = ew_promotion,
                     "All PPP (FE)" = ppp_promotion_FE,
                     "Transportation PPP (FE)" = T_promotion_FE,
                     "Enviromental PPP (FE)" = ew_promotion_FE)
  
  summary_row <- tribble(
    ~ term, ~ "All PPP", ~ "Transportation ppp", ~ "Enviromental ppp",~ "All PPP", ~ "Transportation ppp", ~ "Enviromental ppp",
    "City FE", "✓", "✓", "✓","✓", "✓", "✓",
    "Year FE", "✓", "✓", "✓", "✓", "✓", "✓",
    "Province-year FE", "✓", "✓", "✓","", "", "",
    "Mayor Controls", "✓", "✓", "✓","✓", "✓", "✓",
    "City Controls", "✓", "✓", "✓","✓", "✓", "✓")
  
  msummary(
    model_list,
    coef_omit = "Mayor_c_*|lpop_1|lgdp_1|lrev_1|GRP_growth_1|gender2|race6|Mayor_age|City_Code|Year.x|(Intercept)",
    output = "gt",
    add_rows = summary_row,
    #add_rows = data.frame(intercept = c("Mayor controls","City controls"), "(1)" = c("",""), "(2)" = c("X",""), "(3)" = c("X","X"), "(4)" = c("X","X")),
    coef_rename = c("invest_l" = "Project Investment"),
    title = "Table 3. PPP and Mayoral Promotion",
    gof_omit = "R2|R2 Adj.|R2 Within|R2 Within Adj.|AIC|BIC|RMSE|Std.Errors|FE: City_Code|FE: Year|FE: Year:pro_code|Log.Lik.",
    stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
    notes = c("Note: Standard errors clustered at the city level are reported in parentheses. Control variables: (1) mayor controls include gender, ethnicity, age, education level, political connection with provincial party secretary, and previous work experience in county government, provincial government, central government, state-owned enterprises, university, and the Communist Youth League; (2) city controls include population, GDP size, GDP growth rate, and fiscal revenue in the previous year. FE represents fixed effects."),
  ) |> tab_spanner(label = "Mayor Promoted within Three Years", columns = 2:7)
  
  