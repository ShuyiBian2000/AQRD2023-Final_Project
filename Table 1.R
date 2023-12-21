library(haven)
library(stargazer)

##Table 1 Summary Statistics table
# Read the Stata data file
data <- read_dta("subway_analysis_use.dta")


# Define the variables and their labels
variables <- c("Mayor_promotion3y", "Mayor_connection_work", "Mayor_age", 
               "Per_pop", "gdp", "rev", "GRP_growth", "Mayor_plan", 
               "inv1_per", "GRP_per", "land_per", "rev_per")

labels <- c("Mayor promoted within three years", "Mayor connection", "Mayor age", 
            "City population", "City GDP (billion ¥)", "City fiscal revenue (billion ¥)", 
            "City GDP growth rate (%)", "Mayor obtaining subway approval", 
            "City investment in infrastructure per capita (¥)", "City GDP per capita (¥)", 
            "City land sales revenue per capita (¥)", "City fiscal revenue per capita (¥)")

# Create a function for summary statistics
summary_stats <- function(var) {
  data %>%
    summarise(
      N = sum(!is.na(.data[[var]])),
      Mean = mean(.data[[var]], na.rm = TRUE),
      SD = sd(.data[[var]], na.rm = TRUE),
      Min = min(.data[[var]], na.rm = TRUE),
      Max = max(.data[[var]], na.rm = TRUE)
    )
}

# Apply the function to each variable and bind the results
summary_data <- map2_df(variables, labels, ~ cbind(Variable = .y, summary_stats(.x)))

# Create the gt table
gt_table <- summary_data %>%
  gt() %>%
  tab_header(title = "Table 1. Summary Statistics") %>%
  cols_label(Variable = "Variable", N = "N", Mean = "Mean", SD = "SD", Min = "Min", Max = "Max") %>%
  fmt_number(columns = vars(Mean, SD, Min, Max), decimals = 2)

# Print the table
print(gt_table)