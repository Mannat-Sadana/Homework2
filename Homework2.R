# Homework 2

library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)

sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

sales_clean <- sales_raw %>%
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )
sales_clean %>%
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)
sales_codes <- sales_clean %>%
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )
invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(sale_id, code_raw, code_clean)
invalid_codes
valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  select(sale_id, code_clean) %>%
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number")
  )
valid_code_parts
sale_tags <- sales_clean %>%
  select(sale_id, tags) %>%
  separate_longer_delim(tags, delim = ";")
sale_tags

# separate_wider_delim() splits one string into multiple columns, while separate_longer_delim() splits one string into multiple rows.

customer_summary <- sales_clean %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if (known_n == 0) {
      NA_real_
    } else {
      large_n / known_n
    },
    .by = customer_id
  ) %>%
  arrange(customer_id)
customer_summary

# C1's large_share is 0 / 1 = 0. The numerator is 0 large sales, and the denominator is 1 sale with a known net amount. The other sale has a missing amount.

sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )
levels(sales_factors$item_factor)
sales_factors %>%
  count(item_factor)
levels(sales_factors$item_group)
sales_factors %>%
  count(item_group)

# fct_infreq() changes the order of factor levels based on frequency. fct_lump_n() combines less frequent categories into "Other".

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)
customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)
reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(customer_id, .direction = "down") %>%
  ungroup()
reports_labeled
reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )
reports_clean %>%
  select(
    form, line, customer_id, round,
    primary_amount, backup_amount, amount
  )

# coalesce() returns the first non-missing value. A recorded primary value, including zero, is kept because zero is not missing. The backup is used only if primary_clean is NA.

reports_complete <- reports_clean %>%
  select(customer_id, round, amount) %>%
  mutate(recorded_row = TRUE) %>%
  complete(
    customer_id,
    round = 1:3,
    fill = list(recorded_row = FALSE)
  )
reports_complete
reports_clean %>%
  summarise(
    original_rows = n(),
    added_rows = 0,
    missing_amounts = sum(is.na(amount))
  )
reports_complete %>%
  summarise(
    original_rows = sum(recorded_row),
    added_rows = sum(!recorded_row),
    missing_amounts = sum(is.na(amount))
  )

# Explicit missing amount: C3, round 1, has amount = NA in an existing report. Implicit missing row: C1, round 2, had no report before completion. Check customer metadata IDs

customers %>%
  summarise(
    missing_ids = sum(is.na(customer_id)),
    duplicate_ids = anyDuplicated(customer_id) > 0
  )
reports_clean %>%
  count(customer_id, round) %>%
  filter(n > 1)
customers_marked <- customers %>%
  mutate(customer_found = TRUE)
reports_joined <- reports_clean %>%
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(
    customer_found = coalesce(customer_found, FALSE)
  )
reports_joined %>%
  select(customer_id, round, amount, region, customer_found)

# customer_found distinguishes a missing region from a missing customer record. C3 is found even though its region is NA; C4 is not found in the customer metadata table.

anti_join(
  reports_clean %>% distinct(customer_id),
  customers %>% distinct(customer_id),
  by = join_by(customer_id),
  na_matches = "never"
)
anti_join(
  customers %>% distinct(customer_id),
  reports_clean %>% distinct(customer_id),
  by = join_by(customer_id),
  na_matches = "never"
)
customers_bad <- bind_rows(
  customers,
  tibble(
    customer_id = "C1",
    region = "West"
  )
)
reports_duplicated <- reports_clean %>%
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many"
  )

# Customer C1 has reports in rounds 1 and 3, and each report is duplicated by the join. The row count increases from 6 to 8, and the total amount increases from 75 to 100. The total increases because C1's amounts of 10 and 15 are each counted twice.

reports_clean %>%
  summarise(
    rows_before = n(),
    total_before = sum(amount, na.rm = TRUE)
  )
reports_duplicated %>%
  summarise(
    rows_after = n(),
    total_after = sum(amount, na.rm = TRUE)
  )

# relationship = "many-to-one" would flag an unexpected situation where a report matches multiple customer metadata rows. Allowing many-to-many permits the join but does not resolve conflicting customer information.

reports_wide <- reports_complete %>%
  select(customer_id, round, amount) %>%
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )
reports_wide
reports_wide %>%
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    values_to = "amount",
    values_drop_na = TRUE
  )

# Five rows remain after pivoting back to long form with values_drop_na = TRUE, because five customer-round amounts are recorded and non-missing. Dropping NA removes both unknown amounts and newly completed missing combinations.

two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  nx <- length(x)
  ny <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  if (equal_var == TRUE) {
    sp2 <- ((nx - 1) * var_x + (ny - 1) * var_y) /
      (nx + ny - 2)
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
    df <- nx + ny - 2
    method <- "Pooled"
  } else {
    se <- sqrt(var_x / nx + var_y / ny)
    df <- (var_x / nx + var_y / ny)^2 /
      ((var_x / nx)^2 / (nx - 1) +
       (var_y / ny)^2 / (ny - 1))
    method <- "Welch"
  }
  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(
    abs(t_stat),
    df = df,
    lower.tail = FALSE
  )
  reject <- p_value < alpha
  return(list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  ))
}
x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)
welch_result <- two_sample_t(x_check, y_check)
pooled_result <- two_sample_t(
  x_check, y_check, equal_var = TRUE
)
welch_result
pooled_result
welch_reference <- t.test(
  x_check, y_check, var.equal = FALSE
)
pooled_reference <- t.test(
  x_check, y_check, var.equal = TRUE
)
welch_diff <- c(
  abs(welch_result$t_stat - unname(welch_reference$statistic)),
  abs(welch_result$df - unname(welch_reference$parameter)),
  abs(welch_result$p_value - welch_reference$p.value)
)
pooled_diff <- c(
  abs(pooled_result$t_stat - unname(pooled_reference$statistic)),
  abs(pooled_result$df - unname(pooled_reference$parameter)),
  abs(pooled_result$p_value - pooled_reference$p.value)
)
welch_diff
pooled_diff
all(welch_diff < 1e-8)
all(pooled_diff < 1e-8)
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05
scenarios <- data.frame(
  scenario = c(
    "equal_null", "unequal_null",
    "equal_shift", "unequal_shift"
  ),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)
set.seed(2026)
simulation_results <- data.frame(
  scenario = character(4 * B * 2),
  replicate = integer(4 * B * 2),
  method = character(4 * B * 2),
  t_stat = numeric(4 * B * 2),
  df = numeric(4 * B * 2),
  p_value = numeric(4 * B * 2),
  reject = logical(4 * B * 2)
)
row_index <- 1
for (i in 1:nrow(scenarios)) {
  for (b in 1:B) {
    x <- rnorm(
      n_x,
      mean = scenarios$mean_x[i],
      sd = scenarios$sd_x[i]
    )
    y <- rnorm(
      n_y,
      mean = scenarios$mean_y[i],
      sd = scenarios$sd_y[i]
    )
    result_welch <- two_sample_t(
      x, y, equal_var = FALSE, alpha = alpha
    )
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i], b,
      result_welch$method,
      result_welch$t_stat,
      result_welch$df,
      result_welch$p_value,
      result_welch$reject
    )
    row_index <- row_index + 1
    result_pooled <- two_sample_t(
      x, y, equal_var = TRUE, alpha = alpha
    )
    simulation_results[row_index, ] <- list(
      scenarios$scenario[i], b,
      result_pooled$method,
      result_pooled$t_stat,
      result_pooled$df,
      result_pooled$p_value,
      result_pooled$reject
    )
    row_index <- row_index + 1
  }
}
head(simulation_results, 6)
nrow(simulation_results)
simulation_summary <- simulation_results %>%
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  )
simulation_summary %>%
  mutate(
    across(
      c(mean_t, sd_t, rejection_rate),
      ~ round(.x, 3)
    )
  )
simulation_summary %>%
  select(scenario, method, rejection_rate) %>%
  mutate(rejection_rate = round(rejection_rate, 3))
plot_data <- simulation_summary
barplot(
  height = plot_data$rejection_rate,
  names.arg = paste(plot_data$scenario, plot_data$method, sep = "\n"),
  las = 2,
  ylab = "Rejection rate",
  main = "Rejection Rates by Scenario and Test",
  ylim = c(0, 1),
  cex.names = 0.7
)
abline(h = alpha, lty = 2)

# Under the equal_null scenario, the rejection rates for Welch's and the pooled test were 0.044 and 0.038, respectively, both close to the target of 0.05. Under unequal_null, the rates were 0.061 for Welch's test and 0.139 for the pooled test, showing that the pooled test had an inflated Type I error rate when the population variances differed. Under equal_shift, the rejection rates were 0.956 for Welch's test and 0.957 for the pooled test, while under unequal_shift they were 0.282 and 0.497, respectively, indicating that power depended on the variance conditions. Higher power alone is not sufficient to judge a test because excessive Type I error means rejecting a true null hypothesis too often.