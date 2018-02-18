library(dplyr)
library(parallel)
library(Information)

data <- read.csv("C:/Users/artemior/Desktop/Lending Club model/LoanStats3d.csv")

data <- mutate(data,
Keep = ifelse(loan_status == "Charged Off" |
loan_status == "Default" |
loan_status == "Fully Paid" |
loan_status == "Late (16-30 days)" |
loan_status == "Late (31-120 days)",
"Keep", "Remove"))

sample <- filter(data, Keep == "Keep")

sample_36 <- filter(sample, term == " 36 months")
sample_60 <- filter(sample, term == " 60 months")

sample_36 <- mutate(sample_36, Bad = ifelse(loan_status == "Fully Paid", 1, 0))

features_36 % select(-id, -member_id, -loan_amnt,
-funded_amnt, -funded_amnt_inv, -term, -int_rate, -installment,
-grade, -sub_grade, -pymnt_plan, -purpose, -loan_status,
-emp_title, -out_prncp, -out_prncp_inv, -total_pymnt, -total_pymnt_inv,
-total_rec_int, -total_rec_late_fee, -recoveries, -last_pymnt_d, -last_pymnt_amnt,
-next_pymnt_d, -policy_code, -total_rec_prncp, -Keep)

IV <- create_infotables(data = features_36, y = "Bad")

IV$summary

IV$Tables$last_credit_pull_d

features_36_names_plot <- colnames(features_36)[c(-7, -11, -ncol(features_36))]

plotWOE <- function(x) {
p <- plot_infotables(IV, variable = x, show_values = TRUE)
return(p) }

feature_name_vector_length_plot <- length(features_36_names_plot)

for (i in 1:feature_name_vector_length_plot) {
p <- tryCatch(plotWOE(features_36_names_plot[i]),
error = function(e)
{print(paste("Removed variable: ",
features_36_names_plot[i])); NaN})
print(p) }
