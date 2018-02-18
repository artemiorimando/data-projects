library(dplyr)
library(ggplot2)
library(scales)

rejected_data <- read.csv("C:/Users/artemior/Desktop/Lending Club Model/RejectStatsD.csv")

Bad_Binary_Original <- features_36$Bad
sample_inference_features <- WOE_matrix_final[c("zip_code", "addr_state", "emp_length")]
sample_inference_features["Bad_Binary"] <- Bad_Binary_Original

features_36_inference % select(-Amount.Requested, -Application.Date,
-Loan.Title, -Risk_Score,
-Debt.To.Income.Ratio, -Policy.Code)
 
features_36_inference_names <- colnames(features_36_inference)

require(parallel)

number_cores <- detectCores() – 1
cluster <- makeCluster(number_cores)
clusterExport(cluster, c("IV", "min_function", "max_function",
"features_36_inference",
"features_36_inference_names",
"only_features_36", "recode", "WOE_tables"))

WOE_matrix_table_inference <- parSapply(cluster, as.matrix(features_36_inference_names),
FUN = WOE_tables_function)

WOE_matrix_inference <- parSapply(cluster, features_36_inference_names,
FUN = create_WOE_matrix)

rejected_inference_prob <- predict(sample_inference_model,
data = WOE_matrix_inference,
type = "response")
rejected_inference_prob_matrix <- as.matrix(rejected_inference_prob)
rejected_inference_prob_dataframe <- as.data.frame(rejected_inference_prob_matrix)
colnames(rejected_inference_prob_dataframe) <- c("Probabilities")
 
stopCluster(cluster)

number_cores <- detectCores()
cluster <- makeCluster(number_cores)
registerDoParallel(cluster)
 
accepted_prob <- predict(glmnet.fit, LC_WOE_Dataset, type = "prob")
accepted_prob_matrix <- as.matrix(accepted_prob[,2])
accepted_prob_dataframe <- as.data.frame(accepted_prob_matrix)
colnames(accepted_prob_dataframe) <- c("Probabilities")

probability_matrix <- rbind(accepted_prob_matrix, rejected_inference_prob_matrix)
probability_matrix <- as.data.frame(probability_matrix)
colnames(probability_matrix) <- c("Probabilities")

probability_distribution <- ggplot(data = probability_matrix, aes(Probabilities))
probability_distribution <- probability_distribution + geom_histogram(bins = 50)
 
accepted_probability_distribution <- ggplot(data = accepted_prob_dataframe, aes(Probabilities))
accepted_probability_distribution <- accepted_probability_distribution + geom_histogram(bins = 50)

rejected_probability_distribution <- ggplot(data = rejected_inference_prob_dataframe, aes(Probabilities))
rejected_probability_distribution <- rejected_probability_distribution + geom_histogram(bins = 50)

bins = 25
Bad_Binary_Values <- features_36$Bad
prob_bad_matrix <- as.data.frame(cbind(accepted_prob_matrix, Bad_Binary_Values))
colnames(prob_bad_matrix) <- c("Probabilities", "Bad_Binary_Values")

Probabilities <- prob_bad_matrix[,1]
Bad_Binary_Values <- prob_bad_matrix[,2]
order_accepted_prob <- prob_bad_matrix[order(Probabilities, Bad_Binary_Values, decreasing = FALSE),]

bin_prob <- cut(order_accepted_prob$Probabilities, breaks = bins, labels = 1:bins)
order_bin <- as.data.frame(cbind(bin_prob, order_accepted_prob[,2]))
colnames(order_bin) <- c("Bin", "Bad")

bin_table <- table(order_bin$Bin, order_bin$Bad)
 
Bin_Summary <- group_by(order_bin, Bin)
 
Bad_Summary <- summarize(Bin_Summary, Total = n(), Good = sum(Bad), Bad = 1 - Good/Total)

lift_plot <- ggplot(Bad_Summary, aes(x = Bin, y = Bad))
lift_plot <- lift_plot + geom_bar(stat = "identity", colour = "skyblue", fill = "skyblue")
lift_plot <- lift_plot + xlab("Bin")
lift_plot <- lift_plot + ylab("Proportion of Bad")

Accepted_Probabilities <- Probabilities
LogOdds <- log(Accepted_Probabilities)

max_score <- 1000
min_score <- 100
max_LogOdds <- max(LogOdds)
min_LogOdds <- min(LogOdds)
 
linear_slope <- (max_score - min_score)/(max_LogOdds - min_LogOdds)
linear_intercept <- max_score - linear_slope * max_LogOdds
