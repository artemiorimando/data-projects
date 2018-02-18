library(parallel)

min_function <- function(x) {
remove_brackets <- gsub("\\[|\\]", "", x = x)
take_min <- gsub(",.*", "", remove_brackets)
min_value <- as.numeric(take_min)
return(min_value)
}
 
max_function <- function(x) {
remove_brackets <- gsub("\\[|\\]", "", x = x)
take_max <- gsub(".*,", "", remove_brackets)
max_value <- as.numeric(take_max)
return(max_value)
}

features_36_names_WOE <- colnames(features_36)[-ncol(features_36)]
features_36_names_WOE_vector_length <- length(features_36_names_WOE)
only_features_36 <- features_36[-ncol(features_36)]
 
WOE_tables_function <- function(x) {
table_text <- sprintf("IV$Tables$%s", x)
create_table <- eval(parse(text = table_text))
MIN <- sapply(create_table, min_function, USE.NAMES = FALSE)[,1]
MAX <- sapply(create_table, max_function, USE.NAMES = FALSE)[,1]
MIN_equal_NA <- is.na(MIN)
count_MIN_equal_NA <- length(MIN[MIN_equal_NA])
 
if (count_MIN_equal_NA == 1) {
MIN[is.na(MIN)] <- -1
MAX[is.na(MAX)] <- -1
WOE <- create_table$WOE
table  1) {
WOE <- create_table$WOE
categories <- create_table[,1]
table <- cbind.data.frame(categories, WOE)
return(table)
 
} else {
WOE <- create_table$WOE
table <- cbind(MIN, MAX, WOE)
return(table)
}
}

number_cores <- detectCores() - 1

cluster <- makeCluster(number_cores)

clusterExport(cluster, c("IV", "min_function", "max_function"))

WOE_tables <- parSapply(cluster, as.matrix(features_36_names_WOE), FUN = WOE_tables_function)

recode <- function(x, y) {
r_WOE_table_text <- sprintf("WOE_tables$%s", y)
create_r_WOE_table <- eval(parse(text = r_WOE_table_text))
data_type_indicator <- create_r_WOE_table[1,1]
 
if (is.factor(data_type_indicator)) {
category_Table <- as.numeric(create_r_WOE_table[,1])
corresponding_WOE_Table <- as.character(create_r_WOE_table[,2])
category_Table_length <- length(category_Table)
raw_variable <- as.numeric(factor(x))
 
for (i in 1:category_Table_length) {
condition_1 <- raw_variable == category_Table[i]
raw_variable[condition_1] <- corresponding_WOE_Table[i]
}
 
return(as.numeric(raw_variable))
 
} else if (data_type_indicator == -1) {
min_r_Table <- create_r_WOE_table[,1]
max_r_Table <- create_r_WOE_table[,2]
corresponding_WOE_Table <- as.character(create_r_WOE_table[,3])
min_r_Table_length <- length(min_r_Table)
raw_variable <- x
 
for (i in 2:min_r_Table_length) {
condition_1 = min_r_Table[i]
condition_2 <- raw_variable <= max_r_Table[i]
raw_variable[condition_1 & condition_2] <- as.numeric(corresponding_WOE_Table[i])
}
 
condition_3 <- is.na(raw_variable)
raw_variable[condition_3] <- corresponding_WOE_Table[1]
 
return(as.numeric(raw_variable))
 
} else {
min_r_Table <- create_r_WOE_table[,1]
max_r_Table <- create_r_WOE_table[,2]
corresponding_WOE_Table <- create_r_WOE_table[,3]
min_r_Table_length <- length(min_r_Table)
raw_variable <- x
 
for (i in 1:min_r_Table_length) {
condition_1 = min_r_Table[i]
condition_2 <- raw_variable <= max_r_Table[i]
raw_variable[condition_1 & condition_2] <- corresponding_WOE_Table[i]
}
 
return(as.numeric(raw_variable))
 
}
}

create_WOE_matrix <- function(x) {
variable_text <- sprintf("only_features_36$%s", x)
create_variable <- eval(parse(text = variable_text))
variable <- create_variable
variable_name <- x
WOE_vector <- recode(variable, variable_name)
return(WOE_vector)
}

clusterExport(cluster, c("only_features_36", "create_WOE_matrix", "recode", "WOE_tables"))
 
WOE_matrix <- parSapply(cluster, features_36_names_WOE, FUN = create_WOE_matrix)
WOE_matrix <- as.data.frame(WOE_matrix)
Bad_Binary <- features_36$Bad
Bad_Condition_1 <- Bad_Binary == 1
Bad_Condition_0 <- Bad_Binary == 0
Bad_Binary[Bad_Condition_1] <- "Good"
Bad_Binary[Bad_Condition_0] <- "Bad"
Bad_Binary <- as.factor(Bad_Binary)
WOE_matrix["Bad_Binary"] <- Bad_Binary
WOE_matrix_final <- WOE_matrix
 
stopCluster(cluster)
