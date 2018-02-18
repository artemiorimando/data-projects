library(caret)
library(doParallel)
library(pROC)
library(glmnet)
library(Matrix)

set.seed(20160727)

LC_WOE_Dataset <- WOE_matrix_final

partition <- createDataPartition(LC_WOE_Dataset$Bad_Binary, p = 0.75, list = FALSE)
training <- LC_WOE_Dataset[partition,]
testing <- LC_WOE_Dataset[-partition,]

fitControl <- trainControl(method = "cv",
number = 3,
savePredictions = TRUE,
classProbs = TRUE,
summaryFunction = twoClassSummary)

set.seed(1107)

number_cores <- detectCores()
cluster <- makeCluster(number_cores)
registerDoParallel(cluster)

glmnet.fit <- train(Bad_Binary ~., data = training,
method = "glmnet",
family = "binomial",
metric = "ROC",
trControl = fitControl,
tuneLength = 5)

glmnet.fit
plot(glmnet.fit)

test.glmnet.fit <- predict(glmnet.fit, testing, type = "prob")
auc.condition <- ifelse(testing$Bad_Binary == "Good", 1, 0)
auc.test.glmnet.fit <- roc(auc.condition, test.glmnet.fit[[2]])

plot(auc.test.glmnet.fit, col = "red", grid = TRUE)

final.model <- glmnet.fit$finalModel
coef.final.model <- as.matrix(coef(final.model, glmnet.fit$bestTune$lambda))
