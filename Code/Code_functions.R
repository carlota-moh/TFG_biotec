model <- function(exp_train, exp_test) {

# Set the training control parameters

tr_control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'grid')

# Load the parameters for the different models

k <- c(1:40)
k_parameter <- data.frame(k)

mtry <- c(3,4,5)
mtry_parameter <- data.frame(mtry)

nb_parameters <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = 1:5)

svm_parameter <- data.frame(C = seq(0.01, 2, by = 0.01))

nn_parameter <- expand.grid(size = c(1:3), decay = seq(0.01, 0.1, by = 0.01))

# Build the models and use them for predicting

set.seed(1)
knn_1 <- train(Subtype ~ ., data = exp_train, method = 'knn', tuneGrid = k_parameter, trControl = tr_control)

knn_t <- predict(knn_1, exp_test)

table_k <- confusionMatrix(knn_t, exp_test$Subtype)

Acc_k <- table_k$overall[1]
IC_kl <- table_k$overall[3]
IC_ku <- table_k$overall[4]
SS_k <- table_k$byClass[,c("Sensitivity","Specificity")]

set.seed(1)
rf_1 <- train(Subtype ~ ., data = exp_train, method = 'rf', trControl = tr_control, tuneGrid = mtry_parameter)

rf_t <- predict(rf_1, exp_test)

table_rf <- confusionMatrix(rf_t, exp_test$Subtype)

Acc_rf <- table_rf$overall[1]
IC_rfl <- table_rf$overall[3]
IC_rfu <- table_rf$overall[4]
SS_rf <- table_rf$byClass[,c("Sensitivity","Specificity")]

set.seed(1)
nb <- train(Subtype ~ ., data = exp_train, method = 'nb', trControl = tr_control, tuneGrid = nb_parameters)

nb_t <- predict(nb, exp_test)

table_nb <- confusionMatrix(nb_t, exp_test$Subtype)

Acc_nb <- table_nb$overall[1]
IC_nbl <- table_nb$overall[3]
IC_nbu <- table_nb$overall[4]
SS_nb <- table_nb$byClass[,c("Sensitivity","Specificity")]

set.seed(1)
svm <- train(Subtype ~ ., data = exp_train, method = 'svmLinear', trControl = tr_control, tuneGrid = svm_parameter)

svm_t <- predict(svm, exp_test)

table_svm <- confusionMatrix(svm_t, exp_test$Subtype)

Acc_svm <- table_svm$overall[1]
IC_svml <- table_svm$overall[3]
IC_svmu <- table_svm$overall[4]
SS_svm <- table_svm$byClass[,c("Sensitivity","Specificity")]

set.seed(1)
svm_p <- train(Subtype ~ ., data = exp_train, method = 'svmPoly', trControl = tr_control, tuneLength = 4)

svm_pt <- predict(svm_p, exp_test)

table_svmp <- confusionMatrix(svm_pt, exp_test$Subtype)

Acc_svmp <- table_svmp$overall[1]
IC_svmpl <- table_svmp$overall[3]
IC_svmpu <- table_svmp$overall[4]
SS_svmp <- table_svmp$byClass[,c("Sensitivity","Specificity")]

set.seed(1)
svm_r <- train(Subtype ~ ., data = exp_train, method = 'svmRadial', trControl = tr_control, tuneLength = 10)

svm_rt <- predict(svm_r, exp_test)

table_svmr <- confusionMatrix(svm_rt, exp_test$Subtype)

Acc_svmr <- table_svmr$overall[1]
IC_svmrl <- table_svmr$overall[3]
IC_svmru <- table_svmr$overall[4]
SS_svmr <- table_svmr$byClass[,c("Sensitivity","Specificity")]

set.seed(1)
nn_1 <- train(Subtype ~ ., data = exp_train, method = 'nnet', tuneGrid = nn_parameter, lineout = 0, trControl = tr_control)

nn_t <- predict(nn_1, exp_test)

table_nn <- confusionMatrix(nn_t, exp_test$Subtype)

Acc_nn <- table_nn$overall[1]
IC_nnl <- table_nn$overall[3]
IC_nnu <- table_nn$overall[4]
SS_nn <- table_nn$byClass[,c("Sensitivity","Specificity")]

# Create a table to summarize the results

Model_name <- c('knn', 'RandomForest', 'NaiveBayes', 'SVMLin', 'SVMPoly', 'SVMRad', 'NeuralNet')

Accuracies <- c(Acc_k, Acc_rf, Acc_nb, Acc_svm, Acc_svmp, Acc_svmr, Acc_nn)

IC_lower <- c(IC_kl, IC_rfl, IC_nbl, IC_svml, IC_svmpl, IC_svmrl, IC_nnl)

IC_upper <- c(IC_ku, IC_rfu, IC_nbu, IC_svmu, IC_svmpu, IC_svmru, IC_nnu)

Sensitivity_CL <- c(SS_k[1,1], SS_rf[1,1], SS_nb[1,1], SS_svm[1,1], SS_svmp[1,1], SS_svmr[1,1], SS_nn[1,1])

Specificity_CL <- c(SS_k[1,2], SS_rf[1,2], SS_nb[1,2], SS_svm[1,2], SS_svmp[1,2], SS_svmr[1,2], SS_nn[1,2])

Sensitivity_MES <- c(SS_k[2,1], SS_rf[2,1], SS_nb[2,1], SS_svm[2,1], SS_svmp[2,1], SS_svmr[2,1], SS_nn[2,1])

Specificity_MES <- c(SS_k[2,2], SS_rf[2,2], SS_nb[2,2], SS_svm[2,2], SS_svmp[2,2], SS_svmr[2,2], SS_nn[2,2])

Sensitivity_PN <- c(SS_k[3,1], SS_rf[3,1], SS_nb[3,1], SS_svm[3,1], SS_svmp[3,1], SS_svmr[3,1], SS_nn[3,1])

Specificity_PN <- c(SS_k[3,2], SS_rf[3,2], SS_nb[3,2], SS_svm[3,2], SS_svmp[3,2], SS_svmr[3,2], SS_nn[3,2])

table_models <- data.frame(Model_name, Accuracies, IC_lower, IC_upper, Sensitivity_CL, Specificity_CL, Sensitivity_MES, Specificity_MES, Sensitivity_PN, Specificity_PN)

return(table_models)}
