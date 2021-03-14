# First load the pre-processed data:

source('Code/Common_1.R')

# Load caret & doSNOW

library(caret)
# library(doSNOW)

# Partition the data into training and testing cohort

set.seed(1)
train_idx <- createDataPartition(exp$Subtype, p = 0.8, list = FALSE)

exp_train <- exp[train_idx, ]
exp_test <- exp[-train_idx, ]

# Check that the proportions are kept

prop.table(table(exp$Subtype))
prop.table(table(exp_train$Subtype))
prop.table(table(exp_test$Subtype))

# Set the training control parameters

tr_control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'grid')

mtry <- c(3,4,5) # Number of randomly sampled variables presented to the tree for selection. Literature advices for the use of mtry = sqrt(ncol(exp)-1). Default setting for the alg uses mtr = (ncol(exp)-1)/3. Added an intermediate value to evaluate performance.
mtry_parameter <- data.frame(mtry)

# Use DoSNOW for working in parallel

# cl <- makeCluster(5, type = 'SOCK')

# registerDoSNOW(cl)

set.seed(1)
rf_1 <- train(Subtype ~ ., data = exp_train, method = 'rf', trControl = tr_control, tuneGrid = mtry_parameter)

# stopCluster(cl)

rf_1

rf_t <- predict(rf_1, exp_test)

table_rf <- confusionMatrix(rf_t, exp_test$Subtype)
table_rf
