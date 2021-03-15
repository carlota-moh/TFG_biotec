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

# Use DoSNOW for working in parallel

# cl <- makeCluster(5, type = 'SOCK')

# registerDoSNOW(cl)

set.seed(1)
svm_r <- train(Subtype ~ ., data = exp_train, method = 'svmRadial', trControl = tr_control, tuneLength = 10) 

plot(svm_r)

# stopCluster(cl)

svm_r

svm_rt <- predict(svm_r, exp_test)
table_svmr <- confusionMatrix(svm_rt, exp_test$Subtype)
table_svmr
