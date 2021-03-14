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
svm_p <- train(Subtype ~ ., data = exp_train, method = 'svmPoly', trControl = tr_control, tuneLength = 4) 

plot(svm_p)

# stopCluster(cl)

svm_p

svm_pt <- predict(svm_p, exp_test)
table_svmp <- confusionMatrix(svm_pt, exp_test$Subtype)
table_svmp
