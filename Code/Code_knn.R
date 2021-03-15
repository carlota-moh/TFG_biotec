# Code for k-nearest neighbour analysis, performed over the IDH-WT samples, filtered for considering only the 15 genes of interest and processed for internal normalization of the samples.

# Load the code for data pre-processing:

source('Code/Common_1.R')

# Install and load the caret library

#install.packages(caret)
library(caret)
# library(doSNOW) # Completely optional, just for working in parallel when having large CVs and lots of parameters

# First of all, split the data so that we have a training cohort and a testing cohort. Do this by creating a data partition using the createDataPartition() function

set.seed(1)
train_idx <- createDataPartition(exp$Subtype, p = .8, list = FALSE)

exp_train <- exp[train_idx, ]
exp_test <- exp[-train_idx, ]

# Check that the proportions are kept

prop.table(table(exp$Subtype))
prop.table(table(exp_train$Subtype))
prop.table(table(exp_test$Subtype))

# Set the training control parameters

tr_control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'grid')

k <- c(1:50)
k_parameter <- data.frame(k)

# I do not think it will be necessary, but just in case I include the code for using doSNOW and working in parallel

# cl <- makeCluster(5, type = 'SOCK')

# registerDoSNOW(cl)

# Now we are ready to build the model

set.seed(1)
knn_1 <- train(Subtype ~ ., data = exp_train, method = 'knn', tuneGrid = k_parameter, trControl = tr_control)

# stopCluster(cl) # In case we want to use doSNOW

# Use the model to make predictions on the unseen data (aka testing data)

knn_t <- predict(knn_1, exp_test)

# Build a confusion matrix to check the results

table_k <- confusionMatrix(knn_t, exp_test$Subtype)
table_k

