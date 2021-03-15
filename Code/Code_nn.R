# Load the code for data pre-processing:

source('Code/Common_1.R')

# Install and load the caret library

#install.packages(caret)
library(caret)

# Split the data

set.seed(1)
train_idx <- createDataPartition(exp$Subtype, p = 2/3, list = FALSE)

exp_train <- exp[train_idx, ]
exp_test <- exp[-train_idx, ]

# Check that the proportions are kept

prop.table(table(exp$Subtype))
prop.table(table(exp_train$Subtype))
prop.table(table(exp_test$Subtype))

# Set the training control parameters

tr_control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3, search = 'grid')

nn_parameter <- expand.grid(size = c(1:3), decay = seq(0.01, 0.1, by = 0.01))
# Create the model

set.seed(1)
nn_1 <- train(Subtype ~ ., data = exp_train, method = 'nnet', tuneGrid = nn_parameter, lineout = 0, trControl = tr_control)

nn_t <- predict(nn_1, exp_test)

table_nn <- confusionMatrix(nn_t, exp_test$Subtype)
table_nn
