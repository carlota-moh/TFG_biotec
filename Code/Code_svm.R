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

svm_parameter <- data.frame(C = seq(0, 2, by = 0.01))
# Use DoSNOW for working in parallel

# cl <- makeCluster(5, type = 'SOCK')

# registerDoSNOW(cl)

set.seed(1)
svm <- train(Subtype ~ ., data = exp_train, method = 'svmLinear', trControl = tr_control, tuneGrid = svm_parameter) 

# El método de Support Vector Machines es equivalente al Support Vector Classifier cuando el kernel utilizado es linea

# Cuando más se aproxima C a cero, menos se penalizan los errores y más observaciones pueden estar en el lado incorrecto del margen o incluso del hiperplano. C es a fin de cuentas el hiperparámetro encargado de controlar el balance entre bias y varianza del modelo. En la práctica, su valor óptimo se identifica mediante cross-validation.

plot(svm)

# stopCluster(cl)

svm

svm_t <- predict(svm, exp_test)
table_svm <- confusionMatrix(svm_t, exp_test$Subtype)
table_svm

# table_svm$overall[1]
# table_svm$byClass[,c("Sensitivity","Specificity")]