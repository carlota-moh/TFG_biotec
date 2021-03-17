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

nb_parameters <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = 1:5) ## Dado que todas las variables aparecen en los diferentes grupos no creo que tenga mucho sentido usar fL. Por otra parte, el código da errores cuando adjust = 0 & usekernel = TRUE.

# Use DoSNOW for working in parallel

# cl <- makeCluster(5, type = 'SOCK')

# registerDoSNOW(cl)

set.seed(1)
nb <- train(Subtype ~ ., data = exp_train, method = 'nb', trControl = tr_control, tuneGrid = nb_parameters)

# stopCluster(cl)

nb
plot(nb) ## No entiendo por qué elige un adjust = 1 cuando en el lot aparece que el accuracy max se alcanza con adjust = 5

nb_t <- predict(nb, exp_test)

table_nb <- confusionMatrix(nb_t, exp_test$Subtype)
table_nb
