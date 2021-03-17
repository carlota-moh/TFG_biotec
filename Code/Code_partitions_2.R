source('Code/Code_partitions.R')

# Import the function for building the models

source('Code/Code_functions_2.R')

# Split the data

set.seed(123)
train_idx_0.66 <- createDataPartition(exp$Subtype, p = 2/3, list = FALSE)

exp_train <- exp[train_idx_0.66, ]
exp_test <- exp[-train_idx_0.66, ]

model_0.662 <- model(exp_train, exp_test)


set.seed(123)
train_idx_0.75 <- createDataPartition(exp$Subtype, p = 0.75, list = FALSE)

exp_train <- exp[train_idx_0.75, ]
exp_test <- exp[-train_idx_0.75, ]

model_0.752 <- model(exp_train, exp_test)

set.seed(123)
train_idx_0.8 <- createDataPartition(exp$Subtype, p = 0.8, list = FALSE)

exp_train <- exp[train_idx_0.8, ]
exp_test <- exp[-train_idx_0.8, ]

model_0.82 <- model(exp_train, exp_test)

set.seed(123)
train_idx_0.85 <- createDataPartition(exp$Subtype, p = 0.85, list = FALSE)

exp_train <- exp[train_idx_0.85, ]
exp_test <- exp[-train_idx_0.85, ]

model_0.852 <- model(exp_train, exp_test)

set.seed(123)
train_idx_0.9 <- createDataPartition(exp$Subtype, p = 0.9, list = FALSE)

exp_train <- exp[train_idx_0.9, ]
exp_test <- exp[-train_idx_0.9, ]

model_0.92 <- model(exp_train, exp_test)

# Compare performance across partitions

sum_accuracies2 <- c(model_0.662$Accuracies, model_0.752$Accuracies, model_0.82$Accuracies, model_0.852$Accuracies, model_0.92$Accuracies)

acc_table_2 <- data.frame(Partition = rep(c(0.66, 0.75, 0.8, 0.85, 0.9), each = 7), Accuracies = sum_accuracies2, Model_name = rep(c('knn', 'rf', 'nb', 'SVML', 'SVMPoly', 'SVMR', 'NN'), 5))

ggplot(acc_table_2) +
  theme_bw() +
  geom_line(aes(x = Partition, y = Accuracies, col = Model_name)) +
  xlim(c(0.66, 0.9)) +
  ylim(c(0.6, 1)) +
  ggtitle('Accuracies across different data partitions by model') +
  theme(axis.title = element_text(size = rel(1.5)), plot.title = element_text(size = rel(1.5), hjust = 0.5))
