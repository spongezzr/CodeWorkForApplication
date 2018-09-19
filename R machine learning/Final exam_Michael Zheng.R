sink("final_console_2")
f = read.csv("bank_cat-1.csv")
str(f)
summary(f)

any(is.na(f))

#use decision tree
set.seed(123)
train_dt = sample(nrow(f), nrow(f)*0.8, replace = FALSE)
f_train_dt = f[train_dt,]
f_test_dt = f[-train_dt,]

prop.table(table(f_train_dt$y))
prop.table(table(f_test_dt$y))

library(C50)
set.seed(123)
f_model_dt = C5.0(f_train_dt[-16], f_train_dt$y)
f_model_dt
f_pred_dt = predict(f_model_dt, f_test_dt)
summary(f_pred_dt)
library(gmodels)
CrossTable(f_test_dt$y, f_pred_dt, prop.chisq = FALSE, dnn = c('actual y', 'predicted y'))

error_dt = mean(f_test_dt$y != f_pred_dt)
error_dt

#boost the decision tree using adaboost
library(adabag)
set.seed(123)
f_model_dta = boosting(y ~ ., data = f)
f_pred_dta = predict(f_model_dta, f)
f_pred_dta$confusion
kappa(f_pred_dta$confusion)
detach(package:adabag)

#boost the decision tree using C5.0
error_bdt = NULL      
for(i in 1:3){ 
    set.seed(123)
    f_model_bdt = C5.0(f_train_dt[-16], f_train_dt$y, trial = (10*i))
    f_pred_bdt = predict(f_model_bdt, f_test_dt)
    error_bdt[i] = mean(f_test_dt$y != f_pred_bdt)
    CrossTable(f_test_dt$y, f_pred_bdt, prop.chisq = FALSE, dnn = c('actual y', 'predicted y'))}
error_bdt

#use decision tree with data partition
library(caret)
set.seed(123)
train_i = createDataPartition(f$y, p = 0.8, list = FALSE)
f_train_i = f[train_i,]
f_test_i = f[-train_i,]

prop.table(table(f_train_i$y))
prop.table(table(f_test_i$y))

set.seed(123)
f_model_i = C5.0(f_train_i[-16], f_train_i$y)
f_model_i
f_pred_i = predict(f_model_i, f_test_i)
summary(f_pred_i)

CrossTable(f_test_i$y, f_pred_i, prop.chisq = TRUE, prop.c = TRUE, 
           prop.r = TRUE, dnn = c('actual y', 'predicted y'))

error_i = mean(f_test_i$y != f_pred_i)
error_i

#use Bagging
library(ipred)
set.seed(123)
f_bag = bagging(y ~ ., data = f, nbagg = 25)
f_pred_bag = predict(f_bag, f)
table(f_pred_bag, f$y)

error_bag = mean(f_pred_bag != f$y)
error_bag

CrossTable(f_pred_bag, f$y)

set.seed(123)
f_ctrl = trainControl(method = "cv", number = 10)
train(y ~., data = f, method = "treebag", 
      trControl = f_ctrl)

#tuning

set.seed(123)
f_tu = train(y ~ ., data = f, method = "C5.0")

summary(f_tu)

f_pred_tu = predict(f_tu, f)
table(f_pred_tu, f$y)
CrossTable(f_pred_tu, f$y)

#Customize the tuning process
f_ctrl_tu <- trainControl(method = "cv", number = 10,
                     selectionFunction = "oneSE")

f_grid_tu <- expand.grid(.model = "tree",
                    .trials = c(1, 5, 10, 15, 20, 25, 30, 35),
                    .winnow = "FALSE")

f_grid_tu

set.seed(123)
f_ctu <- train(y ~ ., data = f, method = "C5.0",
           metric = "Kappa",
           trControl = f_ctrl_tu,
           tuneGrid = f_grid_tu)

f_ctu

#use random forest
library(randomForest)

set.seed(123)
randomForest(y ~ ., data = f)

library(caret)
f_ctrl_rf = trainControl(method = "repeatedcv",
                         number = 10, repeats = 10)
f_grid_rf = expand.grid(.mtry = c(2,4,8,16))
set.seed(123)
f_rf = train(y ~ ., data = f, method = "rf", metric = "Kappa", 
             trControl = f_ctrl_rf, tuneGrid = f_grid_rf)
f_rf
f_grid_C50 = expand.grid(.model = "tree", .trials = c(10,20,30), 
                         .winnow = "FALSE")
set.seed(123)
f_c50 = train(y ~ ., data = f, method = "C5.0", metric = "Kappa", 
              trControl = f_ctrl_rf, tuneGrid = f_grid_C50)
f_c50
sink()
