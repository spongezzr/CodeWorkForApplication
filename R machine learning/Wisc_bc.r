getwd()
setwd("/Users/michaelz/Document/Data Science Camp/R machine learning")
data_cancer = read.csv("wisc_bc_data.csv")
data_cancer = data_cancer[,-1]
summary(data_cancer)
number = nrow(data_cancer)
sample.rate = 0.7

training2 = sample(number, number*sample.rate, replace = FALSE)
testing2 = setdiff(1:number, training2)

cancer_train = subset(data_cancer[training2,])
cancer_test = subset(data_cancer[testing2,])

diag_train = cancer_train[,1]
diag_test = cancer_test[,1]

cancer_train = cancer_train[,-1]
cancer_test = cancer_test[,-1]

which(colnames(iris) == "Species")

normalize = function(x){
    return((x-min(x))/(max(x)-min(x)))
}

cancer_train_n = sapply(cancer_train, normalize)
cancer_test_n = sapply(cancer_test, normalize)
summary(cancer_train_n)

predict_diag = knn(cancer_train_n, cancer_test_n, diag_train, k=1, prob=TRUE)
predict_diag

error.rate = mean(diag_test!=predict_diag)
error.rate

error.rate = NULL

for(i in 1:30){
  set.seed(2017)
  predict_diag = knn(cancer_train_n, cancer_test_n, diag_train, k=i)
  error.rate[i] = mean(diag_test!=predict_diag)}

error.rate
