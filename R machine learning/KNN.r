person1 = list(name = "Person1", blood = factor("A", levels = c("A", "B", "AB", "O")), gender = factor("MALE", levels = c("MALE","FEMALE")))
person1     

data(iris)
str(iris)
head(iris)

summary(iris)

n.point = nrow(iris)
sampling.rate = 0.7

training = sample(n.point, sampling.rate*n.point, replace = FALSE)
training
set.seed(1234)  #set.seed get the same random result again

?setdiff
testing = setdiff(1:n.point, training)
testing

iris_train = subset(iris[training,])
iris_train
iris_test = subset(iris[testing,])
iris_test

table(iris_train$Species)
table(iris_test$Species)

?prop.table
prop.table(table(iris_train$Species))

str(iris_train)
spc_train = iris_train[,5]
spc_test = iris_test[,5]
iris_train = iris_train[,-5]
iris_train
iris_test = iris_test[,-5]
normalize = function(x){
  return((x-min(x))/(max(x)-min(x)))
}
?lapply
# lapply - list; sapply - dataframe; vapply - vector

iris_train_n = sapply(iris_train, normalize)
iris_test_n = sapply(iris_test, normalize)
summary(iris_train_n)

library(class)
predict_spc = knn(iris_train_n, iris_test_n, spc_train, k=1, prob=TRUE)
predict_spc

predict_spc3 = knn(iris_train_n, iris_test_n, spc_train, k=3, spc_train, prob=TRUE)
predict_spc3

plot(iris[,1:4], col=iris$Species)

error.rate = NULL

for(i in 1:5){
  set.seed(2017)
  predict_spc_1_5 = knn(iris_train_n, iris_test_n, spc_train, k=i)
  error.rate[i] = mean(spc_test!=predict_spc_1_5)}

error.rate

k = 1:5
plot(k, error.rate, type = "b", col = "red")

misclass.error = mean(spc_test != predict_spc)
print(misclass.error)

install.packages("gmodels")
library(gmodels)
CrossTable(x = spc_test, y = predict_spc, prop.chrisq = FALSE)
