library(gmodels)
install.packages("mlbench")
library(mlbench)
data("HouseVotes84")
# V are issues
str(HouseVotes84)# 1 means no, 2 means yes
table(HouseVotes84$V1)

n.point = nrow(HouseVotes84)
sampling.rate = 0.7

training = sample(n.point, sampling.rate*n.point, replace = FALSE)
training

testing = setdiff(1:n.point, training)
testing

house_train = subset(HouseVotes84[training,])
house_test = subset(HouseVotes84[testing,])

install.packages("e1071")
library(e1071)
?naiveBayes

model = naiveBayes(Class~., data = house_train) #"." means all the factors
summary(model)

model

class_predict = predict(model, house_test)
?predict

str(class_predict)
CrossTable(x = house_test$Class, y = class_predict, prop.chisq = FALSE)
