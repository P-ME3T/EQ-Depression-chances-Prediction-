#Importing libraries and Dataset
library(readxl)
EQ_final <- read_excel("C:/Users/Admin/Downloads/EQ_final.xlsx")

attach(EQ_final)
names(EQ_final)

#Linear Regression 
Linear_reg_1<-lm(EQ~Conscientiousness)
summary(Linear_reg_1)

Linear_reg_2<-lm(EQ~Neuroticism)
summary(Linear_reg_2)

Linear_reg_3<-lm(EQ~Extraversion)
summary(Linear_reg_3)

Linear_reg_4 <- lm(EQ~Openness)
summary(Linear_reg_4)

Linear_reg_5<-lm(EQ~Agreeableness)
summary(Linear_reg_5)

Linear_reg<-lm(EQ~Openness+Extraversion+Neuroticism+Agreeableness+I(Conscientiousness^3))
summary(Linear_reg)

plot(EQ,Extraversion,main = "Scatterplot")

#naive Bayes theorem
library(e1071)
library(caTools)

EQ_final$Depression=as.factor(EQ_final$Depression)
set.seed(200)

split = sample.split(EQ_final$Depression, SplitRatio = 0.95)
training_set = subset(EQ_final, split == TRUE)
test_set = subset(EQ_final, split == FALSE)

bays<-naiveBayes(Depression~.-EQ,data=training_set)
train_naive<-predict(bays,training_set)
train_ans<-table(train_naive,training_set$Depression)
sum(diag(train_ans))/sum(train_ans)

test_naive<-predict(bays,test_set)
test_ans<-table(test_naive,test_set$Depression)
sum(diag(test_ans))/sum(test_ans)

train_ans
test_ans

#Decision tree
library(rpart)

classifier<-rpart(formula=Depression~.-EQ,data=training_set)

train<-predict(classifier,training_set,type="class")
train_cm<-table(train,training_set$Depression)
sum(diag(train_cm))/sum(train_cm)

train<-predict(classifier,test_set,type="class")
train_cm1<-table(train,test_set$Depression)
sum(diag(train_cm1))/sum(train_cm1)

train_cm
train_cm1