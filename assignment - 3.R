set.seed(100);

#install.packages("cluster"); 
#install.packages("mclust");
#install.packages("poLCA");
library(cluster);
library(mclust);
library(poLCA);
install.packages("MASS"); 
install.packages("ggplot2");
install.packages("gmodels");
install.packages("e1071"); 
install.packages("mclust");
install.packages("randomForest");
install.packages("rpart")
install.packages("nFactors");
install.packages("GPArotation");
install.packages("semPlot")
install.packages(gplots)
library(MASS);
library(ggplot2);
library(gmodels);
library(e1071);
library(mclust);
library(randomForest);
library(rpart)
library(caret)
library(gplots);
library(corrplot);
library(nFactors);
library(GPArotation);
library(RColorBrewer);
library(semPlot);
library(gplots);
library(corrplot);
library(nFactors);
library(GPArotation);
library(RColorBrewer);
library(semPlot);
library(cluster);
library(mclust);
library(poLCA);

#Question - 3 
train<-read.csv("bank_train.csv")
test<-read.csv("bank_valid.csv")
str(train)
str(test)

#LDA on train
lda_q3 <-train[,-2:-3]
str(lda_q3)

lda_model <- lda(y~., data=lda_q3);
lda_model

#Question - 4
# LDA on train
q4_lda<- lda(y~., data=train)
q4_lda

#LDA predict on test
q4_predict <- predict(q4_lda, test)$class;
q4_predict

q4_table <- table(q4_predict, test$y)
q4_table

#LDA accuracy on test
accuracy <- sum(diag(q4_table))/sum(q4_table);
accuracy

# JAccard on test
q4_jac <- q4_table[4]/sum(q4_table[2:4])
q4_jac

#Question - 5 Naive prior probability
prob<- table(test$y)
prop.table(prob)

# Question - 6 Naive
#Train
q6_train <- naiveBayes(y~ age + duration + previous, data = train)
q6_train

#Valid set
q6_valid <- predict(q6_train, test)
q6_valid

#Accuracy
q6_accuracy <- table(test$y, q6_valid) 
diag(prop.table(q6_accuracy,1))
sum(diag(prop.table(q6_accuracy)))

#Jaccard
q6_jac <- q6_accuracy[4]/ sum(q6_accuracy[2:4])
q6_jac


#Question - 10

#Random Forest
q10_train <- randomForest(y ~ ., data=train, ntree=1000)
q10_test <- predict(q10_train, test)

q10_table <- table(q10_test)

#Jaccard
q10_jac <- diag(q10_table)[2]/(sum(q10_table)-diag(q10_table)[1]);

#Question - 11

#Random Forest
q11_train <- randomForest(y ~ age+duration+previous, data=train, ntree=500)
q11_test <- predict(q11_train, test)


q11_table <- table(q11_test)

#Jaccard
q11_jac <- diag(q11_table)[2]/(sum(q11_table)-diag(q11_table)[1]);

### Factor analysis

data<-read.csv("servqual.csv")
str(data)

scaled <- scale(data[,3:24]);

three_factors<-factanal(scaled, factors=3, scores = "Bartlett")
three_factors

semPaths(three_factors, what = "est", residuals = FALSE, posCol = c("white", "darkblue"), nCharNodes = 0)
semPaths

heatmap.2(three_factors$loadings,
          col = brewer.pal(9, "Blues"), trace="none",
          key = FALSE, dend = "none", Colv = FALSE, cexCol = 1.2,
          main = "Factors loadings for brand adjectives");

three_factors1<-factanal(scaled, factors=3, rotation="oblimin", scores = "Bartlett")
three_factors1

three_factors<- data.frame(three_factors$scores)
three_factors

three_factors1_scores <- data.frame(three_factors1$scores)

#Regression
q17_reg1 <- lm(servqual~ three_factors1_scores[,1] + three_factors1_scores[,3], data=data)
q17_reg1

q18_reg2 <- lm(servqual~ three_factors1_scores[,1] + three_factors1_scores[,2] + three_factors1_scores[,3], data=data)
q18_reg2



