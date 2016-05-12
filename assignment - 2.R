
install.packages("gpairs")
install.packages("car")
install.packages("corrplot")
install.packages("coefplot")
install.packages("ggplot2")
library("car")
library("gpairs")
library("corrplot")
library("ggplot2")
library("coefplot")

#upload data from google 
bankt <- read.csv("bank_train.csv");
head(bankt)
str(bankt); #display structure
summary(bankt); #summarize contents

# predicted probability
# Logit value
# Q - 1
plogis(1.5)
qlogis(.81)

# Q- 2
model_1 <- glm(y ~ age + marital + housing + duration , family = binomial("logit"), data = bankt)
summary(model_1)

# Q - 4
plogis(.00641)
plogis(.1090)

# Log odds
# Q - 5
exp(coef(model_1))

# Q - 6
t1<-data.frame(30, "single", "yes", 400)
colnames(t1)<-c("age", "marital", "housing", "duration")
predict(model_1, t1, type="response")

# Q - 7
t2<- data.frame(60, "single", "no", 100)
colnames(t2)<-c("age", "marital", "housing", "duration")
predict(model_1, t2, type="response")

# Q - 8
t3<- data.frame(50, "married", "yes", 200)
colnames(t3)<-c("age", "marital", "housing", "duration")
predict(model_1, t3, type="link")

# Q - 9
bankv<- read.csv("bank_valid.csv", head= TRUE);
nv<-dim(bankv)[1]
bankv_predicted<-predict.glm(model_1, bankv, type="response")
bankv_response<-rep(0,nv)
bankv_response[bankv_predicted > .5]= 1;
table(bankv_response)
str(bankv)
table(bankv[,5], bankv_response)