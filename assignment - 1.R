
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
lifestyle <- read.csv("lifestyle.csv");
str(lifestyle); #display structure
summary(lifestyle); #summarize contents

experiment <- read.csv("experiment_price_ad.csv");
str(experiment); #display structure
summary(experiment); #summarize contents

############

#### Chi Square crosstab
#### Question - 1
chi <- chisq.test(lifestyle$buyamer, lifestyle$emplmerg)
chi$observed
chi$expected
str(chi)
summary(chi)

# Chi Squred test statistic
#### Quesiton - 3
chi;

#### Question - 4
chi$p.value

##### Question - 6
str(lifestyle)
cor <- cor.test(lifestyle$deathpen, lifestyle$age)
cor;

##### Question - 8
str(lifestyle)
reg <- lm(lifestyle$netdome~lifestyle$age)
reg
summary(reg)

##### Question - 9
summary(reg)

##### Quesiton - 10
summary(reg)

##### Quesiton - 13
summary(reg)
reg1 <- lm(lifestyle$netdome~lifestyle$sex)
summary(reg1)

##### Question - 14
reg2 <- lm(liberal~age+politics, data=lifestyle)
summary(reg2)

##### Question - 17
reg3 <- lm(netdome~age+politics, data = lifestyle)
summary(reg3)

##### Question - 18
str(experiment)
reg_exp <- lm(experiment$adv~experiment$Sales, data = experiment)
summary(reg_exp)

##### Question - 19
mul_reg_exp <- lm(Sales ~ price + adv, data = experiment)
summary(mul_reg_exp)

##### Question - 20
mul_reg_exp1 <- lm(Sales ~ price + adv + store.volume, data = experiment)
summary(mul_reg_exp1)
