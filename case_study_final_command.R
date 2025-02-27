library(readxl)
library(reshape2)
library(epiDisplay)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
setwd("D://Data_analysis//")
getwd()
dt <- read.csv("Titanic-Dataset.csv")
dt <- data.frame(dt)
str(dt)

#select 8 variables
dt1 <- dt[,c(1:3,5:8,12)]
str(dt1)

# check duplicated data
dt1$checkId <- paste(dt1$PassengerId)
dup <- duplicated(dt1$checkId)
table(dup)

dt1[dup==T,]
dt1 <- dt1[dup==F,]
str(dt1)

# Replace empty strings with NA in character columns
dt1[dt1 == ""] <- NA
missing_value <- is.na(dt1)
missing_count <- colSums(missing_value)
missing_count
str(dt1)

# Handle missing values of variable Age 
# replace Age variable that has missing values
mean_age <- mean(dt1$Age, na.rm = TRUE)
mean_age
dt1$age <- ifelse(is.na(dt1$Age),mean_age, dt1$Age)
str(dt1)

#Handle missing value of Embarked variable
missing_embarked_count <- sum(is.na(dt1$Embarked))
missing_embarked_count
# drop 2 columns 
dt1 <- dt1[!is.na(dt1$Embarked), ]
str(dt1)

#Explore data in each variable
#Survived variable
dt1$Survived <- factor(dt1$Survived, levels = c(0, 1), labels = c("ไม่รอด", "รอด"))
tab1(dt1$Survived, bar.value="percent", 
     main="Distribution of Survived", col=c("yellow","blue"))
mtext(side=3, "Percent", adj=-0.1, padj=0)
#Number of person survived is less than demise

#Pclass variable
dt1$Pclass <- factor(dt1$Pclass, levels = c(1, 2, 3), labels = c("ชั้น1","ชั้น2","ชั้น3"))
tab1(dt1$Pclass, bar.value = "percent", 
     main="Distribution of Passenger class", col=cm.colors(3))
mtext(side=3, "Percent", adj=-0.1, padj=0)
#The above visualisation clearly states most of the people are from 3rd class

#age
hist(dt1$age, col="orange", main="Distribution of age")
shapiro.test(dt1$age)
quantile(dt1$age)
dt1$age <- cut(dt1$age, c(0, 22, 29, 35, 80))
dt1$age <- as.factor(dt1$age)
levels(dt1$age) <- c("<=22","23-29", "30-35", "36+")
tab1(dt1$age, bar.value="percent", col=terrain.colors(4),
       main="Distribution of age")
mtext(side=3, "Percent", adj=-0.1, padj=0)
#The above visualisation clearly states most of the people are in age 20-30

#Sex
dt1$Sex <- factor(dt1$Sex, levels = c("male", "female"), labels = c("M", "F"))
tab1(dt1$Sex, bar.value = "percent", 
     main="Distribution of Sex", col=c("orange","pink"))
mtext(side=3, "Percent", adj=-0.1, padj=0)
#The above visualisation clearly states most of the people are male than female

#SibSp
hist(dt1$SibSp, main="Number of siblings or spouses", col=c("blue"))
dt1$SibSp <- as.factor(dt1$SibSp)
dt1$SibSp <- ifelse(dt1$SibSp == 0, "ไม่มี", "มี")
tab1(dt1$SibSp, bar.value="percent", 
     main="Distribution of SibSp variable", col=rainbow(2))
mtext(side=3, "Percent", adj=-0.1, padj=0)
#The above visualisation clearly states most of the people travelled didnot have sibling or spouse

#Parch
hist(dt1$Parch, main="Number of parents or children", col=c("orange"))
dt1$Parch <- as.factor(dt1$Parch)
dt1$Parch <- ifelse(dt1$Parch == 0, "ไม่มี", "มี")
tab1(dt1$Parch, bar.value="percent", 
     main="Distribution of Parch variable", col=rainbow(2))
mtext(side=3, "Percent", adj=-0.1, padj=0)
#The above visualisation clearly states most of the people travelled didnot have children accompanying them


#turn Embarked variable to full name
dt1$Embarked <- factor(dt1$Embarked, levels = c("C", "Q", "S"), 
                      labels = c("Cherbourg", "Queenstown", "Southampton"))
tab1(dt1$Embarked, bar.value="percent", 
     main="Distribution of the port of embarkation", col=rainbow(3))
mtext(side=3, "Percent", adj=-0.1, padj=2)
#The above visualisation clearly states most of the people are from the port Southampton

# การวิเคราะห์ตัวแปรทีละคู่ระหว่างตัวแปรตามและตัวแปรอิสระ
tableStack(data= dt1, c(Pclass, Sex, age, SibSp, Parch, Embarked), by = Survived)

#ข้อตกลง (Assumptions) : 
#- ค่าความคลาดคลื่น ( error ) เป็นอิสระต่อกัน
#- ไม่มี multicollinearity ระหว่างตัวแปรอิสระ

# สร้างตัวแบบ
mod <- glm(formula = Survived ~ Pclass + age + Sex + SibSp + Parch + Embarked,
       family = binomial, data = dt1)
summary(mod)

bw <- step(mod, direction = "backward")
bw$anova

# การสร้างตัวแบบสุดท้าย
m2 <- glm(data=dt1, Survived ~ Embarked + age + Pclass + Sex, family=binomial)
summary(m2)

# การแปลผลจากตัวแบบสุดท้ายที่เลือก
logistic.display(m2)

# การวินิจฉัยตัวแบบ
ro <- lroc(m2, graph=TRUE)
attributes(ro)
ro$auc