

launch <- read.csv("/machine_learning_data/challenger.csv")

str(launch)
b = cov(launch$temperature,launch$distress_ct)/var(launch$temperature) 
a = mean(launch$distress_ct)-b*mean(launch$temperature)
a
lm(distress_ct~temperature,data = launch)

r = cov(launch$temperature,launch$distress_ct)/(sd(launch$temperature)*sd(launch$distress_ct))
r

cor(launch$temperature,launch$distress_ct)

reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}

reg(y = launch$distress_ct, x = launch$temperature)
reg(y = launch$distress_ct, x = launch[3:5])




#read data from files 
insurance = read.csv("/home/wqh/workdir/machine_learning_data/insurance.csv",stringsAsFactors=T)
str(insurance)

summary(insurance$charges)

hist(insurance$charges)
table(insurance$region)

cor(insurance[c("age", "bmi", "children", "charges")])
pairs(insurance[c("age", "bmi", "children", "charges")])

# install.packages("psych")
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])


ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance)

# ins_model <- lm(charges ~ ., data = insurance)
summary(ins_model)

insurance$age2 <- insurance$age^2
a = lm(charges~age+age2,data = insurance)
summary(a)

insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
head(insurance)
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)

summary(ins_model2)

tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)

bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)


sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) + length(at2) / length(tee) * sd(at2))

sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) + length(bt2) / length(tee) * sd(bt2))



wine <- read.csv("/home/wqh/workdir/machine_learning_data/whitewines.csv")
wine
str(wine)
hist(wine$quality)
summary(wine$quality)
set.seed(100)
train = sample(4898,3750)
wine_train <- wine[train, ]
wine_test <- wine[-train, ]

hist(wine_train$quality)
hist(wine_test$quality)

install.packages("rpart")
library(rpart)

m.rpart <- rpart(quality ~ ., data = wine_train)

summary(m.rpart)
library(rpart.plot)

rpart.plot(m.rpart,digits = 3)

rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,type = 1, extra = 101)

dim(wine_test)

p.rpart <- predict(m.rpart, wine_test)

summary(p.rpart)

summary(wine_test$quality)


cor(p.rpart, wine_test$quality)


MAE <- function(actual, predicted) {
      mean(abs(actual - predicted))
}

MAE(p.rpart, wine_test$quality)
mean(wine_train$quality)

# MAE(5.872, wine_test$quality)

library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p

summary(m.m5p)

p.m5p <- predict(m.m5p, wine_test)

summary(p.m5p)

cor(p.m5p, wine_test$quality)

MAE(p.m5p,wine_test$quality )


concrete <- read.csv("/machine_learning_data/concrete.csv")
str(concrete)
hist(concrete$age)

boxplot(concrete)

boxplot(concrete_norm)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))

summary(concrete_norm$strength)
summary(concrete_norm$ash)
summary(concrete$strength)

set.seed(100)
sample = sample(1030,773)
concrete_train <- concrete_norm[sample, ]
concrete_test <- concrete_norm[-sample, ]


# install.packages("neuralnet")
library(neuralnet)
concrete_model <- neuralnet(strength ~ cement + slag + ash + water 
                            + superplastic + coarseagg + fineagg + age,
                            data = concrete_train)

plot(concrete_model)

model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)




concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)


plot(concrete_model2)


model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result


cor(predicted_strength2, concrete_test$strength)





letters <- read.csv("/machine_learning_data/letterdata.csv")
letters
str(letters)


letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]
install.packages("kernlab")

library(kernlab)

letter_classifier <- ksvm(letter ~ ., data = letters_train,kernel = "vanilladot")

###step 4 
letter_predictions <- predict(letter_classifier, letters_test)


table(letter_predictions, letters_test$letter)


agreement <- letter_predictions == letters_test$letter

agreement %>% table  %>% prop.table



##step 5
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")


letter_predictions_rbf <- predict(letter_classifier_rbf,letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter

agreement_rbf %>% table %>% prop.table



#chapter 9

## step2
teens <- read.csv("/machine_learning_data/snsdata.csv")

head(teens)

str(teens)

table(teens$gender)
table(teens$gender,useNA = "ifany")


summary(teens$age)


teens$age <- ifelse(teens$age >= 13 & teens$age < 20,teens$age, NA)

summary(teens$age)

teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

table(teens$gender, useNA = "ifany")
table(teens$female,useNA = "ifany")
table(teens$no_gender,useNA = "ifany")


mean(teens$age)

mean(teens$age,na.rm = T)

aggregate(data=teens, age ~ gradyear, mean,na.rm=T)

ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))



teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

summary(teens$age)

interests <- teens[5:40]

interests_z <- as.data.frame(lapply(interests, scale))


set.seed(2345)

teen_clusters <- kmeans(interests_z, 5) 
teen_clusters$size

teen_clusters$centers


teens$cluster <- teen_clusters$cluster

teens[1:5, c("cluster", "gender", "age", "friends")]


aggregate(data = teens, age ~ cluster, mean)

aggregate(data = teens, female ~ cluster, mean)

aggregate(data = teens, friends ~ cluster, mean)


