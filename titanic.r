install.packages("titanic")
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

### 3 significant digits
options(digits = 3)

### clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

### setting the seed to 42, splitting the data in 20/80 = test/train data
set.seed(42, sample.kind = 'Rounding') # if R version >= 3.6
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean[-test_index,]
test_set <- titanic_clean[test_index,]


## Trying to apply LOESS
## Titanic exercises part2 - Survival by fare - Loess
### Q7

set.seed(1)
### Load the 'caret' package if you haven't already
library(caret)

### Create a train control object for cross-validation (if needed)
trainControl <- trainControl(method = "cv", number = 10)  # Use 10-fold cross-validation

### Create the formula for the model (Survived ~ fare)
### formula <- as.formula("Survived ~ Fare")

### Ensure 'Survived' is a factor with two levels (0 and 1)
train_set$Survived <- as.factor(train_set$Survived)
test_set$Survived <- as.factor(test_set$Survived)

### Train the Loess model using the gamLoess method
loess_model <- train(Survived ~ Fare, data = train_set, method = "gamLoess", trControl = trainControl)

### making predictions
predictions <- predict(loess_model, newdata = test_set)

### checking predictions
class(predictions)
unique(predictions)

### Calculate the accuracy for the model
accuracy <- mean(predictions == test_set$Survived)
print(accuracy)

### Q8
### using caret glm method and only Age as predictor
fit_logreg_a <- glm(Survived ~ Age, data = train_set, family = 'binomial')
survived_hat_a <- ifelse(predict(fit_logreg_a, test_set) >= 0, 1, 0)
mean(survived_hat_a == test_set$Survived)

### setting seed = 1, using caret glm method and sex, class, fare and age as predictors
set.seed(1)
fit_logreg_b <- glm(Survived ~ Sex + Pclass + Fare + Age, data = train_set, family = 'binomial')
survived_hat_b <- ifelse(predict(fit_logreg_b, test_set) >= 0, 1, 0)
mean(survived_hat_b == test_set$Survived)

### using caret glm method and sex, class, fare and age as predictors
set.seed(1)
str(train_set)
fit_logreg_c <- glm(Survived ~ ., data = train_set, family = 'binomial')
survived_hat_c <- ifelse(predict(fit_logreg_c, test_set) >= 0, 1, 0)
mean(survived_hat_c == test_set$Survived)

### Q9 Knn model
k <- seq(3,51,2)
fit_knn9a <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k))
fit_knn9a$bestTune

ggplot(fit_knn9a)

survived_hat <- predict(fit_knn9a, test_set) %>% factor(levels = levels(test_set$Survived))
cm_test <- confusionMatrix(data = survived_hat, reference = test_set$Survived)
cm_test$overall["Accuracy"]