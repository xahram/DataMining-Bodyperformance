# KNOWLEDGE DISCOVERY QUESTIONS


# LOAD LIBRARIES
library(ggplot2)
library(dplyr)
library(caTools)
library(ggfortify)


# bodyPerformance <- read.csv("./Datasets/marketing_campaign.csv", header= T, sep = '\t')
bodyPerformance <- read.csv("./Datasets/bodyPerformance.csv", stringsAsFactors = T)


str(bodyPerformance)
View(bodyPerformance)

# na.omit(bodyPerformance)

scaled_bodyPerformance <- scale(bodyPerformance[,3:11])
final_dataset <- cbind(scaled_bodyPerformance, bodyPerformance[,c(1,2,12)])
View(final_dataset)

### TRAIN TEST SPLIT
###

set.seed(101)

sample <- sample.split(bodyPerformance, SplitRatio = 0.7)
train <- subset(bodyPerformance, sample == T) 
test <- subset(bodyPerformance, sample == F)

View(test)

#### PRE PROCESSING ####

bodyperformance_GLM <- bodyPerformance

age_f <- function(x){
  if(x <= 25) return(as.factor("young"))
  if(x <= 50 && x>= 25) return(as.factor("adult"))
  if (x > 50) return(as.factor("old"))
}

max(bodyperformance_GLM$systolic)
min(bodyperformance_GLM$diastolic)


bodyperformance_GLM %>% mutate(bp = case_when(systolic <= 120  & diastolic <= 80 ~ "Normal",
                                              systolic %in% 120:129 & diastolic %in% 50:95 ~ "Elevated",
                                              systolic %in% 130:180 & diastolic %in% 60:100 ~ "hypertension",
                                              systolic >= 180 & diastolic >= 90~ "crises",
                                              TRUE  ~ "unknown")) -> bodyperformance_GLM

table(bodyperformance_GLM$bp)                               
View(bodyperformance_GLM)
bodyperformance_GLM$bp <- as.factor(bodyperformance_GLM$bp)

bodyperformance_GLM <- select(bodyperformance_GLM, -c("diastolic","systolic"))
View(bodyperformance_GLM)

sample <- sample.split(bodyperformance_GLM, SplitRatio = 0.7)
train <- subset(bodyperformance_GLM, sample == T) 
test <- subset(bodyperformance_GLM, sample == F)

### LOGISTIC REGRESSION

model <- glm(bp ~ . , family = binomial("logit"), data = train)
summary(model)

new_model <- step(model)
summary(new_model)

# Confusion Matrix
test$predictedagegroup <- predict(new_model, newdata = test, type="response") 
table(test$bp, test$predictedagegroup>0.5)
length(test$predictedagegroup)

mean(test$predictedagegroup)

head(test$bp)
View(test)
head(test$predictedagegroup)
### CHECKING ADULT CLASSIFICATION ACCURACY
acc <- (1591+ 784 + 1133) / 4465
precision_adult <- 4465 / (1447 + 770 + 145)


print(recall_adult)


##### MLR

model_lm <- lm(age ~ height_cm + body.fat_. + 
   weight_kg + sit.ups.counts + sit.and.bend.forward_cm + gripForce, bodyPerformance)

predict(model_lm, test)

summary(predict)




#####    DT
library(rpart)

tree <- rpart(bp ~  ., method = "class", data= train)
summary(tree)
tree_preds <- predict(tree, test)
head(tree_preds)
tree.preds <- as.data.frame(tree_preds)


View(tree.preds)

bodyPerformance_DT <- cbind(tree_preds,as.character(test$bp))
bodyPerformance_DT <- as.data.frame(bodyPerformance_DT)

bodyPerformance_DT$max <- NULL

bodyPerformance_DT$max <- colnames(bodyPerformance_DT)[max.col(bodyPerformance_DT[,1:4], ties.method = "first")]

View(bodyPerformance_DT)



DT_accuracy <- ifelse(bodyPerformance_DT$V6 == bodyPerformance_DT$max , T, F)
table(DT_accuracy)

dt_acc <- 2711 / 4810
dt_acc

bodyperformance_GLM


### RANDOM FOREST

library(randomForest)
bodyperformance_GLM$bp <- as.factor(bodyperformance_GLM$bp)

sample <- sample.split(bodyperformance_GLM, SplitRatio = 0.7)
train <- subset(bodyperformance_GLM, sample == T) 
test <- subset(bodyperformance_GLM, sample == F)


rf.model <- randomForest(bp ~ body.fat_. + height_cm +  weight_kg + sit.and.bend.forward_cm  +  gripForce + sit.ups.counts + age , data = train, importance= T)
rf.preds <- predict(rf.model, test)
# Confusion Matrix of Random Forest
table(rf.preds, test$bp)

nrow(test)
# DT GIVES 0.7390817 percent accuracy in our quest




# KNN NOT GOOD AT NUMERICA DATA BUT WORKS WELL ON CATEGORICAL
library(class)

### uSW ORIGINAL BODYPERFORM_GLM AND BODYPERFORMANCE FOR ACCURACY
sample <- sample.split(bodyperformance_GLM, SplitRatio = 0.7)
train <- subset(bodyperformance_GLM, sample == T) 
test <- subset(bodyperformance_GLM, sample == F)

View(train)
View(bodyperformance_GLM)
# CHECK FOR K VALUES
error.rate <- NULL
for (i in 1:10){
  set.seed(101)
  predicted.age <- knn(train[,c(1,3,4,5,6,7,8,9)], test[,c(1,3,4,5,6,7,8,9)], train$bp, k= i )
  error.rate[i] =  mean(test$bp != predicted.age)
}
k.values <- 1:10
error.df <- data.frame(error.rate, k.values)

pl <- ggplot(error.df, aes(x=k.values, y= error.rate)) +  geom_point()
pl <- pl + geom_line(lty="dotted", color= "red", size = 2)

print(pl)


predicted.values <- knn(train[,3:9] , test[,3:9], train$bp, k= 5) 
print(predicted.values)
View(as.data.frame(predicted.values))

print(table(test$bp == predicted.values))

new.df <- NULL
new.df <- cbind(test$bp, predicted.values)
View(new.df)
######## Choose K values

predicted.age <- NULL
error.rate <- NULL





#### KMEANS ####

# ELBOW METHOD
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}



# KMEANS FOR JUST AGE AND BP
km <- kmeans(bodyPerformance[,c(1,6,7)], 5)
autoplot(km, bodyPerformance[,c(1,6,7)], frame = 1)
km$centers

View(bodyPerformance)
# Evaluating KMEANS
wssplot(bodyPerformance[,-c(2,12)])

km <- kmeans(bodyPerformance[,-c(2,12)], 3)
autoplot(km, bodyPerformance[,-c(2,12)], frame = 1)
km$centers

# KMEANS ACCURACY CHECK
table(bodyperformance_GLM$bp, km$cluster)
print(km$cluster)

print()




































joiner <-  function(x,y,z,a){
  result <- NULL
  if(x > y && y > z && z > a)  {
    result <- "Elevated"
  } 
  if(y > z && z > a && a > x){
    result <- "Normal"
  }  
  if(z > a && a > y && y > x){
    result <- "hypertension"
  } 
  #else {
  # result <- "unknown"
  #}
  return(result)
  
}

bodyPerformance_DT$bp <- sapply(bodyPerformance_DT$Elevated,
                                joiner,
                                bodyPerformance_DT$Normal,
                                bodyPerformance_DT$hypertension,
                                bodyPerformance_DT$unknown
)


DT_accuracy <- sapply(bodyPerformance_DT$max,
                      acc,
                      bodyPerformance_DT$V5)



acc <- function(x,y){
  if(x == y) {
    return("True") 
  }
  else {
    
    return("False")
  } 
}






bloodpressure <-  function(x,y){
  if(x < 120 && y < 80) return("Normal")
  if((x > 120 && x < 130) && (y < 80)) return("Elevated")
  if(x > 129 && x < 180) return("hypertension")
  if(x >= 180 && y >= 120) return("crises")
  else return("unknown")
}
bodyperformance_GLM$bp <- sapply(bodyperformance_GLM$systolic, bloodpressure , bodyperformance_GLM$diastolic)
