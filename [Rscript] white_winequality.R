###### Data Mining - Project R Script ######
setwd("C:/Users/cc933/Desktop/Mook/3학년 2학기/Data mining/프로젝트")
library_loading <- function(){
  library(tidyverse)    # data science package
  library(tm)           # Corpus
  library(ggplot2)      # Graphics for data.frame
  library(ggiraph)      # visualize linear Regression
  library(ggiraphExtra) # visualize linear Regression
  library(car)          # KNN method
  library(magrittr)     # pipe
  library(nnet)         # multinom
  library(MASS)         # LDA, QDA
  library(cvTools)      # cvFolds
  library(tree)         # tree
  library(randomForest) # randomForest
  library(reshape)      # melt
  library(gbm)          # gbm
  library(e1071)        # svm
}
library_loading()


### Data Documentation
Document <- readLines("winequality.names")
corpus_Document <- Corpus(VectorSource(Document))
inspect(corpus_Document)

### Data EDA
data <- read_csv("winequality-white.csv")
names(data) %<>% str_replace_all(" ", "_") # for convenient
a <- table(data$quality)
sum(data$quality>=6)/sum(a)
cumsum(a)/sum(a)
head(data, 10)
str(data)
summary(data)
boxplot(data)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 1.2/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(data, upper.panel = panel.cor)

### Normal distribution test
sapply(data, function(x) shapiro.test(x)$p.value)


### Multiple linear Regression
par(mfrow=c(2,2))
lm.fit <- lm(quality~., data=data)
summary(lm.fit)       # adjust R : 0.2803, RES = 0.7514
plot(lm.fit)          # check points 2782, 4746, 3308

### cleansing data
# outlier (studentized resiual check)
student_resid <- function(lm.fit, plot=F, limit=3){ # 스튜던트화 잔차
  res <- lm.fit$residuals/summary(lm.fit)$sigma
  attributes(res)$names <- NULL
  if(plot==T){
    plot(res)
    abline(h=c(-limit,limit), col="red", lty=2)
  }
  return(res)
}
par(mfrow=c(1,1))
student_resid(lm.fit, plot=T)

ind <- abs(student_resid(lm.fit))<=3; head(ind)
data_clean <- data[ind,]
student_resid(lm(quality~., data=data_clean), plot=T)

# Leverage
lm.fit <- lm(quality~., data=data_clean)
leveragePlots(lm.fit)                      # 레버리지가 높은 값 식별
# leverage 플롯에서 각 변수마다 가질 수 있는 범주를 크게 벗어난 값을 수작업 제외
data_clean <- data_clean[-c(1508, 3865, 2305, 3020, 3121, 480), ] 

par(mfrow=c(1,1))
lm.fit <- lm(quality~., data=data_clean)

# 레버리지 식별 함수
Leverage.High.index <- function(lm.fit, plot=F, limit=NULL){
  leverstat <- hatvalues(lm.fit)
  resid_stu <- student_resid(lm.fit, plot=F)
  line = (ncol(data_clean)-1)/nrow(data_clean)
  if(is.null(limit)) limit = line*4.5
  if(plot==T){
    plot(leverstat, resid_stu, xlab="Leverage", 
         ylab="studentized residual")
    abline(v=c(line, limit), col=c("red", "blue")) 
  }
  return(leverstat>=limit)
}
ind <- Leverage.High.index(lm.fit, plot=T, limit=0.015)
data_clean <- data_clean[!ind,] 

# Variance inflation factor(VIF)
vif(lm(quality~., data=data_clean)) # residual_sugar, density, alcohol
vif(lm(quality~.-density, data=data_clean))
data_clean <- data[,-8]

## linear regression
lm.fit <- lm(quality~., data=data_clean)
summary(lm.fit)
lm.fit_prod <- lm(quality~.*., data=data_clean)
summary(lm.fit_prod)



# Adjust R value by simple linear Regression
par(mfrow=c(1,1))
sapply(data[,1:11], function(x){
  lm.fit <- lm(quality~x, data=data)
  summary(lm.fit)$adj.r.squared
}) %>% barplot(xlab="variable", ylab="adj.r.squared", ylim=c(0,0.25), ann = F)

# plot quality vs another variables
par(mfrow=c(3,4))
lapply(data[,1:11], function(x){
  lm.fit <- lm(data$quality~x)
  plot(x,data$quality)
  abline(lm.fit, col="red", lwd=2)
  })
par(mfrow=c(1,1))

# with jitter
par(mfrow=c(3,4))
lapply(1:10, function(ind){           
  plot(data_clean[[ind]], jitter(data_clean$quality,amount = 1))}) 
par(mfrow=c(1,1)) 





# 분류 (Classification) : Logistic Regression vs LDA vs QDA

data <- read_csv("winequality-white.csv")
names(data) %<>% str_replace_all(" ", "_") # for convenient
barplot(round(table(data$quality)),2, col=c(2,2,4,4,4,3,3), axes=T)

ind3 <- data$quality<=4
ind2 <- data$quality>=5 & data$quality<=7
ind1 <- data$quality>=8

data$quality[ind3] <- "low"
data$quality[ind2] <- "middle"
data$quality[ind1] <- "high"
data$quality <- as.factor(data$quality)

classification_score <- function(formula, train, test, label){
  
  multinom.fit <- multinom(formula = formula, data=train)
  lda.fit <- lda(formula=formula, data=train)
  qda.fit <- qda(formula=formula, data=train)
  
  multinom.pred <- predict(multinom.fit, test, type="class")
  lda.pred <- predict(lda.fit, test)$class
  qda.pred <- predict(qda.fit, test)$class
  
  multinom.score = mean(multinom.pred == label)
  lda.score = mean(lda.pred == label)
  qda.score = mean(qda.pred == label)
  
  return(c("Logistic"=multinom.score, "LDA"=lda.score, "QDA"=qda.score))
}

# split train, test data [0 : test, 1 : train]
ind <- sample(c(0,1), size=nrow(data), replace = T, prob=c(3,7)) 
train = data[ind==1, ]
test = data[ind==0, ]
classification_score(formula=quality~., train=train, test=test, label=test$quality)

# 각 클래스별 공분산 행렬
round(cov(data[data$quality=="high",1:11]),2)
round(cov(data[data$quality=="middle",1:11]),2)
round(cov(data[data$quality=="low",1:11]),2)

# K-folds CV : 검정오차율 계산
CV_ErrorRate <- function(formula, data, K=5, labelname, plot=F){
  
  MSE <- matrix(0, nrow=K, ncol=3)
  data <- as.data.frame(data)
  colnames(MSE) <- c("Logistic", "LDA", "QDA")
  rownames(MSE) <- stringr::str_c("Fold[", 1:K, "]", sep="")
  ind <- cvFolds(n=nrow(data), K=K, R=1, type = "random")
  
  for(i in 1:K){
    train <- ind$subsets[ind$which!=i]
    test <- ind$subsets[ind$which==i]
    train <- data[train,]
    test <- data[test,]
    
    multinom.fit <- multinom(formula = formula, data=train)
    lda.fit <- lda(formula=formula, data=train)
    qda.fit <- qda(formula=formula, data=train)
    
    multinom.pred <- predict(multinom.fit, test, type="class")
    lda.pred <- predict(lda.fit, test)$class
    qda.pred <- predict(qda.fit, test)$class
    
    multinom.score = mean(multinom.pred != test[[labelname]])
    lda.score = mean(lda.pred != test[[labelname]])
    qda.score = mean(qda.pred != test[[labelname]])
    
    MSE[i,1] <- multinom.score
    MSE[i,2] <- lda.score
    MSE[i,3] <- qda.score
  }
  cat("\n\n")
  res <- colMeans(MSE)
  names(res) <- c("Logistic", "LDA", "QDA")
  
  if(plot==T){
    plot_data <- data.frame(melt(MSE))
    plot(plot_data %>% ggplot(mapping=aes(x=X1, y=value)) + 
           geom_line(aes(group=X2, color=X2), size=1.2)  +
           xlab(label="fold") + ylab(label="Error_rate"))
  }
  
  return(list("Error_rate"=res, "ER_fold"=MSE))
}

# Test of Data : Error Rate
CV_ErrorRate(formula=quality~., data=data, K=5, labelname="quality", plot=T)   # K=5
CV_ErrorRate(formula=quality~., data=data, K=10, labelname="quality", plot=T)  # K=10




# Tree-base classification
data <- read_csv("winequality-white.csv")
names(data) %<>% str_replace_all(" ", "_") # for convenient
tree.fit <- tree(quality~., data=data)
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty = 0)

ind3 <- data$quality<=4
ind2 <- data$quality>=5 & data$quality<=7
ind1 <- data$quality>=8

data$quality[ind3] <- "low"
data$quality[ind2] <- "middle"
data$quality[ind1] <- "high"
data$quality <- as.factor(data$quality)

classification_score <- function(formula, train, test, label){

  tree.fit <- tree(formula = formula ,data = train)
  bag.fit <- randomForest(formula = formula, data =train, mtry = ncol(train)-1)
  rf.fit <- randomForest(formula = formula, data =train, mtry = round(sqrt(ncol(train))))
  
  tree.pred <- predict(tree.fit ,test, type = 'class')
  bag.pred <- predict(bag.fit, test, type = 'class')
  rf.pred <- predict(rf.fit, test, type = 'class')
  
  tree.score = mean(tree.pred == label)
  bag.score = mean(bag.pred == label) 
  rf.score = mean(rf.pred == label)
  
  return(c("Tree" = tree.score,
           "Bag" = bag.score,
           "Randomfrest" = rf.score))
}

CV_ErrorRate <- function(formula, data, K=5, labelname, plot=F){
  
  MSE <- matrix(0, nrow=K, ncol=3)
  data <- as.data.frame(data)
  colnames(MSE) <- c("Tree","Bagging","Randomforest")
  rownames(MSE) <- stringr::str_c("Fold[", 1:K, "]", sep="")
  ind <- cvFolds(n=nrow(data), K=K, R=1, type = "random")
  
  for(i in 1:K){
    train <- ind$subsets[ind$which!=i]
    test <- ind$subsets[ind$which==i]
    train <- data[train,]
    test <- data[test,]
    
    tree.fit <- tree(formula = formula ,data = train)
    bag.fit <- randomForest(formula = formula ,data =train ,  mtry = ncol(train)-1)
    rf.fit = randomForest(formula = formula ,data =train ,  mtry = round(sqrt(ncol(train))))
    
    tree.pred <- predict(tree.fit ,test, type = 'class')
    bag.pred <- predict(bag.fit, test, type = 'class')
    rf.pred <- predict(rf.fit, test, type = 'class')
    
    tree.score = mean(tree.pred != test[[labelname]])
    bag.score = mean(bag.pred != test[[labelname]]) 
    rf.score = mean(rf.pred != test[[labelname]])
    
    MSE[i,1] <- tree.score
    MSE[i,2] <- bag.score
    MSE[i,3] <- rf.score
  }
  cat("\n\n")
  res <- colMeans(MSE)
  names(res) <- c("Tree","Bagging","Randomforest")
  
  if(plot==T){
    plot_data <- data.frame(melt(MSE))
    plot(plot_data %>% ggplot(mapping=aes(x=X1, y=value)) + 
           geom_line(aes(group=X2, color=X2), size=1.2)  +
           xlab(label="fold") + ylab(label="Error_rate"))
  }
  
  return(list("Error_rate"=res, "ER_fold"=MSE))
}

# Split train, test data (0:test, 1:train)

sapply(1:10, function(num){
  ind <- sample(c(0,1), size=nrow(data), replace = T, prob=c(3,7)) 
  train = data[ind==1, ]
  test = data[ind==0, ]
  classification_score(formula=quality~., train=train, test=test, label=test$quality)
})
table(data$quality)/nrow(data)
CV_ErrorRate(formula=quality~., data=data, K=10, labelname="quality", plot=T) 


cv.tree.fit = cv.tree(tree.fit,FUN = prune.misclass)
cv.tree.fit ##트리의 개수가 적어 줄일 필요가없어보인다.

###bagging
bag.fit = randomForest(formula = quality~. ,data =data ,  mtry = ncol(data)-1)
bag.pred <- predict(bag.fit, test, type = 'class')
mean(bag.pred == test$quality)

varImpPlot(bag.fit) ##변수의 중요도
plot(bag.fit)

rf.fit = randomForest(quality~. ,data =data ,  mtry = round(sqrt(ncol(data))))
varImpPlot(rf.fit)

###부스팅
data <- read_csv("winequality-white.csv")
names(data) %<>% str_replace_all(" ", "_") # for convenient
shrinkage = c(1,0.1,0.01,0.001)
interaction.depth = c(1,2,4)
RSS <- matrix(0, 4, 3)
for(i in 1:4){
  for(j in 1:3){
    total <- 0
    for(k in 1:5){
      ind <- sample(c(0,1), size=nrow(data), replace = T, prob=c(3,7)) 
      train = data[ind==1, ]
      test = data[ind==0, ]
      boost.fit = gbm(quality~.,data = train, distribution = "gaussian",
                      n.tree = 15000, shrinkage = shrinkage[i],
                      interaction.depth = interaction.depth[j])
      boost.pred <- predict(boost.fit, test, type = 'response')
      total <- total + sum(boost.pred-test$quality)^2
    }
    RSS[i,j] <-total/5 
  }
}
rownames(RSS) <- str_c("shrinkage ", shrinkage, sep="")
colnames(RSS) <- str_c("depth", interaction.depth, sep="")
RSS 


ind <- sample(c(0,1), size=nrow(data), replace = T, prob=c(2,8)) 
train = data[ind==1, ]
test = data[ind==0, ]
svm.fit <- svm(quality~., data = train, kernel = "radial",
               gamma=1, cost=10)
svm.pred <- predict(svm.fit, test)
mean(svm.pred == test$quality)
plot(svm.fit, test)
