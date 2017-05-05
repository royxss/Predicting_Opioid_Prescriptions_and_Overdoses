
######################### File Name : FeatureSelection_Modelling.R ##########################
## Description : This file applies various machine learning techniques for predictive 
##               analytics.
#############################################################################################

rm(list=ls())
setwd("C:\\Users\\SROY\\Documents\\CodeBase\\Datasets\\U.S.OpiatePrescriptionsOverdoses")
load("Data_Cleaning.RData")
seedVal = 18725

# Load libraries
library(caret)
library(cluster)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(doMC)
library(MASS)
library(rms)
library(e1071)
library(dummies)
library(glmnet)
library(h2o)
library(h2oEnsemble)
library(cvAUC)
library(SuperLearner)
library(wordcloud2)
library(gbm)
library(kknn)
library(klaR)
library(randomForest)
library(RRF)


# Use multiple cores for faster processing
registerDoMC(3)

# Model formula
createModelFormula <- function(yvar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(yvar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(yvar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}

# Records all model performances in a dataframe which will be 
# used later to plot
PerfMetrics <- data.frame(ModelName=NA, FitTime=NA, Accuracy=NA, Sensitivity=NA, Specificity=NA)

# Function to plot model performance
plotPerfMetrics <- function(){
  PerfMetrics <- PerfMetrics[! is.na(PerfMetrics$ModelName),]
  PerfMetrics$FitTime <- as.numeric(PerfMetrics$FitTime)
  
  plot1 <- ggplot(PerfMetrics, aes(x=ModelName, y=Accuracy, group=0)) +
    geom_line(color="red", size=1.2) + 
    theme(axis.text.x=element_text(angle=45,vjust = 1,hjust=1),axis.ticks.x = element_blank()) +
    geom_point()
  
  plot2 <- ggplot(PerfMetrics, aes(x=ModelName, y=FitTime, group=0)) +
    geom_line(color="blue", size=1.2) +
    theme(axis.text.x=element_text(angle=45,vjust = 1,hjust=1),axis.ticks.x = element_blank()) +
    geom_point()
  
  plot3 <- ggplot(PerfMetrics, aes(x=ModelName, y=Sensitivity, group=0)) +
    geom_line(color="green", size=1.2) +
    theme(axis.text.x=element_text(angle=45,vjust = 1,hjust=1),axis.ticks.x = element_blank()) +
    geom_point()
  
  plot4 <- ggplot(PerfMetrics, aes(x=ModelName, y=Specificity, group=0)) +
    geom_line(color="gray", size=1.2) +
    theme(axis.text.x=element_text(angle=45,vjust = 1,hjust=1),axis.ticks.x = element_blank()) +
    geom_point()
  
  grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
}

# Function to print confusion matrix
plotConfusionMatrix <- function(tbl,title){
  ConfusionTable <- as.data.frame(confusion$table)
  tile <- ggplot() +
    geom_tile(aes(x=Reference, y=Prediction,fill=Freq),data=ConfusionTable, color="black",size=0.1) +
    labs(x="Reference",y="Prediction", title=paste("Confusion Matrix for",title))
  tile = tile + 
    geom_text(aes(x=Reference,y=Prediction, label=sprintf("%d", Freq)),data=ConfusionTable, size=5, colour="white")
  tile = tile + 
    geom_tile(aes(x=Reference,y=Prediction),data=subset(ConfusionTable, as.character(Reference)==as.character(Prediction)), color="black",size=0.3, fill="black", alpha=0) 
  
  #render
  tile
}

# Function to plot variable importance
plotVariableImportance <- function(modelfit, title){
  importance <- as.data.frame(varImp(modelfit)[1])
  importance <- cbind(row.names(importance), Importance=importance)
  row.names(importance)<-NULL
  names(importance) <- c("Feature","Importance")
  importance %>% arrange(desc(Importance)) %>%
    mutate(Feature=factor(Feature,levels=as.character(Feature))) %>%
    slice(1:10) %>%
    ggplot() + geom_bar(aes(x=Feature,y=(Importance)),stat="identity",fill="steel blue") + 
    theme(axis.text.x=element_text(angle=45,vjust = 1,hjust=1),axis.ticks.x = element_blank()) +
    ylab("Importance") +
    ggtitle(paste("Top 10 Feature Importance for", title))
  
}

# Set columns as variables
yVar <- 'Opioid.Prescriber'
# Excluding Credentials as Specility column will handle it
exclude <- c('NPI', 'Credentials')
xVars <- names(Prescriber)[!names(Prescriber) %in% c(yVar, exclude)]

# Create new dataframe excluding unimportant columns
df <- Prescriber[,c(yVar, xVars)]

fullformula <- createModelFormula(yVar, xVars)
nullformula <- createModelFormula(yVar, 1)
xformula <- createModelFormula('', xVars)

# Feature Selection using lasso (done later) resulted only one feature 'Opioid_Claims'
# Therefore every model gave 100% accuracy and the feature selection
# Hence, this column was removed and rerun without it.
# Let's perform some analysis on this column

# Collinearity test between opioid_claims and opioid_prescriber
cor(as.numeric(as.character(Prescriber$Opioid.Prescriber)), Prescriber$Opioid_Claims)
# Collinearity is only 0.273. It doesn't make any sense why would it influence predictions.
# Further reasearch along with domain knowledge is required.

 
# Select features using fast backward
# This is very helpful for quick analysis of important features
ols.full <- ols(fullformula, data = df)             ; ptm <- proc.time()
bwmodel <- fastbw(ols.full, rule = "p", sls = 0.1)  ; FastBwFitTime <- proc.time() - ptm
bwmodel

# From "Approximate Estimates after Deleting Factors" we can see p values as 0
#                 Coef      S.E.       Wald Z      P
# Opioid_Claims   8.895e-03 3.647e-04  2.439e+01   0.000e+00
# We can clearly see how Opioid_Claims is influencing target

# Let's remove Opioid_Claims from data
exclude <- c(exclude, 'Opioid_Claims')
xVars <- names(Prescriber)[!names(Prescriber) %in% c(yVar, exclude)]

# Create new dataframe excluding unimportant columns
df <- Prescriber[,c(yVar, xVars)]

#  It is possible that there are drugs here that aren't prescribed in any of the rows 
# (i.e. the instances where they were prescribed got left behind). This would result in columns of all 0, 
# which is bad. Multicollinear features are already bad because they mean the feature matrix is rank-deficient 
# and, therefore, non-invertible.

df <- df[vapply(df,function(x) if (is.numeric(x)){sum(x)>0}else{TRUE},FUN.VALUE=TRUE)]


# Create new dataframe based on feature selected columns
df <- df[,c(yVar,xVars)]

# Set target as numeric as model matrix binarizes it. # Factor when target demands it.
df$Opioid.Prescriber <- as.factor(ifelse(df$Opioid.Prescriber==1,"yes","no"))
df <- droplevels(df)

# Use dummy to create one hot encoding. Worked better than model matrix.
# Useful for scoring things separately. Missing values are checked (they cause issues for dummy)
# Score for Specialty
df <- df[!is.na(df$Specialty),]
df <- cbind(df[,names(df)!="Specialty"],dummy(df$Specialty))

# Score for States
df <- cbind(df[names(df)!="State"],dummy(df$State))

# Score for Gender
df <- cbind(df[names(df)!="Gender"],dummy(df$Gender))


# Stratefied sampling using caret.
set.seed(seedVal)
trainPct <- .8
testPct <- 1 - trainPct
inTrain <- createDataPartition(y = df$Opioid.Prescriber, p = trainPct, list = FALSE)
Train <- df[inTrain,]
Test <- df[-inTrain,]
stopifnot(nrow(Train) + nrow(Test) == nrow(df))


# In many cases, using these models with built-in feature selection will be more efficient 
# than algorithms where the search routine for the right predictors is external to the model. 
# Built-in feature selection typically couples the predictor search algorithm with the parameter 
# estimation and are usually optimized with a single objective function (e.g. error rates or likelihood)


# Tuning Parameters for machine learning models
metric = "Accuracy"
tuneLength = 10
cvFolds = 5
cvRepeats = 1

########################### Apply Random Forest ##################################

trctrl <- trainControl(method = "cv", number = cvFolds)
set.seed(seedVal)

# Create a tune grid for boosting performance
mtry <- sqrt(length(xVars))
tunegrid <- expand.grid(.mtry=mtry)

ptm <- proc.time()
fitRF <- train(x = Train[,-1], y = Train[,1], method="rf", 
               tuneLength = tuneLength,
               trControl = trctrl, tuneGrid=tunegrid,
               metric = metric,
               allowParallel=TRUE)
RFFitTime <- proc.time() - ptm

# Get cv results
summary(fitRF)

# Plot accuracy based on repeatedcv
plot(fitRF)

# Plot variable importance
varImp(fitRF)
plotVariableImportance(fitRF, "Random Forest")

# Predict GBM on Test Data
predictTestRF <- predict(fitRF, Test[,-1], type = "raw")

# Measure performance
confusion <- confusionMatrix(predictTestRF, Test$Opioid.Prescriber, positive="yes")

# Plot confusion matrix
plotConfusionMatrix(confusion$table, "Random Forest")

# Record Performance
# as ModelName, FitTime, Accuracy, Sensitivity, Specificity

PerfMetrics <- rbind(PerfMetrics, c('Random Forest', 
                                    round(RFFitTime[3],4), 
                                    round(confusion$byClass["Balanced Accuracy"],4), 
                                    round(confusion$byClass["Sensitivity"],4),
                                    round(confusion$byClass["Specificity"],4)))

# Save state of progress.
save.image(file="saveStateTillRF.RData")

########################### Apply GLM ##################################

trctrl <- trainControl(method = "cv", number = cvFolds)
set.seed(seedVal)

ptm <- proc.time()
fitGLM <- train(x = Train[,-1], y = Train[,1], method="glm",                
                trControl=trctrl,
                family="binomial")
glmFitTime <- proc.time() - ptm
# glm.fit: algorithm did not converge
# glm.fit: fitted probabilities numerically 0 or 1 occurred

# Get cv results
fitGLM
summary(fitGLM)

# Plot variable importance
varImp(fitGLM)
plotVariableImportance(fitGLM, "GLM")

# Predict GLM on Test Data
predictTestGLM <- predict(fitGLM, Test[,-1], type = "raw")

# Measure performance
confusion <- confusionMatrix(predictTestGLM, Test$Opioid.Prescriber,positive="yes")

# Plot confusion matrix
plotConfusionMatrix(confusion$table, "GLM")

# Record Performance
# as ModelName, FitTime, Accuracy, Sensitivity, Specificity

PerfMetrics <- rbind(PerfMetrics, c('GLM',
                                    round(glmFitTime[3],4), 
                                    round(confusion$byClass["Balanced Accuracy"],4), 
                                    round(confusion$byClass["Sensitivity"],4),
                                    round(confusion$byClass["Specificity"],4)))

# Save state of progress.
save.image(file="saveStateTillGLM.RData")

########################### Apply Naive Bayes ##################################

# lets the training procedure figure out whether to user or not a kernel density estimate
# buts give warnings Numerical 0 probability for all classes with observation
set.seed(seedVal)

ptm <- proc.time()
fitNB <- naiveBayes(x = Train[,-1], y = Train[,1])
NBFitTime <- proc.time() - ptm

# Predict Naive Bayes on Test Data
predictTestNB <- predict(fitNB, newdata = Test[,-1], type = 'raw')
defpredictTestNB <- (predictTestNB[,'no'] <= predictTestNB[,'yes'])*1

# Measure performance
confusion <- confusionMatrix(defpredictTestNB, as.factor(ifelse(Test$Opioid.Prescriber=="yes",1,0)))

# Plot confusion matrix
plotConfusionMatrix(confusion$table, "Naive Bayes")

# Record Performance
# as ModelName, FitTime, Accuracy, Sensitivity, Specificity

PerfMetrics <- rbind(PerfMetrics, c('Naive Bayes',
                                    round(NBFitTime[3],4), 
                                    round(confusion$byClass["Balanced Accuracy"],4), 
                                    round(confusion$byClass["Sensitivity"],4),
                                    round(confusion$byClass["Specificity"],4)))

# Save state of progress.
save.image(file="saveStateTillNaiveBayes.RData")

########################### Apply Nearest Neighbour ##################################

trctrl <- trainControl(method = "cv", number = cvFolds)
set.seed(seedVal)

ptm <- proc.time()
fitKnn <- train(x = Train[,-1], y = Train[,1], method = "knn", 
                trControl=trctrl,
                preProcess = c("center", "scale"),
                metric = metric,
                tuneLength = tuneLength)
KNNFitTime <- proc.time() - ptm

# Warning in preProcess.default(thresh = 0.95, k = 5, freqCut = 19, uniqueCut = 10,  :
# These variables have zero variances: dfAddiction Medicine, dfClinical Psychologist, dfDenturist, 
# dfHospice and Palliative Care, dfLegal Medicine, dfPathology, dfPhysical Medicine & Rehabilitation

# Get cv results
fitKnn

# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was k = 23.

# Plot k Vs accuracy based on repeatedcv
plot(fitKnn, print.thres = 0.5, type="S")

# Plot variable importance
varImp(fitKnn)

# Predict KNN on Test Data
predictTestKnn <- predict(fitKnn, Test[,-1], type = "raw")

# Measure performance
confusion <- confusionMatrix(predictTestKnn, Test$Opioid.Prescriber, positive="yes")

# Plot confusion matrix
plotConfusionMatrix(confusion$table, "KNN")

# Record Performance
# as ModelName, FitTime, Accuracy, Sensitivity, Specificity

PerfMetrics <- rbind(PerfMetrics, c('Nearest Neighbour',
                                    round(KNNFitTime[3],4), 
                                    round(confusion$byClass["Balanced Accuracy"],4), 
                                    round(confusion$byClass["Sensitivity"],4),
                                    round(confusion$byClass["Specificity"],4)))

# Save state of progress.
save.image(file="saveStateTillKNN.RData")

########################### Apply Lasso GLM ##################################

set.seed(seedVal)

ptm <- proc.time()
fitLassoCV = cv.glmnet(x = as.matrix(Train[,-1]), 
                       y = as.numeric(ifelse(Train$Opioid.Prescriber=="yes",1,0)), 
                       alpha=1,
                       family ='binomial',
                       type.measure = "class")
LassoCVFitTime <- proc.time() - ptm

# Get cv results
fitLassoCV

# $lambda.min = 0.000601371
# $lambda.1se = 0.001153376

plot(fitLassoCV)

# Fit using best lambda
# try bigmemory
ptm <- proc.time()
LassoFit <- glmnet(x = as.matrix(Train[,-1]),
                   y = as.numeric(ifelse(Train$Opioid.Prescriber=="yes", 1, 0)),
                   alpha=1,
                   family='binomial',
                   lambda = fitLassoCV$lambda.min)
LassoFitTime <- proc.time() - ptm

# Plot variable importance
LassoFeat <- data.frame(coef.name = dimnames(coef(LassoFit))[[1]], coef.value = matrix(coef(LassoFit)))
LassoFeat <- LassoFeat[order(-LassoFeat$coef.value),]
LassoFeat$coef.value <- round(LassoFeat$coef.value,3)

# Plot feature importance
ggplot(data = LassoFeat[1:10,]) + geom_bar(aes(x=coef.name,y=coef.value),stat="identity",fill="steel blue") + 
  theme(axis.text.x=element_text(angle=45,vjust = 1,hjust=1),axis.ticks.x = element_blank()) +
  ylab("Coefficients") + xlab("Features") +
  ggtitle(paste("Top 10 Feature Importance for", "Lasso GLM"))

# Predict Lasso on Test Data
predictTestLasso <- predict(LassoFit, as.matrix(Test[,-1]), 
                            s=fitLassoCV$lambda.min,
                            type = "response")
defpredictTestLasso <- ifelse(predictTestLasso > 0.5,"yes","no")

# Measure performance
confusion <- confusionMatrix(defpredictTestLasso, Test$Opioid.Prescriber)

# Plot confusion matrix
plotConfusionMatrix(confusion$table, "Lasso GLM")

# Record Performance
# as ModelName, FitTime, Accuracy, Sensitivity, Specificity
PerfMetrics <- rbind(PerfMetrics, c('Lasso GLM',
                                    round(LassoFitTime[3],4), 
                                    round(confusion$byClass["Balanced Accuracy"],4), 
                                    round(confusion$byClass["Sensitivity"],4),
                                    round(confusion$byClass["Specificity"],4)))

# Save state of progress.
save.image(file="saveStateTillLassoGLM.RData")

########################### Apply Gradient Boost ##################################

trctrl <- trainControl(method = "cv", number = cvFolds,
                       returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
set.seed(seedVal)

ptm <- proc.time()
fitGBM <- train(x = Train[,-1], y = Train[,1], method="gbm",                
                trControl=trctrl)
gbmFitTime <- proc.time() - ptm

# Get cv results
fitGBM

# Plot variable importance
varImp(fitGBM)
plotVariableImportance(fitGBM, "Stochastic Gradient Boost")

# Predict GBM on Test Data
predictTestGBM <- predict(fitGBM, Test[,-1], type = "raw")

# Measure performance
confusion <- confusionMatrix(predictTestGBM, Test$Opioid.Prescriber, positive="yes")

# Plot confusion matrix
plotConfusionMatrix(confusion$table, "Stochastic Gradient Boost")

# Record Performance
# as ModelName, FitTime, Accuracy, Sensitivity, Specificity

PerfMetrics <- rbind(PerfMetrics, c('Stochastic Gradient Boost',
                                    round(gbmFitTime[3],4), 
                                    round(confusion$byClass["Balanced Accuracy"],4), 
                                    round(confusion$byClass["Sensitivity"],4),
                                    round(confusion$byClass["Specificity"],4)))

# Save state of progress.
save.image(file="saveStateTillSGB.RData")



########################### Ensemble using h2o ##################################

detach("package:doMC", unload = TRUE)
set.seed(seedVal)

h2o.init(nthreads = -1)
h2o.removeAll()

training_frame <- as.h2o(Train)
validation_frame <- as.h2o(Test)

family <- "binomial"

x <- setdiff(names(training_frame), yVar)
y <- yVar

training_frame[,y] <- as.factor(training_frame[,y])  
validation_frame[,y] <- as.factor(validation_frame[,y])

ptm <- proc.time()
# Create base models
glm1 <- h2o.glm(x = x, y = y, family = family, 
                training_frame = training_frame,
                nfolds = cvFolds,
                seed = seedVal,
                lambda = fitLassoCV$lambda.min,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

gbm1 <- h2o.gbm(x = x, y = y,
                training_frame = training_frame,
                seed = seedVal,
                ntrees = 150,
                max_depth = 3,
                nfolds = cvFolds,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

rf1 <- h2o.randomForest(x = x, y = y,
                        training_frame = training_frame,
                        seed = seedVal,
                        ntrees = 150,
                        nfolds = cvFolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)

nb1 <- h2o.naiveBayes(x = x, y = y,
                      training_frame = training_frame,
                      laplace = 3, 
                      seed = seedVal,
                      nfolds = cvFolds,
                      fold_assignment = "Modulo",
                      keep_cross_validation_predictions = TRUE)

models <- list(glm1, gbm1, rf1, nb1)
metalearner <- "h2o.glm.wrapper"

# Stack models
fitEnsbl <- h2o.stack(models = models,
                      response_frame = training_frame[,y],
                      metalearner = metalearner,
                      seed = seedVal,
                      keep_levelone_data = TRUE)
EsnblFitTime <- proc.time() - ptm
# Warning : Dropping constant columns: [dfPathology, dfHospice and Palliative Care, dfLegal Medicine, dfDenturist]

perfEnsblTest <- h2o.ensemble_performance(fitEnsbl, newdata = validation_frame)
perfEnsblTest

predictTestEnsbl <- predict(fitEnsbl, newdata = validation_frame)
outpredictTestEnsbl <- as.data.frame(predictTestEnsbl$pred)
defpredictTestEnsbl <- ifelse(outpredictTestEnsbl$yes > 0.5,"yes","no")

# Measure performance
confusion <- confusionMatrix(defpredictTestEnsbl, Test$Opioid.Prescriber, positive="yes")

# Plot confusion matrix
plotConfusionMatrix(confusion$table, "Ensemble")

# Record Performance
# as ModelName, FitTime, Accuracy, Sensitivity, Specificity

PerfMetrics <- rbind(PerfMetrics, c('Ensemble',
                                    round(EsnblFitTime[3],4),
                                    round(confusion$byClass["Balanced Accuracy"],4), 
                                    round(confusion$byClass["Sensitivity"],4),
                                    round(confusion$byClass["Specificity"],4)))
h2o.shutdown()

## Run performance plot
plotPerfMetrics()

save.image(file="saveStateTillEnsbl.RData")

################################### Save and Cleanup ###############################

# Save Image File
save.image(file="FeatureSelection_Modelling.R")

# Detach libraries
detach("package:caret", unload = TRUE)
detach("package:cluster", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
detach("package:gridExtra", unload = TRUE)
detach("package:plyr", unload = TRUE)
detach("package:dplyr", unload = TRUE)
detach("package:doMC", unload = TRUE)
detach("package:MASS", unload = TRUE)
detach("package:rms", unload = TRUE)
detach("package:e1071", unload = TRUE)
detach("package:dummies", unload = TRUE)
detach("package:glmnet", unload = TRUE)
detach("package:h2o", unload = TRUE)
detach("package:h2oEnsemble", unload = TRUE)
detach("package:cvAUC", unload = TRUE)
detach("package:SuperLearner", unload = TRUE)
detach("package:wordcloud2", unload = TRUE)
detach("package:gbm", unload = TRUE)
detach("package:kknn", unload = TRUE)
detach("package:klaR", unload = TRUE)
detach("package:randomForest", unload = TRUE)
detach("package:RRF", unload = TRUE)

# Remove all
rm(list=ls())