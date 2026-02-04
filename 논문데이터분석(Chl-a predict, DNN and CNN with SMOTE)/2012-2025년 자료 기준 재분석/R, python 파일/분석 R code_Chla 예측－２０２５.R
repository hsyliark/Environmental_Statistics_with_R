## Loading data

SC <- read.csv("C:/Users/User/Desktop/seungchon_2012-2025.csv", sep=",", header=T, fileEncoding="CP949")
JS <- read.csv("C:/Users/User/Desktop/juksan_2012-2025.csv", sep=",", header=T, fileEncoding="CP949")

## Divide train and test

SC_train <- SC[SC$Year != 2025,]
SC_test <- SC[SC$Year == 2025,]

JS_train <- JS[JS$Year != 2025,]
JS_test <- JS[JS$Year == 2025,]


## Exploratory Data Analysis

# Descriptive Statistics
library(psych)
describe(SC_train[,5:23], trim = 0.05) 
describe(JS_train[,5:23], trim = 0.05) 

# Normality test for time-series data 
# (Jarque-Bera test / H0 : Normality)
library(tseries)
jarque.bera.test(SC_train$TD)
jarque.bera.test(JS_train$TD)

# Normality test for time-series data 
# (Lobato and Velasco's test / H0 : The given data follows a Gaussian process)
library(nortsTest)
lobato.test(SC_train$TD)
lobato.test(JS_train$TD)

# Boxplot with Density curve
library(ggplot2)
require(reshape2)
# Seungchon weir
melt.SC <- melt(SC_train[,5:23])
ggplot(data=melt.SC, aes(x=value)) + 
  geom_boxplot(aes(y=-0.5, col=variable)) +
  stat_density(aes(x=value, fill=variable), inherit.aes=F) +
  facet_wrap(~variable, scales = "free") +
  xlab("Value of measured variables") + ylab("Density") +
  ggtitle("Boxplot with Density curve (Seungchon weir)")
# Juksan weir
melt.JS <- melt(JS_train[,5:23])
ggplot(data=melt.JS, aes(x=value)) + 
  geom_boxplot(aes(y=-0.5, col=variable)) +
  stat_density(aes(x=value, fill=variable), inherit.aes=F) +
  facet_wrap(~variable, scales = "free") +
  xlab("Value of measured variables") + ylab("Density") +
  ggtitle("Boxplot with Density curve (Juksan weir)")

# Correlation analysis
library(ggcorrplot)
# Seungchon weir
cor_SC <- round(cor(SC_train[,5:23], method="spearman"), 2)
p_SC <- cor_pmat(SC_train[,5:23], method="spearman")
ggcorrplot(cor_SC, type="lower", p.mat=p_SC, lab=T,
           outline.color="white",
           legend.title="Spearman",
           title="Correlation Analysis (Seungchon weir)")
library(ggstatsplot)
ggstatsplot::ggcorrmat(
  data = SC_train[,5:23],
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)
# Juksan weir
cor_JS <- round(cor(JS_train[,5:23], method="spearman"), 2)
p_JS <- cor_pmat(JS_train[,5:23], method="spearman")
ggcorrplot(cor_JS, type="lower", p.mat=p_JS, lab=T, 
           outline.color="white",
           legend.title="Spearman",
           title="Correlation Analysis (Juksan weir)")
library(ggstatsplot)
ggstatsplot::ggcorrmat(
  data = JS_train[,5:23],
  type = "nonparametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

# Finding best probability distribution for estimate Chl-a
library(fitdistrplus)
# Seungchon weir
descdist(SC_train$Chla, boot=1000)
fw_SC <- fitdist(SC_train$Chla, "weibull")
fg_SC <- fitdist(SC_train$Chla, "gamma")
fe_SC <- fitdist(SC_train$Chla, "exp")
gofstat(list(fw_SC, fg_SC, fe_SC))
denscomp(list(fw_SC, fg_SC, fe_SC), legendtext = c("Weibull", "Gamma", "Exponential"))
# Juksan weir
descdist(JS_train$Chla, boot=1000)
fw_JS <- fitdist(JS_train$Chla, "weibull")
fg_JS <- fitdist(JS_train$Chla, "gamma")
fe_JS <- fitdist(JS_train$Chla, "exp")
gofstat(list(fw_JS, fg_JS, fe_JS))
denscomp(list(fw_JS, fg_JS, fe_JS), legendtext = c("Weibull", "Gamma", "Exponential"))

## Statistical Models

# Generalized Linear Model (GLM) (Generalized Gamma Linear Model (GGLM))
# Seungchon weir
glm.SC <- glm(Chla ~ BOD + COD + TN + TP + TOC + NH3N + NO3N + PO4P +
                 SS + pH + DO + EC + WT + WL + WS + RF + TI + TD, 
               data=SC_train[,6:24], family=Gamma(link=log)) 
summary(glm.SC)
step.glm.SC <- step(glm.SC, direction="both") # stepwise regression
summary(step.glm.SC)
library(car)
vif(step.glm.SC)
GGLM_SC <- predict(step.glm.SC, newdata=SC_test[,6:24], type="response")
GGLM_SC
# Juksan weir
glm.JS <- glm(Chla ~ BOD + COD + TN + TP + TOC + NH3N + NO3N + PO4P +
                SS + pH + DO + EC + WT + WL + WS + RF + TI + TD, 
              data=JS_train[,6:24], family=Gamma(link=log)) 
summary(glm.JS)
step.glm.JS <- step(glm.JS, direction="both") # stepwise regression
summary(step.glm.JS)
library(car)
vif(step.glm.JS)
GGLM_JS <- predict(step.glm.JS, newdata=JS_test[,6:24], type="response")
GGLM_JS

# Generalized Additive Model (GAM) (Generalized Gamma Additive Model (GGAM)) 
library(mgcv)
library(gam)
# Seungchon weir
gam.SC <- gam::gam(Chla ~ s(BOD) + s(COD) + s(TN) + s(TP) + s(TOC) +
                     s(NH3N) + s(NO3N) + s(PO4P) + s(SS) + s(pH) +
                     s(DO) + s(EC) + s(WT) + s(WL) + s(WS) + s(RF) +
                     s(TI) + s(TD),
                   data=SC_train[,6:24], family=Gamma(link=log))
summary(gam.SC)
step.gam.SC <- step.Gam(gam.SC, direction="both", 
                        scope=gam.scope(SC_train[,6:24])) # stepwise regression in GAM
summary(step.gam.SC)
vif(step.gam.SC)
GGAM_SC <- predict(step.gam.SC, newdata=SC_test[,6:24], type="response")
GGAM_SC
# Juksan weir
gam.JS <- gam::gam(Chla ~ s(BOD) + s(COD) + s(TN) + s(TP) + s(TOC) +
                     s(NH3N) + s(NO3N) + s(PO4P) + s(SS) + s(pH) +
                     s(DO) + s(EC) + s(WT) + s(WL) + s(WS) + s(RF) +
                     s(TI) + s(TD),
                   data=JS_train[,6:24], family=Gamma(link=log))
summary(gam.JS)
step.gam.JS <- step.Gam(gam.JS, direction="both", 
                        scope=gam.scope(JS_train[,6:24])) # stepwise regression in GAM
summary(step.gam.JS)
vif(step.gam.JS)
GGAM_JS <- predict(step.gam.JS, newdata=JS_test[,6:24], type="response")
GGAM_JS

# Time Varying Coefficient Model (TVCM) (Time Varying Coefficient Model with Gamma distribution (TVCMG))
# Seungchon weir
tvcm.SC <- mgcv::gam(Chla ~ s(Time,by=BOD) + s(Time,by=COD) + s(Time,by=TN) + 
                      s(Time,by=TP) + s(Time,by=TOC) + s(Time,by=NH3N) + 
                      s(Time,by=NO3N) + s(Time,by=PO4P) + s(Time,by=SS) + 
                      s(Time,by=pH) + s(Time,by=DO) + s(Time,by=EC) + 
                      s(Time,by=WT) + s(Time,by=WL) + s(Time,by=WS) + 
                      s(Time,by=RF) + s(Time,by=TI) + s(Time,by=TD),
                    data=SC_train[,5:24], family=Gamma(link=log))
summary(tvcm.SC)
auto.tvcm.SC <- mgcv::gam(Chla ~ s(Time,by=BOD) + s(Time,by=COD) + s(Time,by=TN) + 
                            s(Time,by=TP) + s(Time,by=TOC) + s(Time,by=NH3N) + 
                            s(Time,by=NO3N) + s(Time,by=PO4P) + s(Time,by=SS) + 
                            s(Time,by=pH) + s(Time,by=DO) + s(Time,by=EC) + 
                            s(Time,by=WT) + s(Time,by=WL) + s(Time,by=WS) + 
                            s(Time,by=RF) + s(Time,by=TI) + s(Time,by=TD),
                          data=SC_train[,5:24], family=Gamma(link=log),
                          select=T, method="REML") # automatic model selection in TVCM with Restricted Maximum Likelihood method
summary(auto.tvcm.SC)
vif(auto.tvcm.SC)
TVCMG_SC <- predict(auto.tvcm.SC, newdata=SC_test[,5:24], type="response")
TVCMG_SC 
# Seungchon weir
tvcm.JS <- mgcv::gam(Chla ~ s(Time,by=BOD) + s(Time,by=COD) + s(Time,by=TN) + 
                       s(Time,by=TP) + s(Time,by=TOC) + s(Time,by=NH3N) + 
                       s(Time,by=NO3N) + s(Time,by=PO4P) + s(Time,by=SS) + 
                       s(Time,by=pH) + s(Time,by=DO) + s(Time,by=EC) + 
                       s(Time,by=WT) + s(Time,by=WL) + s(Time,by=WS) + 
                       s(Time,by=RF) + s(Time,by=TI) + s(Time,by=TD),
                     data=JS_train[,5:24], family=Gamma(link=log))
summary(tvcm.JS)
auto.tvcm.JS <- mgcv::gam(Chla ~ s(Time,by=BOD) + s(Time,by=COD) + s(Time,by=TN) + 
                            s(Time,by=TP) + s(Time,by=TOC) + s(Time,by=NH3N) + 
                            s(Time,by=NO3N) + s(Time,by=PO4P) + s(Time,by=SS) + 
                            s(Time,by=pH) + s(Time,by=DO) + s(Time,by=EC) + 
                            s(Time,by=WT) + s(Time,by=WL) + s(Time,by=WS) + 
                            s(Time,by=RF) + s(Time,by=TI) + s(Time,by=TD),
                          data=JS_train[,5:24], family=Gamma(link=log),
                          select=T, method="REML") # automatic model selection in TVCM with Restricted Maximum Likelihood method
summary(auto.tvcm.JS)
vif(auto.tvcm.JS)
TVCMG_JS <- predict(auto.tvcm.JS, newdata=JS_test[,5:24], type="response")
TVCMG_JS 


## Machine Learning Algorithms

# Extreme Gradient Boosting (XGBoost)
library(xgboost)
library(caret)
# Seungchon weir
train_x_SC <- data.matrix(SC_train[,c(6:17,19:24)]) # define predictor and response variables in training set
train_y_SC <- SC_train[,18]
test_x_SC <- data.matrix(SC_test[,c(6:17,19:24)]) # define predictor and response variables in testing set
test_y_SC <- SC_test[,18]
xgb_train_SC <- xgb.DMatrix(data=train_x_SC, label=train_y_SC) # define final training and testing sets
xgb_test_SC <- xgb.DMatrix(data=test_x_SC, label=test_y_SC)
watchlist_SC <- list(train=xgb_train_SC, test=xgb_test_SC) # defining a watchlist
xgboost_SC <- xgb.train(data=xgb_train_SC, max.depth=3, 
                        watchlist=watchlist_SC, nrounds=100) # fit XGBoost model and display training and testing data at each iteartion
summary(xgboost_SC)
importance_matrix_SC <- xgb.importance(colnames(xgb_train_SC), model=xgboost_SC)
importance_matrix_SC
library(Ckmeans.1d.dp)
xgb.ggplot.importance(importance_matrix_SC, xlab="Relative importance")
XGBoost_pred_SC <- predict(xgboost_SC, xgb_test_SC)
XGBoost_pred_SC
# Juksan weir
train_x_JS <- data.matrix(JS_train[,c(6:17,19:24)]) # define predictor and response variables in training set
train_y_JS <- JS_train[,18]
test_x_JS <- data.matrix(JS_test[,c(6:17,19:24)]) # define predictor and response variables in testing set
test_y_JS <- JS_test[,18]
xgb_train_JS <- xgb.DMatrix(data=train_x_JS, label=train_y_JS) # define final training and testing sets
xgb_test_JS <- xgb.DMatrix(data=test_x_JS, label=test_y_JS)
watchlist_JS <- list(train=xgb_train_JS, test=xgb_test_JS) # defining a watchlist
xgboost_JS <- xgb.train(data=xgb_train_JS, max.depth=3, 
                        watchlist=watchlist_JS, nrounds=100) # fit XGBoost model and display training and testing data at each iteartion
summary(xgboost_JS)
importance_matrix_JS <- xgb.importance(colnames(xgb_train_JS), model=xgboost_JS)
importance_matrix_JS
library(Ckmeans.1d.dp)
xgb.ggplot.importance(importance_matrix_JS, xlab="Relative importance")
XGBoost_pred_JS <- predict(xgboost_JS, xgb_test_JS)
XGBoost_pred_JS

# Support Vector Machine (SVM)
library(e1071)
# Seungchon weir
svm.SC <- svm(Chla ~ BOD + COD + TN + TP + TOC + NH3N + NO3N + PO4P +
                SS + pH + DO + EC + WT + WL + WS + RF + TI + TD, 
              data=SC_train[,6:24]) 
summary(svm.SC)
best_svm.SC <- tune.svm(Chla ~ BOD + COD + TN + TP + TOC + NH3N + NO3N + PO4P +
                     SS + pH + DO + EC + WT + WL + WS + RF + TI + TD, 
                   data=SC_train[,6:24], gamma=seq(0,1,0.1), cost=c(1,10,1), 
                   epsilon=c(0,1,0.1)) 
summary(best_svm.SC)
svm_pred_SC <- predict(object=best_svm.SC$best.model, newdata=SC_test[,6:24])
svm_pred_SC
# Juksan weir
svm.JS <- svm(Chla ~ BOD + COD + TN + TP + TOC + NH3N + NO3N + PO4P +
                SS + pH + DO + EC + WT + WL + WS + RF + TI + TD, 
              data=JS_train[,6:24]) 
summary(svm.JS)
best_svm.JS <- tune.svm(Chla ~ BOD + COD + TN + TP + TOC + NH3N + NO3N + PO4P +
                          SS + pH + DO + EC + WT + WL + WS + RF + TI + TD, 
                        data=JS_train[,6:24], gamma=seq(0,1,0.1), cost=c(1,10,1), 
                        epsilon=c(0,1,0.1)) 
summary(best_svm.JS)
svm_pred_JS <- predict(object=best_svm.JS$best.model, newdata=JS_test[,6:24])
svm_pred_JS

## Deep Learning Algorithm

# Deep Neural Network (DNN)
library(keras)
library(dplyr)
library(ggplot2)
reticulate::py_install("pydot", pip = TRUE)
reticulate::conda_install(packages = "graphviz")
# Seungchon weir
train_x_SC <- data.matrix(SC_train[,c(6:17,19:24)]) # define predictor and response variables in training set
train_y_SC <- SC_train[,18]
test_x_SC <- data.matrix(SC_test[,c(6:17,19:24)]) # define predictor and response variables in testing set
test_y_SC <- SC_test[,18]
dnn_SC <- keras_model_sequential() %>% 
  layer_dense(units=64, activation="relu", input_shape=18) %>%
  layer_dense(units=32, activation="relu") %>%
  layer_dense(units=16, activation="relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units=1, activation="linear")
dnn_SC %>% compile(
  loss="mse",
  optimizer="adam", 
  metrics=list("mean_squared_error","mean_absolute_error"))
dnn_SC %>% summary()
dnn_SC %>% fit(train_x_SC, train_y_SC, epochs=100, validation_split=0.2, verbose=1)
dnn_pred_SC <- dnn_SC %>% predict(test_x_SC)
dnn_pred_SC
# Juksan weir
train_x_JS <- data.matrix(JS_train[,c(6:17,19:24)]) # define predictor and response variables in training set
train_y_JS <- JS_train[,18]
test_x_JS <- data.matrix(JS_test[,c(6:17,19:24)]) # define predictor and response variables in testing set
test_y_JS <- JS_test[,18]
dnn_JS <- keras_model_sequential() %>% 
  layer_dense(units=64, activation="relu", input_shape=18) %>%
  layer_dense(units=32, activation="relu") %>%
  layer_dense(units=16, activation="relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units=1, activation="linear")
dnn_JS %>% compile(
  loss="mse",
  optimizer="adam", 
  metrics=list("mean_squared_error","mean_absolute_error"))
dnn_JS %>% summary()
dnn_JS %>% fit(train_x_JS, train_y_JS, epochs=100, validation_split=0.2, verbose=1)
dnn_pred_JS <- dnn_JS %>% predict(test_x_JS)
dnn_pred_JS

# Convolutional Neural Network (CNN)
library(keras)
library(caret)
# Seungchon weir
train_x_SC <- data.matrix(SC_train[,c(6:17,19:24)]) # define predictor and response variables in training set
train_y_SC <- SC_train[,18]
test_x_SC <- data.matrix(SC_test[,c(6:17,19:24)]) # define predictor and response variables in testing set
test_y_SC <- SC_test[,18]
dim(train_x_SC)
train_x_SC <- array(train_x_SC, dim=c(nrow(train_x_SC), 18, 1))
test_x_SC <- array(test_x_SC, dim=c(nrow(test_x_SC), 18, 1))
in_dim_SC <- c(dim(train_x_SC)[2:3])
cnn_SC <- keras_model_sequential() %>%
  layer_conv_1d(filters=64, kernel_size=2,
                input_shape=in_dim_SC, activation="relu") %>%
  layer_conv_1d(filters=32, kernel_size=3, activation='relu') %>%
  layer_max_pooling_1d(pool_size=2) %>%
  layer_dropout(0.2) %>%
  layer_flatten() %>%
  layer_dense(units=32, activation="relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units=1, activation="linear")
cnn_SC %>% compile(
  loss="mse",
  optimizer="adam",
  metrics=list("mean_squared_error","mean_absolute_error"))
cnn_SC %>% summary()
cnn_SC %>% fit(train_x_SC, train_y_SC, epochs=100, batch_size=16, validation_split=0.2, verbose=1)
cnn_pred_SC <- cnn_SC %>% predict(test_x_SC)
cnn_pred_SC
# Juksan weir
train_x_JS <- data.matrix(JS_train[,c(6:17,19:24)]) # define predictor and response variables in training set
train_y_JS <- JS_train[,18]
test_x_JS <- data.matrix(JS_test[,c(6:17,19:24)]) # define predictor and response variables in testing set
test_y_JS <- JS_test[,18]
dim(train_x_JS)
train_x_JS <- array(train_x_JS, dim=c(nrow(train_x_JS), 18, 1))
test_x_JS <- array(test_x_JS, dim=c(nrow(test_x_JS), 18, 1))
in_dim_JS <- c(dim(train_x_JS)[2:3])
cnn_JS <- keras_model_sequential() %>%
  layer_conv_1d(filters=64, kernel_size=2,
                input_shape=in_dim_JS, activation="relu") %>%
  layer_conv_1d(filters=32, kernel_size=3, activation='relu') %>%
  layer_max_pooling_1d(pool_size=2) %>%
  layer_dropout(0.2) %>%
  layer_flatten() %>%
  layer_dense(units=32, activation="relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units=1, activation="linear")
cnn_JS %>% compile(
  loss="mse",
  optimizer="adam",
  metrics=list("mean_squared_error","mean_absolute_error"))
cnn_JS %>% summary()
cnn_JS %>% fit(train_x_JS, train_y_JS, epochs=100, batch_size=16, validation_split=0.2, verbose=1)
cnn_pred_JS <- cnn_JS %>% predict(test_x_JS)
cnn_pred_JS

## Comparing prediction power
res_SC <- data.frame(Chla=SC_test$Chla, GLM=GGLM_SC, GAM=GGAM_SC,
                     TVCM=TVCMG_SC, XGBoost=XGBoost_pred_SC,
                     SVM=svm_pred_SC, DNN=dnn_pred_SC,
                     CNN=cnn_pred_SC)
res_JS <- data.frame(Chla=JS_test$Chla, GLM=GGLM_JS, GAM=GGAM_JS,
                     TVCM=TVCMG_JS, XGBoost=XGBoost_pred_JS,
                     SVM=svm_pred_JS, DNN=dnn_pred_JS,
                     CNN=cnn_pred_JS)
RMSE <- function(y,yhat) {sqrt(sum((y-yhat)^2)/length(y))}
MAE <- function(y,yhat) {sum(abs(y-yhat))/length(y)}
MAPE <- function(y,yhat) {(100/length(y))*sum(abs((y-yhat)/y))}
RMSE(res_SC$a_Chla, res_SC$b_GLM)
write.csv(res_SC, file="C:/Users/User/Desktop/res_SC.csv")
write.csv(res_JS, file="C:/Users/User/Desktop/res_JS.csv")

# with python

res_SC <- read.csv("C:/Users/User/Desktop/res_SC.csv", sep=",", header=T)
res_JS <- read.csv("C:/Users/User/Desktop/res_JS.csv", sep=",", header=T)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)

res_SC_melt <- res_SC %>%
  gather(key="variable", value="value", -date)
ggplot(res_SC_melt, aes(x = ymd(date), y = value, group = variable)) +
  theme(axis.text.x = element_text(angle=90)) +
  geom_line(aes(color = variable), size = 1) +
  ggtitle("Result(Seungchon weir)") +
  xlab("date") + ylab("Chlorophyll-a(Observed vs Predicted)") +
  theme_minimal()

res_JS_melt <- res_JS %>%
  gather(key="variable", value="value", -date)
ggplot(res_JS_melt, aes(x = ymd(date), y = value, group = variable)) +
  theme(axis.text.x = element_text(angle=90)) +
  geom_line(aes(color = variable), size = 1) +
  ggtitle("Result(Juksan weir)") +
  xlab("date") + ylab("Chlorophyll-a(Observed vs Predicted)") +
  theme_minimal()
