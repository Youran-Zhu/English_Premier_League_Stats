
# title: EPL_Analysis
# author: Youran Zhu
# date: 5/20/2020"

rm(list=ls())
cat("\014")
graphics.off()

set.seed(1)

if(!require(pacman)) install.packages('pacman')
pacman::p_load(glmnet, dplyr, tidyr, ggplot2, readr, car, readxl, olsrr, lubridate, stringr, writexl, httr,mltools,data.table,randomForest,gridExtra,plotly)

epl = read.csv("epl2020.csv",stringsAsFactors=FALSE)


epl = epl %>% na.omit
dim(epl)
colnames(epl)


y = epl %>% select(scored)

#standardize numeric variables
epl_n = epl %>% select(-scored) %>% select_if(is.numeric)
n_numeric = dim(epl_n)[1]
X_numeric =   data.matrix(epl_n)
mu       =   as.vector(apply(X_numeric, 2, 'mean'))
sd       =   as.vector(apply(X_numeric, 2, 'sd'))

for (i in c(1:n_numeric)){
  X_numeric[i,]   =    (X_numeric[i,] - mu)/sd
}

#encode categorical variables
epl_c = epl %>% select(c('h_a', 'matchDay')) %>% 
  mutate(matchDay = replace(matchDay, !(matchDay %in% c("Sat","Sun")), "Weekday")) %>% droplevels()

epl_c$h_a <- as.factor(epl_c$h_a)
epl_c$matchDay <- as.factor(epl_c$matchDay)
epl_encode = epl_c%>% as.data.table() %>% one_hot()
colnames(epl_encode)

#combine numerical and categorical variables
X = cbind(X_numeric,epl_encode)

n        =    dim(X)[1]
p        =    dim(X)[2]

X        =   data.matrix(X)  
y        =   data.matrix(y) 


n.train        =     floor(0.8*n)
n.test         =     n-n.train

M              =     100
Rsq.test.rf    =     rep(0,M)  # rf= randomForest
Rsq.train.rf   =     rep(0,M)
Rsq.test.ri    =     rep(0,M)  #ri = ridge 
Rsq.train.ri   =     rep(0,M)
Rsq.test.en    =     rep(0,M)  #en = elastic net
Rsq.train.en   =     rep(0,M)
Rsq.test.la    =     rep(0,M)  #la = lasso
Rsq.train.la   =     rep(0,M)

for (m in c(1:M)) {
  shuffled_indexes =     sample(n)
  train            =     shuffled_indexes[1:n.train]
  test             =     shuffled_indexes[(1+n.train):n]
  X.train          =     X[train, ]
  y.train          =     y[train]
  X.test           =     X[test, ]
  y.test           =     y[test]
  
  # fit RF and calculate and record the train and test R squares 
  rf               =     randomForest(X.train, y.train, mtry = sqrt(p), importance = TRUE)
  y.test.hat       =     predict(rf, X.test)
  y.train.hat      =     predict(rf, X.train)
  Rsq.test.rf[m]   =     1-mean((y.test - y.test.hat)^2)/mean((y - mean(y))^2)
  Rsq.train.rf[m]  =     1-mean((y.train - y.train.hat)^2)/mean((y - mean(y))^2)
  resid.test.rf    =     y.test - y.test.hat
  resid.train.rf   =     y.train - y.train.hat
  
  # fit ridge and calculate and record the train and test R squares 
  a=0 # ridge
  cv.fit.ri        =     cv.glmnet(X.train, y.train, alpha = a, nfolds = 10)
  fit              =     glmnet(X.train, y.train, alpha = a, lambda = cv.fit.ri$lambda.min)
  y.train.hat      =     predict(fit, newx = X.train, type = "response") # y.train.hat=X.train %*% fit$beta + fit$a0
  y.test.hat       =     predict(fit, newx = X.test, type = "response") # y.test.hat=X.test %*% fit$beta  + fit$a0
  Rsq.test.ri[m]   =     1-mean((y.test - y.test.hat)^2)/mean((y - mean(y))^2)
  Rsq.train.ri[m]  =     1-mean((y.train - y.train.hat)^2)/mean((y - mean(y))^2)
  resid.test.ri    =     y.test - y.test.hat
  resid.train.ri   =     y.train - y.train.hat
  
  # fit elastic-net and calculate and record the train and test R squares 
  a=0.5 # elastic-net
  cv.fit.en        =     cv.glmnet(X.train, y.train, alpha = a, nfolds = 10)
  fit              =     glmnet(X.train, y.train, alpha = a, lambda = cv.fit.en$lambda.min)
  y.train.hat      =     predict(fit, newx = X.train, type = "response") # y.train.hat=X.train %*% fit$beta + fit$a0
  y.test.hat       =     predict(fit, newx = X.test, type = "response") # y.test.hat=X.test %*% fit$beta  + fit$a0
  Rsq.test.en[m]   =     1-mean((y.test - y.test.hat)^2)/mean((y - mean(y))^2)
  Rsq.train.en[m]  =     1-mean((y.train - y.train.hat)^2)/mean((y - mean(y))^2)
  resid.test.en    =     y.test - y.test.hat
  resid.train.en   =     y.train - y.train.hat
  
  # fit lasso and calculate and record the train and test R squares 
  a=1 # lasso
  cv.fit.la        =     cv.glmnet(X.train, y.train, alpha = a, nfolds = 10)
  fit              =     glmnet(X.train, y.train, alpha = a, lambda = cv.fit.la$lambda.min)
  y.train.hat      =     predict(fit, newx = X.train, type = "response") # y.train.hat=X.train %*% fit$beta + fit$a0
  y.test.hat       =     predict(fit, newx = X.test, type = "response") # y.test.hat=X.test %*% fit$beta  + fit$a0
  Rsq.test.la[m]   =     1-mean((y.test - y.test.hat)^2)/mean((y - mean(y))^2)
  Rsq.train.la[m]  =     1-mean((y.train - y.train.hat)^2)/mean((y - mean(y))^2)
  resid.test.la    =     y.test - y.test.hat
  resid.train.la   =     y.train - y.train.hat
  
  cat(sprintf("m=%3.f| Rsq.test.rf=%.3f, Rsq.test.ri=%.3f,  Rsq.test.en=%.3f, Rsq.test.la=%.3f| Rsq.train.rf=%.3f, Rsq.train.ri=%.3f,  Rsq.train.en=%.3f, Rsq.train.la=%.3f| \n", 
              m,  Rsq.test.rf[m], Rsq.test.ri[m], Rsq.test.en[m], Rsq.test.la[m], Rsq.train.rf[m],Rsq.train.ri[m], Rsq.train.en[m], Rsq.train.la[m]))
}


##Plot CV Curve
par(mfrow=c(2,2))
plot(cv.fit.ri,sub ='CV Curve for Ridge', font.sub = 2)
plot(cv.fit.en,sub ='CV Curve for ElasticNet',font.sub = 2)
plot(cv.fit.la,sub ='CV Curve for Lasso',font.sub = 2)

cv.fit.ri$lambda.min
cv.fit.en$lambda.min
cv.fit.la$lambda.min

#BoxPlot R-sq
plot_df = data.frame(Rsq.test.rf,Rsq.test.ri,Rsq.test.en,Rsq.test.la,Rsq.train.rf,Rsq.train.ri,Rsq.train.en,Rsq.train.la)

plot_data = plot_df %>%
  gather(variable, Rsq) %>%
  separate(variable, c("measure", "subset","algorithm"), sep = "\\.")

plot_data$algorithm = factor(plot_data$algorithm , levels = c( "rf", "ri", "en", "la"))

fig = plot_ly(plot_data, x = ~algorithm, y = ~Rsq, color = ~subset, type = "box") %>% layout(boxmode = "group",title="R-square Plot")
fig

##Plot Residuals
resid.test.ri = as.numeric(resid.test.ri)
resid.test.en = as.numeric(resid.test.en)
resid.test.la = as.numeric(resid.test.la)

resid.train.ri = as.numeric(resid.train.ri)
resid.train.en = as.numeric(resid.train.en)
resid.train.la = as.numeric(resid.train.la)

resid_test_df = data.frame(resid.test.rf,resid.test.ri,resid.test.en,resid.test.la)
resid_test_data = resid_test_df %>%
  gather(variable, Residual) %>%
  separate(variable, c("measure", "subset","algorithm"), sep = "\\.")

resid_train_df = data.frame(resid.train.rf,resid.train.ri,resid.train.en,resid.train.la)
resid_train_data = resid_train_df %>%
  gather(variable, Residual) %>%
  separate(variable, c("measure", "subset","algorithm"), sep = "\\.")

resid_data = rbind(resid_test_data,resid_train_data)
resid_data$algorithm = factor(resid_data$algorithm , levels = c( "rf", "ri", "en", "la"))

fig = plot_ly(resid_data, x = ~algorithm, y = ~Residual, color = ~subset, type = "box") %>% layout(boxmode = "group",title="Residual Plot")
fig

par(mfrow=c(2,2))
plot(resid.test.rf,sub = 'Random Forest Residual')
plot(resid.test.ri,sub = 'Ridge Residual')
plot(resid.test.en,sub = 'ElasticNet Residual')
plot(resid.test.la,sub = 'Lasso Residual')


##Bootstrap
bootstrapSamples =     100
beta.rf.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples) 
beta.ri.bs      =     matrix(0, nrow = p, ncol = bootstrapSamples)   
beta.en.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)         
beta.la.bs       =     matrix(0, nrow = p, ncol = bootstrapSamples)   

for (m in 1:bootstrapSamples){
  bs_indexes       =     sample(n, replace=T)
  X.bs             =     X[bs_indexes, ]
  y.bs             =     y[bs_indexes]
  
  # fit bs rf
  rf               =     randomForest(X.bs, y.bs, mtry = sqrt(p), importance = TRUE)
  beta.rf.bs[,m]   =     as.vector(rf$importance[,1])
  
  # fit bs ri
  a                =     0 # ridge
  cv.fit           =     cv.glmnet(X.bs, y.bs, alpha = a, nfolds = 10)
  fit              =     glmnet(X.bs, y.bs, alpha = a, lambda = cv.fit$lambda.min)  
  beta.ri.bs[,m]   =     as.vector(fit$beta)
  
  # fit bs en
  a                =     0.5 # elastic-net
  cv.fit           =     cv.glmnet(X.bs, y.bs, alpha = a, nfolds = 10)
  fit              =     glmnet(X.bs, y.bs, alpha = a, lambda = cv.fit$lambda.min)  
  beta.en.bs[,m]   =     as.vector(fit$beta)
  
  # fit bs la
  a                =     1 # lasso
  cv.fit           =     cv.glmnet(X.bs, y.bs, alpha = a, nfolds = 10)
  fit              =     glmnet(X.bs, y.bs, alpha = a, lambda = cv.fit$lambda.min)  
  beta.la.bs[,m]   =     as.vector(fit$beta)
  
  cat(sprintf("Bootstrap Sample %3.f \n", m))
}

# calculate bootstrapped standard errors / alternatively you could use qunatiles to find upper and lower bounds
rf.bs.sd    = apply(beta.rf.bs, 1, "sd")
ri.bs.sd    = apply(beta.ri.bs, 1, "sd")
en.bs.sd    = apply(beta.en.bs, 1, "sd")
la.bs.sd    = apply(beta.la.bs, 1, "sd")

# fit rf to the whole data
s = print(as.numeric(Sys.time()), digits=15)
rf               =     randomForest(X, y, mtry = sqrt(p), importance = TRUE)
e = print(as.numeric(Sys.time()), digits=15)
e-s

betaS.rf               =     data.frame(names(X[1,]), as.vector(rf$importance[,1]), 2*rf.bs.sd)
colnames(betaS.rf)     =     c( "feature", "value", "err")

# fit ri to the whole data
a=0 # ridge
#s = print(as.numeric(Sys.time()), digits=15) #timer
cv.fit           =     cv.glmnet(X, y, alpha = a, nfolds = 10)
fit              =     glmnet(X, y, alpha = a, lambda = cv.fit$lambda.min)
#e = print(as.numeric(Sys.time()), digits=15) #timer
#e-s #timer

betaS.ri               =     data.frame(names(X[1,]), as.vector(fit$beta), 2*ri.bs.sd)
colnames(betaS.ri)     =     c( "feature", "value", "err")


# fit en to the whole data
a=0.5 # elastic-net
#s = print(as.numeric(Sys.time()), digits=15) #timer
cv.fit           =     cv.glmnet(X, y, alpha = a, nfolds = 10)
fit              =     glmnet(X, y, alpha = a, lambda = cv.fit$lambda.min)
#e = print(as.numeric(Sys.time()), digits=15) #timer
#e-s #timer

betaS.en               =     data.frame(names(X[1,]), as.vector(fit$beta), 2*en.bs.sd)
colnames(betaS.en)     =     c( "feature", "value", "err")


# fit la to the whole data
a=1 # lasso
#s = print(as.numeric(Sys.time()), digits=15) #timer
cv.fit           =     cv.glmnet(X, y, alpha = a, nfolds = 10)
fit              =     glmnet(X, y, alpha = a, lambda = cv.fit$lambda.min)
#e = print(as.numeric(Sys.time()), digits=15) #timer
#e-s #timer

betaS.la               =     data.frame(names(X[1,]), as.vector(fit$beta), 2*la.bs.sd)
colnames(betaS.la)     =     c( "feature", "value", "err")

# we need to change the order of factor levels by specifying the order explicitly.
betaS.rf$feature     =  factor(betaS.rf$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])
betaS.ri$feature     =  factor(betaS.ri$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])
betaS.en$feature     =  factor(betaS.en$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])
betaS.la$feature     =  factor(betaS.la$feature, levels = betaS.rf$feature[order(betaS.rf$value, decreasing = TRUE)])


rfPlot =  ggplot(betaS.rf, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=.2) +
  ggtitle("Random Forest Feature Importance with Bootstrapped Error Bars") +
  theme(axis.text.x = element_text(angle = 25, size = 8, hjust = 1)) +
  theme(plot.title = element_text(vjust = -1, hjust = 0.5))

riPlot =  ggplot(betaS.ri, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=.2) +
  ggtitle("Ridge Regression Estimated Coeffcients with Bootstrapped Error Bars") +
  theme(axis.text.x = element_text(angle = 25, size = 8, hjust = 1)) +
  theme(plot.title = element_text(vjust = -1, hjust = 0.5))

enPlot =  ggplot(betaS.en, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=.2) +
  ggtitle("Elastic-Net Regression Estimated Coeffcients with Bootstrapped Error Bars") +
  theme(axis.text.x = element_text(angle = 25, size = 8, hjust = 1)) +
  theme(plot.title = element_text(vjust = -1, hjust = 0.5))

laPlot =  ggplot(betaS.la, aes(x=feature, y=value)) +
  geom_bar(stat = "identity", fill="white", colour="black")    +
  geom_errorbar(aes(ymin=value-err, ymax=value+err), width=.2) +
  ggtitle("Lasso Regression Estimated Coeffcients with Bootstrapped Error Bars") +
  theme(axis.text.x = element_text(angle = 25, size = 8,hjust = 1)) +
  theme(plot.title = element_text(vjust = -1, hjust = 0.5))

grid.arrange(rfPlot, riPlot, enPlot, laPlot, nrow = 4)














