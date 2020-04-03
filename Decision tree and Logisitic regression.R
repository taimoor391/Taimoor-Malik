df <- sales.no.missing
head(df)

df$Insp <-relevel(df$Insp,"fraud")
levels(df$Insp)
#logistic model
model1 <- glm(Insp~Quant+Val, data=df, family= binomial())
summary(model1)
#chi squared
modelChi <- model1$null.deviance - model1$deviance
chidf <- model1$df.null -model1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
chisq.prob
#r square
r2 <- 1-model1$deviance/model1$null.deviance
r2

#Preding based on given sample point
newdata1 <- data.frame(Quant = 500, Val = 10000)

pred <- predict(model1, newdata = newdata1, type = "response")
pred

install.packages("rpart")
library(rpart)

# grow tree
fit <- rpart(Insp ~ Quant + Val, 
             method = "class", data=df)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# get classification err by getting confusion matrix

plot(fit, uniform=TRUE,
     main="Classification Tree for Insp Varaible")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
predict_unseen <-predict(fit, newdat1, type = 'class')

table_mat <- table(newdata1, predict_unseen)
table_mat

# logistic with polynomial term included
#TRy
#model2 <- glm(Insp ~ Quant + poly(Val, 3),data=df,family=binomial())
#summary(model2)

#model3 <- glm(Insp ~ Val+ poly(Quant, 3),data=df,family=binomial())
#summary(model3)

#predit <- predict(model2, newdata = newdata1, type = "response")
#predit
#predit1 <- predict(model3, newdata = newdata1, type = "response")
#predit1
#chi square for logistic model with polynomial term
#/modelChi <- model4$null.deviance - model4$deviance
#chidf <- model4$df.null -model4$df.residual
#chisq.prob <- 1 - pchisq(modelChi, chidf)
#chisq.prob
#r square
##r2 <- 1-model4$deviance/model4$null.deviance
##r2

# logistic with polynomial term included with forward step wise
model4 <- glm(Insp ~ poly(Val,3)+ poly(Quant, 3),data=df,family=binomial())
summary(model4)

model5=glm(Insp~Quant+Val+I(Quant^2)+I(Val^2),
           family=binomial,data=df)
summary(model5)
m.null=glm(Insp ~ 1,family=binomial,data=df)
forwards=step(m.null,
              scope=list(lower=formula(m.null),upper=formula(model5)), direction="forward")

summary(forwards)

# ROC and AUC for logistic regression with no polynomial term
m1.predict=predict(model1,type='response')
prd=prediction(m1.predict,df$Insp)

prf=performance(prd, measure = "tpr", x.measure = "fpr")
plot(prf)

auc=performance(prd, measure = "auc")
auc=auc@y.values[[1]]
auc

auc
#ROC and AUC fro model with polynomial term
m2.predict=predict(model5,type='response')
prd2=prediction(m2.predict,df$Insp)

prf2=performance(prd2, measure = "tpr", x.measure = "fpr")
plot(prf2)

auc=performance(prd2, measure = "auc")
auc=auc@y.values[[1]]
auc


#ROC and AUC for decison Tree

library("ROCR")
pred6 <- prediction(predict(fit, type = "prob")[, 2],df$Insp)
plot(performance(pred6, "tpr", "fpr"))
abline(0, 1, lty = 2)
plot(performance(pred6, "acc"))


auc=performance(pred6, measure = "auc")
auc=auc@y.values[[1]]
auc