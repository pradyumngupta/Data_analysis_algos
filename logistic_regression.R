install.packages('aod')
library(aod)
library(ggplot2)

df = read.csv('D:/sem 6/Data Analytics/Heart.csv')
View(df)

df$AHD =  ifelse(df$AHD == "Yes",1,0)
df$AHD

summary(df)

is.null(df)

library(caret)

split = sample.int(n=nrow(df), size=round(.75*(nrow(df))),replace=FALSE)
train_cl = df[split,]
test_cl = df[-split,]
dim(train_cl)
dim(test_cl)

mylogit = glm(AHD ~ 
                Age + Sex + ChestPain + 
                RestBP + Chol + Fbs + RestECG + MaxHR + 
                ExAng + Oldpeak + Slope + Ca + Thal
                , data = df, family = "binomial")


summary(mylogit)

confint(mylogit)

y_predict=predict(mylogit,newdata = test_cl)
y_predict


logit2prob = function(logit){
  odds = exp(logit)
  prob = odds / (1 + odds)
  return(prob)
}

y_predict = logit2prob(y_predict)
y_predict

y_predict = ifelse(y_predict>0.5 ,1 ,0 )
y_predict

table_mat=table(y_predict,test_cl$AHD)
table_mat

confusionMatrix(table_mat)


