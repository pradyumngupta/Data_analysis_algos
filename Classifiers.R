install.packages("caret") 
library(caret)

#Decision Tree Classifier
#installing packages
install.packages("party")

#load package
library(party)

#load dataset
df = data.frame(iris)
View(df)

#check for NULL values
is.null(df)

#training and testing data
sample = sample.int(n = nrow(df), size = round(.75*nrow(df)), replace = FALSE)
train_data = df[sample,]
test_data = df[-sample,]

dim(train_data)
dim(test_data)


#creating decision tree
decision_tree = ctree(Species ~ .,data = train_data)
summary(decision_tree)
#plot tree
plot(decision_tree)

#predicting
y_predict = predict(decision_tree, newdata = test_data)
y_predict

#accuracy
table_mat = table(y_predict,test_data$Species)
table_mat

confusionMatrix(table_mat)

###############################################

#Naive Bayes Classifier
install.packages("e1071")

#loading packages
library(e1071)

#load dataset
df = data.frame(iris)
View(df)

#check for NULL values
is.null(df)

#training and testing data
sample = sample.int(n = nrow(df), size = round(.75*nrow(df)), replace = FALSE)
train_data = df[sample,]
test_data = df[-sample,]

dim(train_data)
dim(test_data)

#creating model
nb = naiveBayes(Species~., data=train_data)
summary(nb)

# Predicting on test data' 
y_pred = predict(nb, newdata = test_data) 
y_pred

#accuracy
table_mat = table(y_pred,test_data$Species)
table_mat

confusionMatrix(table_mat)

#KNN Classifier
install.packages("class")
library(class)

df = data.frame(iris)

df$Species = as.numeric(as.factor(df$Species))
df$Species
#training and testing data
sample = sample.int(n = nrow(df), size = round(.70*nrow(df)), replace = FALSE)
train_data = df[sample,]
test_data = df[-sample,]

dim(train_data)
dim(test_data)

classifier_knn = knn(train = train_data, test = test_data, cl = train_data$Species, k = 11) 
classifier_knn 
summary(classifier_knn)

table_mat = table(test_data$Species, classifier_knn) 
table_mat

confusionMatrix(table_mat)

#SVM
library(e1071)

classifier_svm = svm(formula = Species ~ ., 
                 data = train_data, 
                 type = 'C-classification', 
                 kernel = 'linear') 
summary(classifier_svm)
y_pred = predict(classifier_svm, newdata = test_data)

table_mat = table(y_pred, test_data$Species) 
table_mat

confusionMatrix(table_mat)


#####################################################
######################################################
######################################################
#custom knn

df = data.frame(iris)

df$Species = as.numeric(as.factor(df$Species))
df$Species
#training and testing data
sample = sample.int(n = nrow(df), size = round(.70*nrow(df)), replace = FALSE)
train_data = df[sample,]
test_data = df[-sample,]

dim(train_data)
dim(test_data)

euclideanDist = function(a, b){
  d = 0
  for(i in c(1:(length(a)-1) ))
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

knn_predict = function(test_data, train_data, k_value){
  pred = c()  #empty pred vector 
  
  #LOOP-1 #looping over each record of test data
  for(i in c(1:nrow(test_data))){   
    eu_dist =c()          #eu_dist & eu_char empty  vector
    eu_char = c()
    class1 = 0              #variable initialization with 0 value
    class2 = 0
    class3 = 0
    
    #LOOP-2-looping over train data 
    for(j in c(1:nrow(train_data))){
      
      #adding euclidean distance b/w test data point and train data to eu_dist vector
      eu_dist = c(eu_dist, euclideanDist(test_data[i,], train_data[j,]))
    
      #adding class variable of training data in eu_char
      eu_char = c(eu_char, as.character(train_data[j,][[5]]))
      eu_char
    }
    
    eu = data.frame(eu_char, eu_dist) #eu dataframe created with eu_char & eu_dist columns
    print(eu)
    eu = eu[order(eu$eu_dist),]       #sorting eu dataframe to gettop K neighbors
    eu = eu[1:k_value,]               #eu dataframe with top K neighbors
    
    #Loop 3: loops over eu and counts classes of neibhors.
    for(k in c(1:nrow(eu))){
      if((eu[k,"eu_char"]) == 1){
      class1 = class1 + 1
      }
      else if((eu[k,"eu_char"]) == 2){
        class2 = class2 + 1
      }
      else
        class3 = class3 + 1
    }
  
    if(class1 >class2 && class1 > class3){
      pred = c(pred,1)
    }
    else if(class2> class3){
      pred = c(pred,2)
    }
    else{
      pred = c(pred,3)
    }
    
  }
  return(pred) #return pred vector
}

accuracy = function(test_data){
  correct = 0
  for(i in c(1:nrow(test_data))){
    if(test_data[i,5] == test_data[i,6]){ 
      correct = correct+1
    }
  }
  accu = correct/nrow(test_data) * 100  
  return(accu)
}

K = 11
predictions = knn_predict(test_data, train_data, K) #calling knn_predict()
predictions

test_data[,6] = predictions #Adding predictions in test data as 6th column
print(accuracy(test_data))