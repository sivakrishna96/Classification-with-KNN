### Setting Directory
###sink("KNN_OUTPUT_File.txt")
setwd("C:\\Users\\vuddi\\Desktop\\DS Files\\Datasets")
getwd()

### Read Data
Data<-read.table("heart.dat")
head(Data)

### Adding Column names
colnames(Data)
names(Data)<-c("AGE", "SEX", "CHESTPAIN", "RESTBP", "CHOL", "SUGAR", "ECG",
               "MAXHR", "ANGINA", "DEP", "EXERCISE", "FLUOR", "THAL", "OUTPUT")

### Understand the data 
head(Data)
class(Data)
dim(Data)
str(Data)
summary(Data)

### Convert the output variable to 1 and 0
Data$OUTPUT<-Data$OUTPUT-1
Data$OUTPUT

### Scalling
normalize<-function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
normalize(c(10,20,30,40,50))
Data1<-as.data.frame(lapply(Data[1:13], normalize))
head(Data1)
summary(Data1)

### create training and test data
heart_train <- Data1[1:215, ]
names(heart_train)
heart_test <- Data1[216:270, ]

### create labels for training and test data
heart_train_labels <- Data[1:215, 14]
heart_train_labels
heart_test_labels <- Data[216:270, 14]

### Build a model on the data ----

#load the "class" library
library(class)

heart_model <- knn(train = heart_train, test = heart_test,
                      cl = heart_train_labels, k=15)
summary(heart_model)

# load the "gmodels" library
library(gmodels)
install.packages("gmodels")

# Create the confusion_matrix of predicted vs. actual
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))

# Precision
(precision <- confusion_matrix[2,2]/sum(confusion_matrix[2,]))

# Recall
(recall <- confusion_matrix[2,2]/sum(confusion_matrix[,2]))

# F-Score
(f = 2 * precision * recall / (precision + recall))

# Accuracy
mean(heart_test_labels == heart_model)



### try several different values of k 
heart_model <- knn(train = heart_train, test = heart_test,
                   cl = heart_train_labels, k=1)
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))
mean(heart_test_labels == heart_model)

heart_model <- knn(train = heart_train, test = heart_test,
                   cl = heart_train_labels, k=5)
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))
mean(heart_test_labels == heart_model)

heart_model <- knn(train = heart_train, test = heart_test,
                   cl = heart_train_labels, k=10)
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))
mean(heart_test_labels == heart_model)

heart_model <- knn(train = heart_train, test = heart_test,
                   cl = heart_train_labels, k=14)
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))
mean(heart_test_labels == heart_model)

heart_model <- knn(train = heart_train, test = heart_test,
                   cl = heart_train_labels, k=16)
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))
mean(heart_test_labels == heart_model)

heart_model <- knn(train = heart_train, test = heart_test,
                   cl = heart_train_labels, k=18)
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))
mean(heart_test_labels == heart_model)

heart_model <- knn(train = heart_train, test = heart_test,
                   cl = heart_train_labels, k=20)
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))
mean(heart_test_labels == heart_model)

heart_model <- knn(train = heart_train, test = heart_test,
                   cl = heart_train_labels, k=25)
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))
mean(heart_test_labels == heart_model)

heart_model <- knn(train = heart_train, test = heart_test,
                   cl = heart_train_labels, k=30)
(confusion_matrix <- table(predicted = heart_model, actual = heart_test_labels))
mean(heart_test_labels == heart_model)

#####################################
###sink()
