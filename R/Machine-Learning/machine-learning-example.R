# Load required library
library(caret)

# Load data, we are using the iris datasets from caret package
data(iris)
dataset <- iris
# view datasets, you can use either dataset or iris, they are identical
head(iris)

# Create a sample from original dataset, split sample data 80% for training model and validation 
sample_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
validation <- dataset[-sample_index,] # 20% of oroginal
# view validation datasets
tail(validation)
# 80% for training data
t_dataset <- dataset[sample_index,]
# view training datasets
head(t_dataset)

# EDA
# Dimension
dim(t_dataset)
# check type of attributes (also called predictors)
sapply(t_dataset, class)

# A class variable type is a factor which has multiple class labels or leverls. Here we have Species variable type is class.
levels(t_dataset$Species)

# Now we know in this dataset, we can refer to an attribute by name / labels as a property of the dataset.
# [1] "setosa"     "versicolor" "virginica"
# A typical multi-class or a multinomial classification problem.

# Class Distribution
percentage <- prop.table(table(t_dataset$Species)) * 100
# Create a dataset to have only our factor and distribution
cbind(freq=table(t_dataset$Species), percentage=percentage)

# View training dataset statistical summary
summary(t_dataset)

# Plots of individual variable
# set up x and y for input and output
x <- t_dataset[,1:4] # col 1-4
y <- t_dataset[,5]  # col 5 => Species

# boxplot for each attri (col 1-4)
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# barplot for class
plot(y, main="Barplot", las=1, horiz=TRUE)

# Scatter Plots to look at the interaction between the variables
featurePlot(x=x, y=y, plot="ellipse")
# You may need to run install.packages("ellipse") 

featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Build model with k-folder cross-validation, using trainControl function to generate params to control how model will be built
k<-10
control <- trainControl(method="cv", number=10)
# will use the metric of “Accuracy” to evaluate models
metric <- "Accuracy"

# Build models
# a) Linear Discriminant Analysis (LDA)
set.seed(7)
m_lda <- train(Species~., data=t_dataset, method="lda", metric=metric, trControl=control)
# Possible values are: ada, bag, bagEarth, bagFDA, blackboost, cforest, ctree, ctree2
# b) Classification and Regression Trees (CART)
set.seed(7)
m_cart <- train(Species~., data=t_dataset, method="rpart", metric=metric, trControl=control)
# c) KNN - nonlinear
set.seed(7)
m_knn <- train(Species~., data=t_dataset, method="knn", metric=metric, trControl=control)
# d) SVM with a linear kernel
set.seed(7)
m_svm <- train(Species~., data=t_dataset, method="svmRadial", metric=metric, trControl=control)
# e) Random Forest
set.seed(7)
m_rf <- train(Species~., data=t_dataset, method="rf", metric=metric, trControl=control)

# Estimate all 5 models performance by checking accuracy
accuracies <- resamples(list(lda=m_lda, cart=m_cart, knn=m_knn, svm=m_svm, rf=m_rf))
summary(accuracies)
# We are looking for accuracy more close to 1.0

# Plot them
dotplot(accuracies)
# LDA model is more accurate

# Check LDA model details
print(m_lda)
# str(m_lda) # can find accuracy SD
# AccuracySD: num 0.0322
# LDA model has 98% accuracy and accuracy +/- 3%

# Make prediction
# We built LDA model, now we should find out the accuracy of the model on our validation dataset

predictions <- predict(m_lda, validation)
confusionMatrix(predictions, validation$Species)

# The accuracy for validation dataset is 96.7%, close what it predicts on traing dataset which is 98%
# So, we can say that we have an accurate and a reliably accurate model.