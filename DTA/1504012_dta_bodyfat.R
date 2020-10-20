
#------------------- Decision Tree Algorithm for bodyfat dataset ------------------------

# install required packages

install.packages("caret")
install.packages("rpart.plot")

# use those packages
library(ggplot2)
library(lattice)
library(caret)
library(rpart.plot)

# Package rpart [Therneau et al., 2010] is used in this section to build a decision tree 
# on the bodyfat data.
# Code - load data
# At first, we load the bodyfat data and have a look at it.

library(mboost)
data("bodyfat", package="TH.data")

# For checking the structure of data frame 

str(bodyfat)

# To check top 5-6 rows of the dataset

head(bodyfat)

# For checking the dimensions of our training data frame and testing data frame
dim(bodyfat)

attributes(bodyfat)

bodyfat[1:5,]

# Code - split data
# Next, the data is split into training and test subsets, and a decision tree is built on the training data.

set.seed(1234) 
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]

# train a decision tree

library(rpart)
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, 
                       control = rpart.control(minsplit = 10))

attributes(bodyfat_rpart)

print(bodyfat_rpart$cptable)

print(bodyfat_rpart)

#Plot Decision Tree   

plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=T)

# check accuracy 

# Code - prediction error
# we select the tree with the minimum prediction error

opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
print(bodyfat_prune)

plot(bodyfat_prune)
text(bodyfat_prune, use.n=T)


# now have a tree with the least possible prediction error.
# Code - make predictions

DEXfat_pred <- predict(bodyfat_prune, newdata=bodyfat.test)
xlim <- range(bodyfat$DEXfat)
plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed", 
     ylab="Predicted", ylim=xlim, xlim=xlim)
abline(a=0, b=1)







