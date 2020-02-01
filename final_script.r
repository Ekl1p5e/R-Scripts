# Load data
athlete.data <- read.csv('athlete_events.csv')

# Remove irrelevant columns
# These variables were deleted because they do not contribute to the predictive power of the model
athlete.data$ID <- NULL
athlete.data$Name <- NULL
athlete.data$Games <- NULL
athlete.data$Year <- NULL

# This variable is deleted because it is redundant for our purposes
athlete.data$Event <- NULL

# This variable is deleted because it is not related to our analysis
athlete.data$Team <- NULL
athlete.data$NOC <- NULL
athlete.data$City <- NULL

# Reassign Medal variable
athlete.data$hasWonMedal <- ifelse(is.na(athlete.data$Medal), 0, 1)
athlete.data$hasWonMedal <- as.factor(athlete.data$hasWonMedal)
athlete.data$Medal <- NULL

# Remove rows with NA from Height and Weight variables
athlete.data <- athlete.data[complete.cases(athlete.data[ , c('Height', 'Weight', 'Age')]), ]

# Make factor variables
athlete.data$Sport <- as.factor(athlete.data$Sport)
athlete.data$Season <- as.factor(athlete.data$Season)

# Remove rows with sports that are no longer offered per Olympic website (https://www.olympic.org/sports)
old.sports.list <- c('Art Competitions', 'Baseball', 'Lacrosse', 'Motorboating', 'Rugby Sevens', 'Softball', 'Tug-Of-War')
athlete.data <- athlete.data[!athlete.data$Sport %in% old.sports.list, ]

# Drop unused factor levels
athlete.data <- droplevels(athlete.data)

# Use cross validation to determine a good value to use
num.pred <- dim(athlete.data)[2] - 1

# Create a training set
n = dim(athlete.data)[1]

# Large number of factors, so we will do random forest
library(randomForest)

# 10-fold CV
k = 10
groups = c(rep(1:k, floor(n / k)), 1:(n - floor(n / k) * k))

set.seed(7)
cvgroups = sample(groups, n)
athlete.tree.predict = rep(-1, n)

for(i in 1:k){
 groupi = (cvgroups == i)

 for(j in 1:num.pred){
  athlete.tree = randomForest(hasWonMedal ~ ., data = athlete.data[!groupi, ], mtry = j, importance = T)	
  athlete.tree.predict[groupi] = predict(athlete.tree, newdata = athlete.data[groupi, ], type = 'response')
 }

 # Confusion matrix error rate
 # mse.athlete.tree <- mean((athlete.data$hasWonMedal - athlete.tree.predict) ^ 2)
}

# Pick mtry with best error rate

# CV to choose number of hidden nodes for ANN
library(nnet)
h = 5:25 # number of hidden nodes
groups = c(rep(1:k, floor(n / k)))
misclassError = matrix( , nr = k, nc = length(h))
conv = matrix(, nr = k, nc = length(h))
set.seed(4)
cvgroups = sample(groups, n)

for(i in 1:k){
    groupi = (cvgroups == i)

    for(j in h){
        fit = nnet(hasWonMedal ~ ., data = athlete.data[!groupi, ], size = j, trace = F) 
        predictions = predict(fit, athlete.data[groupi, ], type = "class")
        misclassError[i, j] = length(which(predictions != athlete.data$hasWonMedal[groupi])) / length(predictions)
        conv[i, j] = fit$convergence
    }
}

colSums(conv)

error = apply(misclassError, 2, mean)
plot(h, error, type = "l", lwd = 2, las = 1)
min(error)
which(error == min(error))

########### DO DOUBLE CV
###########
# this line defines the cvgroups for the outer split (for the “double” cross-validation)

fulldata.out = # whatever your full data is

n.out  # observations in full data

k.out  # numberj of splits in out loop

groups.out  # define groups to start

 

# this line defines the cvgroups for the (outer) split

cvgroups.out = # define cvgroups for outer split using groups.out and n.out

 

# sets up storage locations

allpredictedCV.out = # define appropriate size location

 

for (j in 1:k.out) { # cycle through outer splits

groupj.out = # define how to split OUTER loop

train.out = fulldata.out[!groupj.out,]

test.out = fulldata.out[groupj.out,]

 

# these lines takes in ONLY the training dat from the outer split,

# for one-level cross-validation in following lines

fulldata.in =  train.out

n.in = # use only dimension of input data from the outer loop

k.in = 5

groups.in = c(rep(1:k.in, floor(n.in / k.in)), 1:(n.in %% k.in))

# this line defines the cvgroups for the (inner) split

cvgroups.in = sample(groups.in, n.in)

# sets up storage locations

allpredictedCV.in = matrix(rep(NA, n.in * nmodels), ncol = nmodels)

# K-folds

for (i in 1:k.in)  {

  groupi.in = (cvgroups.in == i)  # how to split

  ####model-fitting process for the model or models, applied to the training data

     #### here you should consider all possible models between which you are selecting, across all methods

  #### predict the test set, for each of the models, for the inner split

}

 

bestmodel = ## select best model, with minimum MSE from the inner loop – this could be any of models

allpredictedCV.out[groupj.out] = predict(bestmodel, test.out)

}
###########
###########


























# Create decision tree
library(tree)
athlete.data.tree <- tree(hasWonMedal ~ ., data = athlete.data, subset = train)

plot(athlete.data.tree)
text(athlete.data.tree, pretty = 0)

# Missclassification rate
summary(athlete.data.tree)