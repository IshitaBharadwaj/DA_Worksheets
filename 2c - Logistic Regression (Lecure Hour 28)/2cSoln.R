#Solution to Worksheet 2c - Logistic Regression 
#SRN:     Name:   
#I have discussed/ collaborated with SRN:    Name: 

library(tidyverse)
library(InformationValue)

char_preds <- read.csv('got_characters.csv')

#Problem 1: 
#No. of characters listed in the dataframe
sprintf("The no. of characters listed in the data = %d", nrow(char_preds))

#Set empty entries to NA
char_preds[char_preds == "" | char_preds == " "] <- NA

# Make a DF for number of null vals in each column
df <- data.frame(colSums(is.na(char_preds)) / nrow(char_preds) * 100)

# Rename column with an appropriate header
colnames(df) <- c('% Missing')

# Reset index and make a feature of col names
df$Columns <- rownames(df)
rownames(df) <- NULL

# Order in decreasing order of percentages - 
#use new_df<-df[order(df$'% Missing', )] with appropriate arguments 
#to order the data by descending order of no. of NA's
#Complete the followig line with the appropriate truth value... 
new_df <- df[order(df$'% Missing', decreasing = ???)]
rownames(new_df) <- NULL
new_df

#Problem 2: 
#What are the inferences you can draw from the output dataframe of 
#the previous problem? How can we handle columns with extremely 
#high proportions of missing data before moving on?


#If the missing data in a column does not tell you something about the target
#variable (`actual`) or it is MCAR, set a missing percentage threshold of 80%, 
#deeming the column as having insufficient data, and drop the column.
remove <- new_df[new_df$'% Missing' > 80,'Columns']
char_preds <- char_preds[!names(char_preds) %in% remove]


#Check if there are any peculiar patterns in the data. For instance, what 
#do you notice from the following graph for age? 
ggplot(char_preds, aes(x=age)) + geom_bar()  

#Function to compute mode (if you need this for any inputation)
# Function to calculate mode
get_mode <- function(x) {
  mode0 <- names(which.max(table(x)))
  if(is.numeric(x)) return(as.numeric(mode0))
  mode0
}

#Impute missing data on the remaining numeric features (use 'unclass')
#You can fill in -1 for categorical values (the following is only an example)
#Select the appropriate list of columns to apply these commands to
char_categorical <- c('culture')
char_preds[, char_categorical] <- lapply(char_preds[, char_categorical], as.factor)
char_preds[, char_categorical] <- sapply(char_preds[, char_categorical], unclass)

# Replace missing with -1
char_preds[is.na(char_preds)] = -1

#Problem 3: 
# Original distribution
table(char_preds$actual)

# Create training data
input_ones <- char_preds[which(char_preds$actual == 1), ]
input_zeros <- char_preds[which(char_preds$actual == 0), ]  # all 0's
set.seed(100)  

# Sample from all alive characters
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_zeros)) 
# Sample from all dead characters
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_zeros))  

training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]

# Row bind both class dataframes
trainingData <- rbind(training_ones, training_zeros)
# Shuffle rows 
trainingData <- trainingData[sample(1:nrow(trainingData)), ]

# Create testing data
test_ones <- input_ones[-input_ones_training_rows, ]

#Create testing data for zeros and bind the dataframes as shown in the above lines 
#testData is the variable that will store the dataframe after binding 

#Check the distribution of classes in the splits
table(trainingData$actual)
table(testData$actual)

#Problem 4: Build the logistic regression model 
# Build the Logistic Regression model with a list of features... 
#Select your own list here (perfectly valid to select all features too)
logitmod <- glm(actual ~ age + culture male + book1 + 
                  isMarried + boolDeadRelations + isPopular + popularity, 
                family = binomial(link="logit"), data=trainingData)

summary(logitmod)

predicted <- plogis(predict(logitmod, testData))  # predicted scores

#The default cut-off, as suggested by Tanish is 0.5
#What is the optimal cutoff? 
#You can use the following lines of code for this: 
library(InformationValue)
optCutOff <- optimalCutoff(testData$actual, predicted)[1] 
optCutOff

#Problem 5: Use the optimal cut-off to compute the confusion matrix and 
#compute measures; how does this differ for your model from the default cutoff?
#You can use the following lines of code as a reference
misClassError(testData$actual, predicted, threshold = optCutOff)
sensitivity(testData$actual, predicted, threshold = optCutOff)
specificity(testData$actual, predicted, threshold = optCutOff)
# The columns are actual values (ground truth), while rows are predicted values.
confusionMatrix(testData$actual, predicted, threshold = optCutOff)

#Plot the RoC curve and report the AUC
plotROC(testData$actual, predicted)


