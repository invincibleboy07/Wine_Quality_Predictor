#remove all objects from R
rm(list=ls())

#set current working directory
setwd("C://Users/Prakash rana/Downloads/lear/datascienceED/mini proj/CaseStudyOnWines")

#load data into R
data_red=read.csv("winequality-red.csv",sep=";",header=T,fill=T)
#ading new column red to merge later with white data
data_red["type"] <- "RED"
#reranging the columns
data_red=data_red[,c(13,1:12)]


#loading the white wine data
data_white=read.csv("winequality-white.csv",sep=";",header=T,fill=T)
#adding type column
data_white["type"] = "WHITE"
#rearanging the column
data_red=data_red[,c(13,1:12)]

#merge two wine data
data = rbind(data_red, data_white)



# Load packages
library('ggplot2')
library('ggthemes')
library('scales') 
library('dplyr') 
library('randomForest')
library("outliers")
library("caret")

install.packages('e1071', dependencies=TRUE)
#Exploratory data Analysis
#understand the data type
str(data)
#lets check if quality can be factor 
length(unique(data$quality))
unique(data$quality)



# lets convert quality and type to factor

data$quality= as.factor(data$quality)
data$type= as.factor(data$type)

## exploring the data
range(data$residual.sugar)
hist(data$residual.sugar, 
     main="Histogram for residual.sugar", 
     xlab="residual sugar", 
     border="red", 
     col="blue",
     xlim=c(0,35),
     las=1, 
     breaks=20)

range(data$alcohol)
hist(data$alcohol, 
     main="Histogram for alcohol", 
     xlab="alcohol", 
     border="red", 
     col="blue",
     xlim=c(5,15),
     las=1, 
     breaks=20)

str(data)


#Look at the block of data
head(data, 10)
tail(data, 10)

#####MISSSING VALUE ANALYSIS
apply(data, 2, function(x)sum(is.na(x)))
#convert all empty space to NA
for(i in names(data[,3:13])){
data[[i]][data[[i]] == ""] =NA
}

#again check for missing values
apply(data, 2, function(x)sum(is.na(x)))






#Identify the row and remove outlier

#boxplot
boxplot(residual.sugar~ type, data,xlab="residual.sugar",col="red",main="boxplot" )

hist(data$residual.suga, 
     main="Histogram for residual.sugar", 
     xlab="residual sugar", 
     border="red", 
     col="blue",
     xlim=c(0,35),
     las=1, 
     breaks=20)

mean(data$fixed.acidity)


outt = function(variable){
  
  outlier_tf = outlier(data[[variable]], logical=T)
  outlier_tf
  find_outlier = which(outlier_tf == TRUE,arr.ind = TRUE)
  data = data[-find_outlier, ]
  return (data)
}
#calling the outt function for each variabless
for(i in names(data[3:13]))
{
  data=outt(i)
}


#correlation 

install.packages("corrplot")
library(corrplot)
M<-cor(data[,3:13])
corrplot(M, method="circle")



##  FEATURE SCALING  ##########
#lets create a function then we will loop it for all numeric variables
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
##here [[]] is programming equivalent of $
#calling the normalize function foor each variables.
for(i in (names(data[3:13])))
{
    
  data[[i]]= normalize(data[[i]])
}






#divide the data into train and test # 80% of 6484 = 5200
train = data[sample(nrow(data), 5200, replace = F), ]
test = data[!(1:nrow(data)) %in% as.numeric(row.names(train)), ]


#############################
                                    ######### xgboost test  ##################









###########################
# building the random forest model with the features
rf_model <- randomForest(factor(quality) ~  fixed.acidity + volatile.acidity + citric.acid + residual.sugar +     
                         chlorides + total.sulfur.dioxide +
                         density + pH + + free.sulfur.dioxide +sulphates           +
                          alcohol   , data = train)


# Get importance
importance = importance(rf_model)
varImportance = data.frame(Variables = row.names(importance), 
                Importance = round(importance[ , 'MeanDecreaseGini'], 2))

# Create a rank variable based on importance
rankImportance = varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
       y = Importance, fill = Importance)) +
       geom_bar(stat='identity') + 
       geom_text(aes(x = Variables, y = 0.5, label = Rank),
       hjust=0, vjust=0.55, size = 4, colour = 'red') +
       labs(x = 'Variables') + coord_flip() + theme_few()

## Predict using the test set
prediction <- predict(rf_model, test[,c(3:13)])
xtab = table(observed = test[,1], predicted = prediction)

confusionMatrix(xtab)

