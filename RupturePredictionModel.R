#The following code generates a prediction model used to estimate
#whether or not an intracranial aneurysm will rupture based on certain properties 
#of the aneurysm in a patient with this condition.  
#Two important caveats here include the fact that data for ruptured aneurysms 
#was measured well before they actually ruptured (months before or even earlier), 
#so the aneurysm properties at the time of rupture are not actually known.
#Secondly, aneurysms that have not ruptured yet could still rupture in the future.
#Therefore, this prediction model should be taken with a grain of salt, but it
#is still helpful for thinking about the data in different ways and thinking about
#what factors or variables might be important to think about when dealing with
#the possibility of aneurysms rupturing in patients.


#Load necessary modelling and data processing libraries.  
library(dplyr)
library(caret)


#Set the working directory where the data is located.   
setwd("C:/Users/310084562/Documents/Aneurysm_Measurement_project/Aneurysm_Machine_Learning")

#Read in the aneurysm data into a data frame.  Convert all the variables to categorical, or 
#factor variables.  
aneurysm_data <- read.csv("Aneurysm_MachineLearningData_clean.csv", stringsAsFactors = F, header=T)
aneurysm_data[,1:ncol(aneurysm_data)] <- lapply(aneurysm_data[,1:ncol(aneurysm_data)], factor)

#Filter the aneurysm data to make sure that data was actually collected for the aneurysms in question.
#Refactor the aneurysm measurement variables.
cleaned_aneurysms<- aneurysm_data %>% filter(Check_DC=="OK")
cleaned_aneurysms[,1:ncol(cleaned_aneurysms)] <- lapply(cleaned_aneurysms[,1:ncol(cleaned_aneurysms)], factor)

#Remove columns that are not relevant data for the aneurysm measurements (ie. labels etc.)
columns <- c(1,2,12,13,14,17)
cleaned_aneurysms <- cleaned_aneurysms[,-columns]

#Separate the data into training and test datasets for the prediction model.  70% of the data
#will be for training the model, and 30% will be for testing the model after it has been trained.
inTrain <- createDataPartition(y=cleaned_aneurysms$Ruptured_Unruptured_DC, p=0.7, list=FALSE)
training_data <- cleaned_aneurysms[inTrain,]
testing_data <- cleaned_aneurysms[-inTrain,]


#Use cross validation to minimize the bias in the training dataset for training the data.  
train_control <- trainControl(method="cv", number =5)

#Generate the prediction model using the training dataset.  The predicted variable is categorical (ruptured vs.
#unruptured aneurysm), so a random forests model will be used.  However, other model types are possible (ie. logistic
#regression, boosting etc.)
model_rf <- train(Ruptured_Unruptured_DC~., data=training_data, trControl = train_control, method = "rf", importance=T)

#Print the model results and details of the final model (accuracy etc.)
print(model_rf)
print(model_rf$finalModel)


#Calculate and rank variable importance for the model, and print the results.
#Show the top 10 variables.
variable_importance_rf <- varImp(model_rf)
plot_var_rf <- plot(variable_importance_rf, top=10)
print(plot_var_rf)

#Now make a prediction on the test data using the trained model.  Print the confusion matrix
#for the prediction, which summarizes model performance on the test data.  
prediction_rf <- predict(model_rf, testing_data)
confusionMatrix(testing_data$Ruptured_Unruptured_DC, prediction_rf)





