### set working directory ####'
#https://github.com/AkankshaUtreja/Diabetic-Patients-Readmission-Prediction/blob/master/Practicum2.ipynb

#Loading the data
data <- read.csv("diabetic_data.csv")

# To get the structure of the data frame

str(data)
# To get the summary of the data frame's variables
summary(data)
# View the first few rows of the data frame (dataoriginal)
head(data)


#Data Wrangling to make it fit for Analysis


#dealing with missing values

# Create a copy of the original data frame
datacopy <- data
# Replace '?' with NA to represent missing values
datacopy[datacopy == '?'] <- NA
# Check for missing values and calculate the count of missing values for each column
nacheck <- colSums(is.na(datacopy))
# Display the count of missing values for each column
nacheck


#columns with large missing values"Weight,payer_code and medical_specialty" 

# Remove the specified columns from the data frame
datacopy <- datacopy[, !(colnames(datacopy) %in% c("weight", "payer_code", "medical_specialty"))]
# Select and view the "readmitted" column
readmitted_column <- datacopy$readmitted
print(readmitted_column)


#create a new categorical variable based on the "readmitted" column using a similar approach. Here's how you can create a new variable "30readmit" with values 0 for 'NO' and 1 for all other cases:

# Create a new categorical variable "30readmit"
datacopy$'30readmit' <- ifelse(datacopy$readmitted == 'NO', 0, 1)
#group your data by the "30readmit" variable and calculate the size of each group, you can use the table() function. Here's how you can achieve that:
# Group the data by "30readmit" and calculate the size of each group
grouped_data <- table(datacopy$'30readmit')
# Print the size of each group
print(grouped_data)

#remove patients from the dataset who are considered "dead" based on certain criteria. You can achieve this by subsetting the data frame to exclude rows that meet these criteria. Here's how you can do it:

# Remove patients with specific discharge_disposition_id values associated with "dead" patients
datacopy <- datacopy[!(datacopy$discharge_disposition_id %in% c(11, 13, 14, 19, 20, 21)), ]
# View the first few rows of the data frame
head(datacopy)


#Performing Exploratory Data Analysis (EDA)

#To visualize the relationship between different variables and explore correlations in R, you can use the pairs() function from the base R package or libraries like ggplot2. Here's an example using pairs():

# Load the ggplot2 library for enhanced plotting
library(ggplot2)

# Create a scatterplot matrix for the selected variables with colors
selected_vars <- datacopy[c('num_procedures', 'num_medications', 'number_emergency')]

# Create a custom color palette
colors <- c("red", "green", "blue")

# Assign colors based on the 'number_emergency' variable
color_vector <- colors[datacopy$number_emergency]

# Create the scatterplot matrix with custom colors
pairs(selected_vars, col = color_vector, pch = 19)


#visualize the relationship between age and the number of medications. You can use the ggplot2 library for this purpose:

# Load the ggplot2 library for enhanced plotting
library(ggplot2)

# Sort the data frame by age
sortage <- datacopy[order(datacopy$age), ]

# Create a scatter plot
ggplot(sortage, aes(x = age, y = num_medications)) +
  geom_point(color = 'red') +
  theme_minimal() +
  labs(x = 'Age', y = 'Number of Medications', title = 'Number of Medications vs. Age') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability


#shows the relationship between gender and readmissions, you can use the ggplot2 library.

# Load the ggplot2 library for enhanced plotting
library(ggplot2)

# Create a count plot
ggplot(datacopy, aes(x = gender, fill = factor(`30readmit`))) +
  geom_bar(position = 'dodge', color = 'black') +
  scale_fill_manual(values = c('0' = 'blue', '1' = 'red'), name = 'Readmitted patients', labels = c('No', 'Yes')) +
  theme_minimal() +
  labs(x = 'Gender', y = 'Count', title = 'Readmissions Balance by Gender')

# Show probability of race

# Create the plot
ggplot(datacopy, aes(x = gender, y = `30readmit`, fill = gender)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme_minimal() +
  labs(x = "Gender", y = "Probability of Being Readmitted") +
  ggtitle("Probability of Being Readmitted by Gender")



#visualize the relationship between age and readmission

# Create a count plot with sorted age values
ggplot(datacopy, aes(x = factor(age, levels = unique(age)), fill = factor(`30readmit`))) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values = c('0' = 'blue', '1' = 'red'), name = 'Readmitted within 30 days', labels = c('No', 'Yes')) +
  theme_minimal() +
  labs(x = 'Age', y = 'Count', title = 'Readmissions Balance by Age')


# Show probability of age
# Create the plot
ggplot(datacopy, aes(x = age, y = `30readmit`, fill = age)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme_minimal() +
  labs(x = "Age", y = "Probability of Being Readmitted") +
  ggtitle("Probability of Being Readmitted by Age")



# create histograms for numerical variables in your dataset

# Load the ggplot2 library for enhanced plotting

# Create histograms for the specified numerical variables
par(mfrow=c(1, 1))  # Set the layout for multiple plots (adjust rows and columns as needed)

hist(datacopy$`30readmit`, main="Readmission within 30 Days", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$admission_source_id, main="Admission Source ID", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$admission_type_id, main="Admission Type ID", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$discharge_disposition_id, main="Discharge Disposition ID", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$encounter_id, main="Encounter ID", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$num_lab_procedures, main="Number of Lab Procedures", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$num_medications, main="Number of Medications", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$num_procedures, main="Number of Procedures", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$number_diagnoses, main="Number of Diagnoses", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$number_emergency, main="Number of Emergencies", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$number_inpatient, main="Number of Inpatient Visits", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$number_outpatient, main="Number of Outpatient Visits", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$patient_nbr, main="Patient Number", xlab="Value", ylab="Frequency", col="lightblue", border="black")
hist(datacopy$time_in_hospital, main="Time in Hospital", xlab="Value", ylab="Frequency", col="lightblue", border="black")

#combined into a single function 

# Load the ggplot2 library for enhanced plotting
library(ggplot2)

# Function to create histograms for specified numerical variables
create_histograms <- function(data, variables) {
  # Set the layout for multiple plots (adjust rows and columns as needed)
  num_variables <- length(variables)
  num_rows <- ceiling(sqrt(num_variables))
  num_cols <- ceiling(num_variables / num_rows)
  par(mfrow = c(num_rows, num_cols))
  
  for (col_name in variables) {
    hist_data <- data[, col_name]
    hist_title <- paste("Histogram of", col_name)
    
    hist(hist_data, main = hist_title, xlab = "Value", ylab = "Frequency", col = "lightblue", border = "black")
  }
  
  # Reset the layout to default
  par(mfrow = c(1, 1))
}

# Specify the list of numerical variables to create histograms for
numerical_variables <- c(
  "30readmit",
  "admission_source_id",
  "admission_type_id",
  "discharge_disposition_id",
  "encounter_id",
  "num_lab_procedures",
  "num_medications",
  "num_procedures",
  "number_diagnoses",
  "number_emergency",
  "number_inpatient",
  "number_outpatient",
  "patient_nbr",
  "time_in_hospital"
)

# Call the function to create histograms for the specified variables
create_histograms(datacopy, numerical_variables)


#explore the categorical variables 

# Load the ggplot2 library for enhanced plotting
library(ggplot2)

# Create a 2x2 grid of count plots for categorical variables with automatically assigned colors
p1 <- ggplot(datacopy, aes(x = readmitted, fill = readmitted)) +
  geom_bar() +
  labs(x = "Readmitted", y = "Count") +
  scale_fill_discrete() +
  ggtitle("Count of Readmissions")

p2 <- ggplot(datacopy, aes(x = race, fill = race)) +
  geom_bar() +
  labs(x = "Race", y = "Count") +
  scale_fill_discrete() +
  ggtitle("Count of Races")

p3 <- ggplot(datacopy, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(x = "Gender", y = "Count") +
  scale_fill_discrete() +
  ggtitle("Count of Genders")

p4 <- ggplot(datacopy, aes(x = age, fill = age)) +
  geom_bar() +
  labs(x = "Age", y = "Count") +
  scale_fill_discrete() +
  ggtitle("Count of Age Groups")

# Create a 2x2 grid of plots
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)




###additional EDA

# Show distribution of race

# Load required libraries
library(ggplot2)

# Open a new graphics device
graphics.off()

# Create a bar plot to show the distribution of race and 30readmit
ggplot(datacopy, aes(x = race, fill = factor(`30readmit`))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(x = "Race", y = "Count") +
  ggtitle("Count of Race - 30readmit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))  # Define custom colors for 30readmit categories

############


# # Show probability of race

# Assuming you have your data in a data frame named 'datacopy'
# You may need to install and load the 'ggplot2' package if not already done

# Load the ggplot2 package if not already loaded
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Define a color palette for race labels
race_colors <- c("Caucasian" = "red", "AfricanAmerican" = "blue", "Asian" = "green", 
                 "Hispanic" = "purple", "Other" = "orange")

# Create the plot
ggplot(datacopy, aes(x = race, y = `30readmit`, fill = race)) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_manual(values = race_colors) +  # Use the defined color palette
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Race", y = "Probability of Being Readmitted") +
  ggtitle("Probability of Being Readmitted by Race") +
  theme_minimal()



#### Show count of Time in hospital

# Assuming you have your data in a data frame named 'datacopy'

# Use the table() function to count the occurrences of 'time_in_hospital'
count_by_time_in_hospital <- table(datacopy$time_in_hospital)

# Print the counts
print(count_by_time_in_hospital)

# Assuming you have your data in a data frame named 'datacopy'

# Create the count plot
ggplot(datacopy, aes(x = factor(time_in_hospital), fill = factor(readmitted))) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Time in Hospital - Readmitted") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Show probability of time in hospital

# Create a data frame with aggregated probabilities

## Calculate probabilities of 30readmit
probabilities <- aggregate(datacopy$`30readmit`, by=list(datacopy$time_in_hospital), FUN=mean)

# Rename the columns for clarity
colnames(probabilities) <- c("time_in_hospital", "Probability")

# Create a probability plot
ggplot(probabilities, aes(x = time_in_hospital, y = Probability)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Time in Hospital", y = "Probability") +
  ggtitle("Probability of 30readmit by Time in Hospital") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Show count of num_medications
# Load the required library 
library(ggplot2)

# Create a count plot for num_medications
ggplot(datacopy, aes(x = factor(num_medications), fill = factor(`30readmit`))) +
  geom_bar() +
  labs(x = "Number of Medications", y = "Count") +
  ggtitle("Number of Medications - 30readmit") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

# Show probability of num_medications

# Calculate probabilities of 30readmit
probabilities <- aggregate(datacopy$`30readmit`, by=list(datacopy$num_medications), FUN=mean)

# Rename the columns for clarity
colnames(probabilities) <- c("num_medications", "Probability")

# Create a probability plot
library(ggplot2)

ggplot(probabilities, aes(x = num_medications, y = Probability)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Number of Medications", y = "Probability") +
  ggtitle("Probability of 30readmit by Number of Medications") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#Machine learning models to predict hospital readmission



#To clean the data and replace null values in numeric columns with 0 and in object columns with "unknown" 

# Identify numeric and object columns
num_columns <- sapply(datacopy, is.numeric)
obj_columns <- sapply(datacopy, is.factor)

# Replace null values in numeric columns with 0
datacopy[, num_columns][is.na(datacopy[, num_columns])] <- 0

# Replace null values in object columns with "unknown"
datacopy[, obj_columns][is.na(datacopy[, obj_columns])] <- "unknown"


#To substitute 0 for missing values in numeric columns and "unknown" for missing values in object columns 

# Identify numeric and object columns
num_columns <- sapply(datacopy, is.numeric)
obj_columns <- sapply(datacopy, is.factor)

# Substitute 0 for missing values in numeric columns
datacopy[, num_columns][is.na(datacopy[, num_columns])] <- 0

# Substitute "unknown" for missing values in object columns
datacopy[, obj_columns][is.na(datacopy[, obj_columns])] <- "unknown"

head(datacopy, 2)




#encode the data

# Define a function to map numeric codes to medical categories
map_now <- function() {
  listname <- list(
    c('infections', 139),
    c('neoplasms', (239 - 139)),
    c('endocrine', (279 - 239)),
    c('blood', (289 - 279)),
    c('mental', (319 - 289)),
    c('nervous', (359 - 319)),
    c('sense', (389 - 359)),
    c('circulatory', (459 - 389)),
    c('respiratory', (519 - 459)),
    c('digestive', (579 - 519)),
    c('genitourinary', (629 - 579)),
    c('pregnancy', (679 - 629)),
    c('skin', (709 - 679)),
    c('musculoskeletal', (739 - 709)),
    c('congenital', (759 - 739)),
    c('perinatal', (779 - 759)),
    c('ill-defined', (799 - 779)),
    c('injury', (999 - 799))
  )
  
  dictcout <- list()
  count <- 1
  
  for (pair in listname) {
    name <- pair[1]
    num <- pair[2]
    
    for (i in 1:num) {
      dictcout[[as.character(count)]] <- name
      count <- count + 1
    }
  }
  
  return(dictcout)
}

# Define a function to perform encoding and mapping
codemap <- function(df, codes) {
  for (col in names(df)) {
    temp <- character(0)
    
    for (num in df[[col]]) {
      if (is.null(num) || num %in% c('unknown', '?') || is.na(num)) {
        temp <- c(temp, 'unknown')
      } else if (substring(num, 1, 1) == 'V') {
        temp <- c(temp, 'supplemental')
      } else if (substring(num, 1, 1) == 'E') {
        temp <- c(temp, 'injury')
      } else {
        lkup <- strsplit(as.character(num), '.', fixed = TRUE)[[1]][1]
        temp <- c(temp, codes[[lkup]])
      }
    }
    
    df[[col]] <- temp
  }
  
  return(df)
}

# Specify the columns to be encoded
listcol <- c('diag_1', 'diag_2', 'diag_3')

# Generate the mapping codes
codes <- map_now()

# Apply the encoding and mapping to the specified columns
datacopy[listcol] <- codemap(datacopy[listcol], codes)


#summary statistics

# View summary statistics of the dataset
summary(datacopy)

#drop columns
# Define a vector of column names to drop
columns_to_drop <- c("encounter_id", "patient_nbr", "admission_type_id", "readmitted")
# Create a new DataFrame with the specified columns dropped
data1 <- datacopy[, !names(datacopy) %in% columns_to_drop]
# View the first 2 rows of the new DataFrame
head(data1, 2)




#normalize data

#which performs standardization (mean centering and scaling to unit variance) on the specified columns

# Specify the columns to normalize
listnormal <- c('time_in_hospital', 'num_lab_procedures', 'num_procedures', 'num_medications',
                'number_outpatient', 'number_emergency', 'number_inpatient', 'number_diagnoses')

# Perform standardization (mean centering and scaling to unit variance)
data1[, listnormal] <- scale(data1[, listnormal])

# View the summary statistics of the normalized data
summary(data1)


########## Encoding non-numerical columns: Label Encoding (taking gender columns as an example how categorical variable can be used in ML)

# Selecting categorical columns (string or factor)
cat_data <- data1[sapply(data1, is.factor) | sapply(data1, is.character)]

# Selecting numeric columns
num_data <- data1[sapply(data1, is.numeric)]

# Get categorical mapping for final application

# Encode the 'race' column into integer labels
data1$gender <- as.integer(factor(data1$gender))
data1$gender <- factor(data1$gender)

# Create a mapping of original values to integer labels
integer_mapping <- levels(data1$gender)




#reduce the data size to made the model trainig efficient


# Randomly sample 20% of the rows
data1_subset <- data1[sample(nrow(data1), size = 0.4 * nrow(data1)), ]

# or

#limit the number of column variables
# Select specific columns (replace colnames with the columns you need)
selected_columns <- c("gender", "num_lab_procedures", "num_procedures", "number_outpatient", "number_emergency", "number_inpatient","30readmit", "time_in_hospital", "num_medications", "number_diagnoses")

data1_subset <- data1_subset[selected_columns]

# Assuming your data frame is named "my_data_frame"
variable_types <- sapply(data1_subset, class)

# Print the variable types
print(variable_types)

data1_subset$`30readmit` <- as.factor(data1_subset$`30readmit`)

# Encode the 'race' column into integer labels
# Remove rows where Value is equal to 3
data1_subset <- data1_subset[data1_subset$gender != 3, ]

data1_subset$gender <- as.integer(factor(data1_subset$gender))
data1_subset$gender <- factor(data1_subset$gender)




save(data1_subset, file='data1_subset.Rdta')

load('data1_subset.Rdta')


# Correlation Matrix
# Understanding the relationship between attributes in our dataset.
library(reshape2)
library(tidyr)


# Set the plot size and font scale
options(repr.plot.width = 18, repr.plot.height = 10)
theme_set(theme_gray(base_size = 20))

# Encode factor variables as numeric
data1_numeric <- as.data.frame(sapply(data1_subset, function(x) {
  if (is.factor(x)) as.numeric(x) else x
}))

# Create a correlation matrix
correlation_matrix <- cor(data1_numeric)

# Create a heatmap using ggplot2
ggplot(data = melt(correlation_matrix), aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) +  # Display correlation values
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # Choose a color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
  labs(x = "", y = "")  # Remove axis labels






#store the '30readmit' column in Y and the rest of the columns in X by selecting and creating new data frames accordingly

# Store '30readmit' in Y
Y <- data1_subset$'30readmit'

# Drop '30readmit' column from X
X <- data1_subset[, !names(data1_subset) %in% '30readmit']

# Convert categorical variables to dummy variables (if needed)
# Note: If your data is already one-hot encoded, you can skip this step

# View the first few rows of X (optional)
head(X)
head(Y)

#split the data into training and validation datasets with an 80-20 split 

# Set the seed for reproducibility
set.seed(7)

# Define the proportion for the validation set (20%)
validation_proportion <- 0.2

# Create an index vector for splitting
index <- sample(1:nrow(X), size = floor(validation_proportion * nrow(X)))

# Split the data into training and validation sets
Xtrain <- X[-index, ]
Xtest <- X[index, ]
Ytrain <- Y[-index]
Ytest <- Y[index]

#Logistic regression 

# Load the required library for logistic regression
library(stats)

str(Ytrain)
str(Xtrain)


#convert character variables into facotr

convertCharacterToFactor <- function(data) {
  # Get the names of character columns
  char_cols <- sapply(data, is.character)
  
  # Convert character columns to factors
  data[char_cols] <- lapply(data[char_cols], as.factor)
  
  return(data)
}

#Convert character variables to factors in Xtrain
Xtrain <- convertCharacterToFactor(Xtrain)
Ytrain <- convertCharacterToFactor(Ytrain)
Xtest <- convertCharacterToFactor(Xtest)


# Create a logistic regression model
logistic_model <- glm(Ytrain ~ ., data = data.frame(Ytrain, Xtrain), family = binomial(link = "logit"))

# Predict the target variable for the test data
Ylog <- predict(logistic_model, newdata = data.frame(Xtest), type = "response")


# Convert predicted probabilities to binary predictions
Ylog <- ifelse(Ylog > 0.5, 1, 0)

#check the accuracy of the model 

# Make predictions on the test data
Y_pred <- predict(logistic_model, newdata = Xtest, type = "response")

# Convert predicted probabilities to binary predictions (e.g., 0 or 1)
Y_pred_binary <- ifelse(Y_pred > 0.5, 1, 0)

#omit the missing values in Ytest and Y_pred_binary
Ytest <- na.omit(Ytest)
Y_pred_binary <- na.omit(Y_pred_binary)

length(Y_pred_binary)
length(Ytest)


# Check for missing values in Ytest or Y_pred_binary
if (any(is.na(Ytest)) || any(is.na(Y_pred_binary))) {
  cat("Warning: Missing values detected in Ytest or Y_pred_binary.\n")
} else {
  # Calculate accuracy
  accuracy <- mean(Y_pred_binary == Ytest)
  
  # Check for NA values in accuracy
  if (is.na(accuracy)) {
    cat("Warning: Accuracy calculation resulted in NA.\n")
  } else {
    # Print the accuracy
    cat("The accuracy of the Logistic regression model:", accuracy, "\n")
  }
}




#checking the confusion matrix

# Assuming you have already trained a logistic regression model named 'logistic_model'
# Make predictions on the test data
Y_pred <- predict(logistic_model, newdata = Xtest, type = "response")

# Convert predicted probabilities to binary predictions (e.g., 0 or 1)
Y_pred_binary <- ifelse(Y_pred > 0.5, 1, 0)

# Load the 'caret' package for calculating confusion matrix
#install.packages("caret")
library(caret)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(data = factor(Y_pred_binary), reference = factor(Ytest))

# Print the confusion matrix
print(confusion_matrix)


# Assuming you have a confusion matrix object named 'conf_matrix'

# Extract the confusion matrix values
conf_matrix_values <- confusion_matrix$table

# Create a data frame from the confusion matrix values
confusion_df <- data.frame(
  Actual = rep(rownames(conf_matrix_values), each = ncol(conf_matrix_values)),
  Predicted = rep(colnames(conf_matrix_values), times = nrow(conf_matrix_values)),
  Frequency = as.vector(conf_matrix_values)
)

# Load necessary libraries
library(ggplot2)

# Create a heatmap
ggplot(data = confusion_df, aes(x = Predicted, y = Actual, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), vjust = 1) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Predicted label", y = "Actual label",
       title = "Confusion Matrix Heatmap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#check summary of classification 

# Assuming you have the following variables:
# Ytest: True target values (actual class labels)
# Ylog: Predicted target values (model's predictions)

# Load the required library
library(caret)

# Create a confusion matrix
confusion_matrix <- confusionMatrix(data = factor(Ylog), reference = factor(Ytest))

# Generate a classification report
class_report <- as.data.frame(confusion_matrix$byClass)

# Print the classification report
print(class_report)

# interpret the report 

# Here's an interpretation of each of these metrics:
# 
# Sensitivity (True Positive Rate): 0.7751
# 
# This metric represents the proportion of actual positive cases (readmitted patients) correctly identified by the model. In this case, the model correctly identified approximately 77.51% of the readmitted patients.
# Specificity (True Negative Rate): 0.4384
# 
# This metric represents the proportion of actual negative cases (non-readmitted patients) correctly identified by the model. In this case, the model correctly identified approximately 43.84% of the non-readmitted patients.
# Positive Predictive Value (Precision): 0.6035
# 
# This metric tells us the accuracy of the model when it predicts a positive outcome (readmission). It means that when the model predicts readmission, it is correct approximately 60.35% of the time.
# Negative Predictive Value: 0.6387
# 
# This metric tells us the accuracy of the model when it predicts a negative outcome (non-readmission). It means that when the model predicts non-readmission, it is correct approximately 63.87% of the time.
# F1-Score: 0.6786
# 
# The F1-Score is the harmonic mean of precision and recall. It balances the trade-off between precision and recall. In this case, it is approximately 67.86%, indicating a good balance between precision and recall.
# Prevalence: 0.5244
# 
# This is the proportion of actual positive cases in the dataset. It represents the prevalence of readmitted patients in the dataset and is approximately 52.44%.
# Detection Rate: 0.4065
# 
# This is the rate at which the model correctly detects positive cases. It is approximately 40.65%, indicating that the model detects readmitted patients at this rate.
# Detection Prevalence: 0.6736
# 
# This is the rate at which the model predicts positive cases. It is approximately 67.36%, indicating that the model predicts readmission in a significant portion of the dataset.
# Balanced Accuracy: 0.6068
# 
# This metric takes into account both sensitivity and specificity to provide an overall measure of classification accuracy. It is approximately 60.68%, indicating a reasonable overall accuracy of the model.




#checking performance of the model by plotting the Receiver Operating Characteristic (ROC) curve


# Assuming you have a logistic regression model named 'logistic_model' and a test dataset 'Xtest'
library(pROC)

# Predict probabilities using the logistic regression model
Y_prob <- predict(logistic_model, newdata = Xtest, type = "response")

# Create a ROC curve object
roc_curve <- roc(response = Ytest, predictor = Y_prob)

# Set up a new plot with larger dimensions
png("roc_curve.png", width = 800, height = 800)  # Adjust the width and height as needed

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")

# Calculate the AUC-ROC
auc(roc_curve)

# Save the plot to a file
dev.off()



#########
######### Random Forest Classifier


# Load the randomForest library
# Load the randomForest package if not already loaded
if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}

# Combine Ytrain and Xtrain into a single data frame
train_data <- data.frame(Ytrain, Xtrain)


# Train the random forest classifier
random_forest <- randomForest(Ytrain ~ ., data = train_data, ntree = 500, importance = TRUE, na.action = na.roughfix)

# Make predictions using the trained model
Yrandforest <- predict(random_forest, Xtest)


#Calculating the score

# Load the pROC package if it's not already installed
# install.packages("pROC")
library(pROC)

# Assuming you have already trained a random forest model named "random_forest" and have your test data Xtest and Ytest

# Calculate the predicted probabilities for class 1
scorey <- predict(random_forest, newdata = Xtest, type = "response")

class(scorey)

scorey <- as.numeric(scorey)


# Create an ROC curve object
roc_curve <- roc(Ytest, scorey)

# Extract the FPR, TPR, and thresholds
rfpr <- roc_curve$specificities
rtpr <- roc_curve$sensitivities
thresholds <- roc_curve$thresholds

#Checking the accuracy,

# Assuming you have trained a Random Forest model named "random_forest" and have test data Xtest and Ytest

# Make predictions using the Random Forest model
predictions <- predict(random_forest, newdata = Xtest)

# Calculate accuracy by comparing predicted values to actual values
accuracy <- sum(predictions == Ytest) / length(Ytest)

# Print the accuracy
cat("Accuracy of Random Forest classification:", accuracy, "\n")


#Printing the confusion matrix,
# Assuming you have the true labels Ytest and predicted labels Yrandforest

# Load the caret package if it's not already installed
# install.packages("caret")
library(caret)

# Create the confusion matrix
confusion_matrix <- confusionMatrix(Yrandforest, reference = Ytest)

# Print the confusion matrix
print(confusion_matrix)

# Convert the confusion matrix to a numeric matrix (if it's not already)
confusion_matrix <- as.matrix(confusion_matrix)


# Install and load the necessary libraries if they're not already installed
# install.packages("ggplot2")
library(ggplot2)

# Assuming you have the confusion matrix stored in a variable named "confusion_matrix"
# You can create it using confusionMatrix() function from the caret package

# Convert the confusion matrix to a data frame
confusion_df <- as.data.frame(as.table(confusion_matrix))

# Set the size of the plot
options(repr.plot.width = 9, repr.plot.height = 9)  # Adjust plot size as needed

# Create a heatmap using ggplot2
ggplot(confusion_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.3f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Predicted label", y = "Actual label", fill = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("Accuracy Score: ", round(random_forest$err.rate[nrow(random_forest$err.rate), "OOB"], 3), sep = ""))


#
# Generate a classification report
class_report <- confusionMatrix(Yrandforest, Ytest)

# Print the classification report
print(class_report)




#Determining which features are most important,


# Assuming you have the random forest model named "random_forest" and your data in "Xtrain"

# Get feature importances from the random forest model
feature_imports <- random_forest$importance

# Get feature names
feature_names <- rownames(feature_imports)

# Create a data frame with feature names and importances
feature_data <- data.frame(Feature = feature_names, Importance = feature_imports[, 1])

# Select the top 10 most important features
top_10_features <- head(arrange(feature_data, desc(Importance)), 10)

# Create a bar plot for the most important features
ggplot(top_10_features, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", alpha = 0.8, fill = "blue") +
  coord_flip() +
  labs(x = "Importance", y = "Feature") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14)) +
  ggtitle("Most Important Features - Random Forest")



############ Decision tree

# Install and load the necessary library if it's not already installed
# install.packages("rpart")
library(rpart)

# Assuming you have your data in Xtrain and Ytrain, and test data in Xtest

# Create the Decision Tree classifier with the Gini criterion
clfgini <- rpart(Ytrain ~ ., data = cbind(Ytrain, Xtrain), method = "class",
                 control = rpart.control(maxdepth = 3, minsplit = 5))


# Fit the classifier to the training data
# This step is not explicitly needed in R as the model is created during the rpart() call

# Make predictions on the test data
ypreddt <- predict(clfgini, cbind(Ytrain, Xtrain), type = "class")

# Print the predictions
print(ypreddt)

###############

# Assuming you have the actual labels Ytest and predicted labels ypreddt

# Ensure that the vectors have the same length by taking the minimum length
min_length <- min(length(Ytest), length(ypreddt))

# Calculate the accuracy score
accuracy <- sum(Ytest[1:min_length] == ypreddt[1:min_length]) / min_length * 100

# Print the accuracy
cat("Accuracy is ", accuracy, "%\n")



####### KNN

# Assuming you have prepared your training and testing data as X_train and X_test, and y_train for the labels

# Load the 'class' package if not already loaded
library(class)

# Set the number of neighbors (k) for the KNN classifier (you can adjust this)
k <- 3

# Fit the KNN classifier to the training data
knn_model <- knn(train = Xtrain, test = Xtest, cl = Ytrain, k = k)

# Make predictions on the test data
knn_pred <- as.factor(knn_model)

# Now knn_pred contains the predicted labels for the test data

# Assuming you have the true labels in y_test and the predicted labels in knn_pred

# Load the necessary libraries if not already loaded
library(caret)
library(e1071)
library(pROC)

# Calculate accuracy
accuracy_knn2 <- sum(Ytest == knn_pred) / length(Ytest) * 100

# Calculate precision
precision_knn2 <- posPredValue(knn_pred, Ytest)

# Calculate recall (sensitivity)
recall_knn2 <- sensitivity(knn_pred, Ytest)

# Print the results
cat("Accuracy is ", format(accuracy_knn2, digits = 2), "%\n")
cat("Precision is ", format(precision_knn2, digits = 2), "\n")
cat("Recall is ", format(recall_knn2, digits = 2), "\n")




##################### Comparing the models


# Install and load the necessary libraries if they're not already installed
# install.packages("pROC")
library(pROC)

# Assuming you have the actual labels Ytest and predicted scores/probabilities

# Calculate ROC curves for different classifiers

# Logistic Regression
# Assuming you want to convert the variable ypreddt to numeric
Y_prob <- as.numeric(Y_prob)
roc_log <- roc(Ytest, Y_prob)

# Random Forest Classifier
Yrandforest <- as.numeric(Yrandforest)
roc_rf <- roc(Ytest, Yrandforest)

# Decision Tree Classifier
ypreddt <- as.numeric(ypreddt)
roc_dt <- roc(Ytest, ypreddt)

#KNN knn_pred
knn_pred <- as.numeric(knn_pred)
roc_knn <- roc(Ytest, knn_pred)

# You can access the TPR (True Positive Rate) and FPR (False Positive Rate) as follows:
fpr_log <- roc_log$specificities
tpr_log <- roc_log$sensitivities

fpr_rf <- roc_rf$specificities
tpr_rf <- roc_rf$sensitivities

fpr_dt <- roc_dt$specificities
tpr_dt <- roc_dt$sensitivities

fpr_knn <- roc_knn$specificities
tpr_knn <- roc_knn$sensitivities

# You can also access the thresholds using roc_log$thresholds, roc_rf$thresholds, etc.

# Install and load the necessary libraries if they're not already installed
# install.packages("ggplot2")
library(ggplot2)

# Assuming you have the vectors fpr_log, tpr_log, fpr_rf, tpr_rf, fpr_dt, tpr_dt

# Install and load the necessary libraries if they're not already installed
# install.packages("ggplot2")
library(ggplot2)

# Assuming you have the vectors fpr_log, tpr_log, fpr_rf, tpr_rf, fpr_dt, tpr_dt

# Create a data frame to combine the ROC curve data
roc_data <- data.frame(
  Model = c("Logistic regression", "Random Forest", "Decision Tree"),
  FPR = c(fpr_log, fpr_rf, fpr_dt),
  TPR = c(tpr_log, tpr_rf, tpr_dt)
)

# Create the ROC curve plot
roc_plot <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1) +
  xlim(0, 1) +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the ROC curve plot
print(roc_plot)


############


# Calculate AUC for each model
auc_values <- c(
  auc(roc(Ytest, Y_prob)),
  auc(roc(Ytest, Yrandforest)),
  auc(roc(Ytest, ypreddt)),
  auc(roc(Ytest, knn_pred))
)

# Create a data frame for AUC values
auc_data <- data.frame(Model = c("Logistic Regression", "Random Forest", "Decision Tree", "KNN"), AUC = auc_values)



# Load the necessary libraries if not already loaded
library(ggplot2)

# Create a bar plot for AUC values
ggplot(auc_data, aes(x = Model, y = AUC, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(y = "AUC Value", x = "Models") +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = round(AUC, 2)), vjust = -0.5, size = 4) +
  coord_flip()



##############

# Assuming you have the true labels in Ytest and the predicted labels for each model

# Load the necessary libraries if not already loaded
library(caret)

# Calculate accuracy, precision, and recall for each model
accuracy_logistic <- sum(Ytest == Y_prob) / length(Ytest) * 100
# Calculate precision for the Logistic Regression model
precision_logistic <- sum(Ytest[Y_prob > 0.5] == 1) / sum(Y_prob > 0.5)
# Calculate recall (sensitivity) for the Logistic Regression model
recall_logistic <- sum(Ytest[Y_prob > 0.5] == 1) / sum(Ytest == 1)


# Convert Yrandforest to a factor with the same levels as Ytest
Yrandforest <- factor(Yrandforest, levels = levels(Ytest))
# Calculate accuracy for the Random Forest model
accuracy_rf <- sum(Ytest == Yrandforest) / length(Ytest) * 100
precision_rf <- posPredValue(Yrandforest, Ytest)
recall_rf <- sensitivity(Yrandforest, Ytest)


accuracy_dt <- sum(Ytest == ypreddt) / length(Ytest) * 100
# Convert ypreddt to a factor with the same levels as Ytest
ypreddt <- factor(ypreddt, levels = levels(Ytest))
# Calculate precision for the Decision Tree model
precision_dt <- posPredValue(ypreddt, Ytest)
recall_dt <- sensitivity(ypreddt, Ytest)


accuracy_knn <- sum(Ytest == knn_pred) / length(Ytest) * 100
# Convert knn_pred to a factor with the same levels as Ytest
knn_pred <- factor(knn_pred, levels = levels(Ytest))
# Calculate precision for the KNN model
precision_knn <- posPredValue(knn_pred, Ytest)
recall_knn <- sensitivity(knn_pred, Ytest)




# Load the necessary libraries if not already loaded
library(ggplot2)

# Define models, values, and metrics
models <- c('Logistic Regression', 'Decision Tree', 'Random Forest', 'KNN')
accuracy_values <- c(accuracy_logistic, accuracy_dt, accuracy_rf, accuracy_knn)
precision_values <- c(precision_logistic, precision_dt, precision_rf, precision_knn)
recall_values <- c(recall_logistic, recall_dt, recall_rf, recall_knn)

# Create a data frame for the metrics
metrics_data <- data.frame(Model = rep(models, each = 3),
                           Metric = c(rep('Accuracy', 4), rep('Precision', 4), rep('Recall', 4)),
                           Value = c(accuracy_values, precision_values, recall_values))

# Create a bar plot for the metrics
ggplot(metrics_data, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 0.6) +
  labs(y = 'Metric Value', x = 'Models') +
  theme_minimal() +
  theme(legend.position = 'top') +
  scale_fill_manual(values = c('Accuracy' = 'orange', 'Precision' = 'lightgreen', 'Recall' = 'lightblue')) +
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.6), vjust = -0.5, size = 4) +
  coord_flip() +
  ggtitle('Model Comparison - Using SMOTE') +
  scale_x_discrete(limits = rev(models))


