################# Data import #################

# 1. Set working directory
setwd("C:\\Users\\bodhi\\OneDrive\\Documents\\APU learning materials\\PFDA\\Assignment")


# 2. Read data from CSV file
credit_risk_classification <- read.csv("5. credit_risk_classification.csv", sep = ',')

# 3. View the dataset
View(credit_risk_classification)


################# Data cleaning/pre-processing #################

# 1. Remove unneccessary columns
# Drop 'X' column as it does not contribute meaningful info
credit_risk_classification$X <- NULL

# 2. Convert categorial columns to factors
credit_risk_classification$class <- as.factor(credit_risk_classification$class)
credit_risk_classification$checking_status <- as.factor(credit_risk_classification$checking_status)
credit_risk_classification$credit_history <- as.factor(credit_risk_classification$credit_history)
credit_risk_classification$purpose <- as.factor(credit_risk_classification$purpose)
credit_risk_classification$savings_status <- as.factor(credit_risk_classification$savings_status)
credit_risk_classification$employment <- as.factor(credit_risk_classification$employment)
credit_risk_classification$personal_status <- as.factor(credit_risk_classification$personal_status)
credit_risk_classification$other_parties <- as.factor(credit_risk_classification$other_parties)
credit_risk_classification$property_magnitude <- as.factor(credit_risk_classification$property_magnitude)
credit_risk_classification$other_payment_plans <- as.factor(credit_risk_classification$other_payment_plans)
credit_risk_classification$housing <- as.factor(credit_risk_classification$housing)
credit_risk_classification$job <- as.factor(credit_risk_classification$job)
credit_risk_classification$own_telephone <- as.factor(credit_risk_classification$own_telephone)
credit_risk_classification$foreign_worker <- as.factor(credit_risk_classification$foreign_worker)

# 3. Convert employment and saving_status to ordered factors
# Assuming there's a meaningful order behind these categories
credit_risk_classification$employment <- factor(credit_risk_classification$employment, 
                                                levels = c("unemployed", "<1", "1<=X<4", "4<=X<7", ">=7"), 
                                                ordered = TRUE)
credit_risk_classification$savings_status <- factor(credit_risk_classification$savings_status, 
                                                    levels = c("no known savings", "<100", ">=100", ">=500", ">=1000"), 
                                                    ordered = TRUE)

# 4. Handle missing values by replacing NAs with common value (mode)
# Check NA values in the entire data frame
colSums(is.na(credit_risk_classification))

# Find the most common value (mode) for savings_status
mode_savings <- names(sort(table(credit_risk_classification$savings_status), decreasing = TRUE))[1]

# Impute NA values with the mode
credit_risk_classification$savings_status[is.na(credit_risk_classification$savings_status)] <- mode_savings

# 5. Values standardization
# Define columns to inspect
columns_to_check <- c("checking_status", "credit_history", "purpose", "personal_status", "job")

# Standardize values by converting to lowercase & trim its whitespaces
credit_risk_classification[columns_to_check] <- lapply(
  credit_risk_classification[columns_to_check], function(x) tolower(trimws(x))
)

# 6. Grouping factor levels into larger categories
# 6a. Grouping savings_status
# Consolidate savings_status into meaningful categories, keeping "no known savings" as is
credit_risk_classification$savings_status <- factor(
  ifelse(credit_risk_classification$savings_status == "<100", "low savings",
         ifelse(credit_risk_classification$savings_status == ">=100" & credit_risk_classification$savings_status != ">=500" & credit_risk_classification$savings_status != ">=1000", "moderate savings",
                ifelse(credit_risk_classification$savings_status == ">=500" & credit_risk_classification$savings_status != ">=1000", "substantial savings",
                       ifelse(credit_risk_classification$savings_status == ">=1000", "high savings",
                              as.character(credit_risk_classification$savings_status)))
         )
  )
)

# 6b. Grouping checking_status
# Consolidate checking_status into meaningful categories, keeping "no checking" as is
credit_risk_classification$checking_status <- factor(
  ifelse(credit_risk_classification$checking_status == "<0", "overdrawn",
         ifelse(credit_risk_classification$checking_status == "0<=x<200", "low balance",
                ifelse(credit_risk_classification$checking_status == ">=200", "adequate balance", 
                       "no checking")))
)


# 6c. Grouping employment
# Consolidate employment into meaningful categories
credit_risk_classification$employment <- factor(
  ifelse(credit_risk_classification$employment == "unemployed", "unemployed",
         ifelse(credit_risk_classification$employment == "<1", "less than 1 year",
                ifelse(credit_risk_classification$employment == "1<=X<4", "1-3 years",
                       ifelse(credit_risk_classification$employment == "4<=X<7", "4-6 years",
                              ifelse(credit_risk_classification$employment == ">=7", "7 or more years", 
                                     as.character(credit_risk_classification$employment)))
                )
         )
  )
)

# 7. Change empty strings in 'other_payment_plans' to unknown
# Add "unknown" as a valid level to the factor
levels(credit_risk_classification$other_payment_plans) <- c(levels(credit_risk_classification$other_payment_plans), "unknown")

# Replace empty strings with "unknown"
credit_risk_classification$other_payment_plans[credit_risk_classification$other_payment_plans == ""] <- "unknown"

# 8. Check for outliers in 'duration', 'credit_amount', 'age'
find_outliers <- function(column, data) {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR_value <- IQR(column)
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  filtered_data <- data[column >= lower_bound & column <= upper_bound, ]
  return(filtered_data)
}

# Apply the function to each column
credit_risk_classification <- find_outliers(credit_risk_classification$duration, credit_risk_classification)
credit_risk_classification <- find_outliers(credit_risk_classification$credit_amount, credit_risk_classification)
credit_risk_classification <- find_outliers(credit_risk_classification$age, credit_risk_classification)

# Save the cleaned data to a new CSV file
write.csv(credit_risk_classification, "cleaned_credit_risk_classification.csv", row.names = FALSE)