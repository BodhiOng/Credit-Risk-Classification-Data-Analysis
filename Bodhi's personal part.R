# Install neccessary libraries
install.packages("ggplot2")

# Load necessary libraries
library(ggplot2)

# Load the dataset
data <- read.csv("cleaned_credit_risk_classification.csv")

########## 3.1 Analysis ########## 
########## Analysis 1: Examine the frequency of checking status across good and bad credit risk classes ########## 

# Step 1: Calculate the frequency of checking_status across class
freq_table <- table(data$checking_status, data$class)

# Step 2: Convert the table to a data frame for easier plotting
freq_df <- as.data.frame(freq_table)
colnames(freq_df) <- c("Checking Status", "Credit Risk", "Count")

# Step 3: Visualize the distribution using a bar chart & pie chart
ggplot(freq_df, aes(x = `Checking Status`, y = Count, fill = `Credit Risk`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Frequency of Checking Status Across Credit Risk",
       x = "Checking Status",
       y = "Count") +
  theme_minimal()

########## Analysis 2: Compare proportions of good and bad credit risk across checking status ########## 

# Step 1: Calculate the proportions of good and bad credit risk within each checking status category
prop_table <- prop.table(freq_table, margin = 1)

# Convert the proportions table to a data frame for visualization
prop_df <- as.data.frame(prop_table)
colnames(prop_df) <- c("Checking Status", "Credit Risk", "Proportion")

# Step 2: Visualize the proportions using a stacked bar chart
ggplot(prop_df, aes(x = `Checking Status`, y = Proportion, fill = `Credit Risk`)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Good vs. Bad Credit Risk by Checking Status",
       x = "Checking Status",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########## 3.1.4 Hypothesis ########## 
########## Hypothesis Testing ########## 
# Step 1: Create a contingency table for checking_status and class
contingency_table <- table(data$checking_status, data$class)

# Step 2: Perform a Chi-square test of independence
chi_square_test <- chisq.test(contingency_table)

# Step 3: Output the results
cat("Chi-square Test Results:\n")
print(chi_square_test)