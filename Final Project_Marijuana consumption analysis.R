rm(list=ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(tableone)
# Install the agricolae package if not already installed
install.packages("agricolae")
install.packages("rcompanion")
library(rcompanion)

# Load the package
library(agricolae)

file_path <- "C:/Users/user/Desktop/YU- semester_1/Computational_stats/Final Project_1st-Draft/Dataset/NHANES_Marijuana study_ Dataset.xlsx"
data <- read_excel(file_path, sheet = 1) 
ls(data)
data <- data[, c('Gender', 'Age', 'AgeDecade', 'Race1', 'HHIncomeMid', 
                 'BMI', 'BMI_WHO', 'HealthGen', 'Depressed', 
                 'SleepTrouble', 'Alcohol12PlusYr', 'AgeFirstMarij', 'RegularMarij')]
str(data)
data$Gender[data$Gender == 'female'] <- 0
data$Gender[data$Gender == 'male'] <- 1
data$Gender <- factor(data$Gender, levels = c(0, 1), labels = c('female', 'male'))

# Remove rows with missing or non-finite BMI values
data_clean <- data[is.finite(data$BMI) & !is.na(data$BMI), ]
data_clean$RegularMarij <- factor(data_clean$RegularMarij, levels = c('No', 'Yes'))
data_clean <- na.omit(data_clean)
data_clean$Depressed <- factor(data_clean$Depressed, levels = c('None', 'Several', 'Most'), labels = c(0, 1, 2))
data_clean$DepressedBinary <- ifelse(data_clean$Depressed == 0, "Not Depressed", "Depressed")

data <- data_clean
str(data)
summary(data$Gender)
bmi_stats <- data %>%
  group_by(RegularMarij, Gender, Race1, AgeDecade) %>%
  summarise(mean_BMI = mean(BMI, na.rm = TRUE),
            count = n())
print(bmi_stats)

vars <- c('BMI', 'Gender', 'Race1', 'AgeDecade')
# Define factor variables
factorVars <- c('Gender', 'Race1', 'AgeDecade', 'RegularMarij')
# Create the summary table (stratified by RegularMarij, i.e., marijuana smokers and non-smokers)
table1 <- CreateTableOne(vars = vars, strata = "RegularMarij", data = data, factorVars = factorVars)

# Print the summary table
print(table1)
# Boxplot of BMI by Marijuana Use
ggplot(data_clean, aes(x = factor(RegularMarij, labels = c("Non-Smoker", "Smoker")), y = BMI, fill = RegularMarij)) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Marijuana Use",
       x = "Marijuana Use", y = "BMI") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen"))

ggplot(data_clean, aes(x = BMI, fill = RegularMarij)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(title = "BMI Distribution by Marijuana Use",
       x = "BMI", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen"))

# Bar Plot for Gender Distribution by Marijuana Use
ggplot(data_clean, aes(x = RegularMarij, fill = Gender)) +
  geom_bar(position = "fill") +
  labs(title = "Gender Distribution by Marijuana Use",
       x = "Marijuana Use", y = "Proportion") +
  theme_minimal() +
  scale_fill_manual(values = c("pink", "blue"))

# Convert Depressed to factor with ordered levels (None, Several, Most)
data_clean$Depressed <- factor(data_clean$Depressed, levels = c(0, 1, 2), labels = c("None", "Several", "Most"))

# Stacked bar plot of depression severity between smokers and non-smokers
ggplot(data_clean, aes(x = RegularMarij, fill = Depressed)) +
  geom_bar(position = "fill") +
  labs(title = "Depression Severity among Marijuana Smokers and Non-Smokers",
       x = "Marijuana Use (Smoker vs. Non-Smoker)",
       y = "Proportion",
       fill = "Depression Severity") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "orange", "red"))

#Data Visualization
# Histogram for age distribution
ggplot(data_clean, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()
# Bar Plot for gender
ggplot(data_clean, aes(x = Gender)) +
  geom_bar(fill = "coral", color = "black") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal()
#Bar-plot for race
ggplot(data_clean, aes(x = Race1)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Race Distribution", x = "Race", y = "Count") +
  theme_minimal()
# Marijuana use by demographics (age, gender, race, age group)
#By Gender
ggplot(data_clean, aes(x = Gender, fill = RegularMarij)) +
  geom_bar(position = "fill") +
  labs(title = "Marijuana Use by Gender", x = "Gender", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
#By age distribution
ggplot(data_clean, aes(x = RegularMarij, y = Age, fill = RegularMarij)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Age Distribution by Marijuana Use (Violin Plot)", x = "Marijuana Use", y = "Age") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "coral"), name = "Marijuana Use")
#By race
ggplot(data_clean, aes(x = Race1, fill = RegularMarij)) +
  geom_bar(position = "fill") +
  labs(title = "Marijuana Use by Race", x = "Race", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()
# Camparison visualization
#BMI
ggplot(data_clean, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "BMI Distribution", x = "BMI", y = "Count") +
  theme_minimal()
#BMI distribution between smokers and non smokers
ggplot(data_clean, aes(x = RegularMarij, y = BMI, fill = RegularMarij)) +
  geom_boxplot() +
  labs(title = "BMI Comparison by Marijuana Use", x = "Marijuana Use", y = "BMI") +
  theme_minimal()
# General Health distribution comparison with Marijuana use
ggplot(data_clean, aes(x = HealthGen, fill = RegularMarij)) +
  geom_bar(position = "fill") +
  labs(title = "Self-Reported Health by Marijuana Use", x = "Health Status", y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

#T test for difference between BMI in smokers and non-smokers
test1 <- t.test(BMI ~ RegularMarij, data = data_clean)
print(t)

#Chi-square test "Marijuana use" and "Depression" using a 2x2 contingency table
# Create the contingency table
contingency_table <- table(data_clean$RegularMarij, data_clean$Depressed)
dimnames(contingency_table) <- list("Marijuana Use" = c("No", "Yes"), 
                                    "Depression Level" = c("None", "Several", "Most"))
print(contingency_table)
chisq.test(table(data_clean$RegularMarij, data_clean$Depressed))
# post-hoc testing to calculate odds ratio
posthoc_result <- pairwise.chisq.test(table(data_clean$RegularMarij, data_clean$Depressed), p.adjust.method = "bonferroni")
posthoc_result

# Logistic Regression to verify relationship between depression and marijuana use
data_clean$DepressedBinary <- ifelse(data_clean$Depressed %in% c(0, 1), 0, 1)
data_clean$Age <- scale(data_clean$Age)
data_clean$HHIncomeMid <- scale(data_clean$HHIncomeMid)
# Try logistic regression with fewer predictors
#logistic_model <- glm(DepressedBinary ~ RegularMarij + Age, data = data_clean, family = binomial)
#summary(logistic_model)

install.packages("logistf")
library(logistf)
# Create a logistic regression model
# Running the logistic regression with increased max iterations
full_model <- logistf(formula = DepressedBinary ~ RegularMarij + Age + Gender, 
                      data = data_clean, control = logistf.control(maxit = 100))

summary(full_model)


library(DT)
datatable(your_table)
