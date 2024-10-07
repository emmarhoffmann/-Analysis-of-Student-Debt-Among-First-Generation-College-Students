---
title: "Investigating Student Debt Among First-Generation College Students"
author: "Emma Hoffmann"
date: "2023-09-05"
output: html_document
---

```{r}
```

# Question 1: What is the number of colleges and average debt accumulation amounts within each quarterly percentile of first-generation students? 

# Load dplyr package
install.packages("dplyr")
library(dplyr)

# Calculate quartiles for the "FirstGen" variable, ignoring missing values
quartiles <- quantile(CollegeScores4yr$FirstGen, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

# Create a new variable to categorize colleges into percentiles
CollegeScores4yr <- CollegeScores4yr %>%
  mutate(FirstGenPercentile = case_when(
    FirstGen <= quartiles[2] ~ "0-25%",
    FirstGen <= quartiles[3] ~ "25-50%",
    FirstGen <= quartiles[4] ~ "50-75%",
    TRUE ~ "75-100%"
  ))

# Group the data by percentile and calculate the average debt and count
summary_table <- CollegeScores4yr %>%
  group_by(FirstGenPercentile) %>%
  summarise(Count = n(),
            AvgDebt = mean(Debt, na.rm = TRUE))

# Print the summary table
print(summary_table)

```{r}
```

# Question 2: How does the variation in student debt levels differ among different categories of percentages of first-generation students? 

# Load libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Read data from the Excel file
CollegeScores4yr <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 1/CollegeScores4yr.xlsx")

# Define the number of bins or ranges for first-generation student percentages
num_bins <- 4 

# Create a new column that categorizes first-generation student percentages into bins
CollegeScores4yr <- CollegeScores4yr %>%
  mutate(FirstGenCategory = cut(FirstGen, breaks = num_bins))

# Create a box plot to visualize the variation in student debt levels by first-generation student percentage
ggplot(CollegeScores4yr, aes(x = FirstGenCategory, y = Debt, fill = FirstGenCategory)) +
  geom_boxplot() +
  labs(title = "Variation in Student Debt Levels by First-Generation Student Percentage",
       x = "First-Generation Student Percentage Category",
       y = "Student Debt",
       fill = "First-Generation Student Percentage Category") +
  scale_fill_brewer(palette = "YlOrRd") +  # Use different shades of blue
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Center the title

```{r}
```

# Question 3: What is the frequency distribution of the percentages of first-generation students, and what are the median and mean percentage values?

# Load libraries
library(readxl)
library(ggplot2)

# Read data from the Excel file
CollegeScores4yr <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 1/CollegeScores4yr.xlsx")

# Calculate mean and median of the "FirstGen" values
mean_firstgen <- round(mean(CollegeScores4yr$FirstGen, na.rm = TRUE), 1)
median_firstgen <- median(CollegeScores4yr$FirstGen, na.rm = TRUE)

# Print mean and median values to the console
cat("Median:", median_firstgen, "\n")
cat("Mean:", mean_firstgen, "\n")

# Create a histogram of the "FirstGen" values
ggplot(CollegeScores4yr, aes(x = FirstGen)) +
  geom_histogram(binwidth = 5, fill = "palegreen3", color = "black") +
  labs(title = "Frequency Distribution of First-Generation Student Percentage",
       x = "First-Generation Student Percentage ",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

```{r}
```

# Question 4: Based on the above question, distributing the top and bottom percent of first-generation students upon the median value of 33.8, how does the average student debt among the bottom and top 50% of first-generation students compare? 

# Load dplyr and ggplot2 packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(dplyr)
library(ggplot2)

# Filter out rows with missing values in the "Debt" and "FirstGen" columns
CollegeScores4yr <- CollegeScores4yr %>%
  filter(!is.na(Debt) & !is.na(FirstGen))

# Categorize colleges into bottom 50% and top 50% based on FirstGen threshold
CollegeScores4yr <- CollegeScores4yr %>%
  mutate(`First-Generation Students` = ifelse(FirstGen <= 33.8, "Bottom 50%", "Top 50%"))

# Calculate the average debt for each category
average_debt <- CollegeScores4yr %>%
  group_by(`First-Generation Students`) %>%
  summarise(`Average Student Debt` = mean(Debt))

# Print the average student debt for each category
cat("Average Student Debt for Bottom 50%:", average_debt$`Average Student Debt`[1], "\n")
cat("Average Student Debt for Top 50%:", average_debt$`Average Student Debt`[2], "\n")

# Create a bar plot to visualize the average debt in bottom 50% and top 50%
ggplot(average_debt, aes(x = `First-Generation Students`, y = `Average Student Debt`, fill = `First-Generation Students`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Student Debt Comparison: Bottom 50% vs. Top 50% of First-Generation Students",
       x = "First-Generation Students",
       y = "Average Student Debt") +
  scale_fill_manual(values = c("Bottom 50%" = "yellow", "Top 50%" = "red")) +  # Set fill colors
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

```{r}
```

# Question 5: Is there a positive or negative correlation between the percentage of first-generation students and the debt accumulated? 

# Load libraries
library(readxl)
library(ggplot2)

# Read data from the new Excel file
CollegeScores4yr <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 1/CollegeScores4yr.xlsx")

# Calculate the correlation coefficient between FirstGen and Debt
correlation_firstgen_debt <- cor(CollegeScores4yr$FirstGen, CollegeScores4yr$Debt, use = "complete.obs")

# Create a scatter plot with palegreen3-colored dots
ggplot(CollegeScores4yr, aes(x = FirstGen, y = Debt)) +
  geom_point(color = "palegreen3") + 
  labs(x = "First-Generation Students Percentage", y = "Student Debt") +
  ggtitle("First-Generation Students Percentage vs. Student Debt") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme(plot.title = element_text(hjust = 0.5))

# Print the correlation coefficient to the console
cat("Correlation between First-Generation Students Percentage and Student Debt:", round(correlation_firstgen_debt, 2), "\n")

```{r}
```

# QUestion 6: Amongst median income and student debt, is there a connection involving the percentages of first-generation college students? 

# Load libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Read data from the Excel file
CollegeScores4yr <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 1/CollegeScores4yr.xlsx")

# Create a scatterplot to visualize the relationship between MedIncome, FirstGen, and Debt
scatterplot <- ggplot(CollegeScores4yr, aes(x = MedIncome, y = Debt, color = as.numeric(FirstGen))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2) +  # Increase line thickness
  labs(title = "Median Income vs. Student Debt by First-Generation Status",
       x = "Median Income",
       y = "Student Debt",
       color = "First Generation Status") +
  scale_color_gradient(low = "yellow", high = "red") +
  xlim(0, 150) +  # Set x-axis limit to 150
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Print the scatterplot
print(scatterplot)

# Calculate correlation between Net Price and Debt for first-generation students
correlation_medincome_debt_fg <- cor(CollegeScores4yr$MedIncome, CollegeScores4yr$Debt, use = "complete.obs")

# Round and print the correlation to the nearest thousandth
rounded_correlation <- round(correlation_medincome_debt_fg, digits = 3)
cat("Correlation between Median Income and Student Debt for First-Generation Students:", rounded_correlation, "\n")

```{r}
```

# Question 7: Amongst the net price of attendance and student debt, is there a connection involving the percentages of first-generation college students? 
 
# Load libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Read data from the Excel file
CollegeScores4yr <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 1/CollegeScores4yr.xlsx")

# Create a scatterplot to visualize the relationship between NetPrice, FirstGen, and Debt
scatterplot <- ggplot(CollegeScores4yr, aes(x = NetPrice, y = Debt, color = as.numeric(FirstGen))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2) +  # Increase line thickness
  labs(title = "Net Price vs. Student Debt by First-Generation Status",
       x = "Net Price",
       y = "Student Debt",
       color = "First Generation Status") +
  scale_color_gradient(low = "yellow", high = "red") +
  xlim(0, 65000) +  # Set x-axis limit to 65000
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Print the scatterplot
print(scatterplot)

# Calculate correlation between NetPrice and Debt for first-generation students
correlation_netprice_debt_fg <- cor(CollegeScores4yr$NetPrice, CollegeScores4yr$Debt, use = "complete.obs")

# Round and print the correlation to the nearest thousandth
rounded_correlation <- round(correlation_netprice_debt_fg, digits = 3)
cat("Correlation between Net Price and Student Debt for First-Generation Students:", rounded_correlation, "\n")

```{r}
```

# Question 8: Amongst the enrollment size and student debt, is there a connection involving the percentages of first-generation college students?   

# Load libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Read data from the Excel file
CollegeScores4yr <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 1/CollegeScores4yr.xlsx")

# Create a scatterplot to visualize the relationship between Enrollment, FirstGen, and Debt
scatterplot <- ggplot(CollegeScores4yr, aes(x = Enrollment, y = Debt, color = as.numeric(FirstGen))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2) +  # Increase line thickness
  labs(title = "Enrollment vs. Student Debt by First-Generation Status",
       x = "Enrollment Size",
       y = "Student Debt",
       color = "First Generation Status") +
  scale_color_gradient(low = "yellow", high = "red") +
  xlim(0, 80000) +  # Set x-axis limit to 80000
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Print the scatterplot
print(scatterplot)

# Calculate correlation between Enrollment and Debt for first-generation students
correlation_enrollment_debt_fg <- cor(CollegeScores4yr$Enrollment, CollegeScores4yr$Debt, use = "complete.obs")

# Round and print the correlation to the nearest thousandth
rounded_correlation <- round(correlation_enrollment_debt_fg, digits = 3)
cat("Correlation between Enrollment and Student Debt for First-Generation Students:", rounded_correlation, "\n")

```{r}
```

# Question 9: What is the mean debt for colleges located in different locales for each the top and bottom 50% of first generation students?

# Load libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Read data from the Excel file
CollegeScores4yr <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 1/CollegeScores4yr.xlsx")

# Calculate quartiles for the "FirstGen" variable, ignoring missing values
quartiles <- quantile(CollegeScores4yr$FirstGen, probs = c(0, 0.5, 1), na.rm = TRUE)

# Create a new column 'FirstGenPercentile' based on quartiles
CollegeScores4yr$FirstGenPercentile <- cut(CollegeScores4yr$FirstGen, breaks = quartiles, labels = c("Bottom 50%", "Top 50%"), include.lowest = TRUE)

# Remove rows with missing values
CollegeScores4yr <- na.omit(CollegeScores4yr)

# Calculate mean debt for the bottom 50% of first-generation students by Locale
bottom_50_mean_debt <- CollegeScores4yr %>%
  filter(FirstGenPercentile == "Bottom 50%") %>%
  group_by(Locale) %>%
  summarize(Mean_Debt = round(mean(Debt, na.rm = TRUE)))

# Calculate mean debt for the top 50% of first-generation students by Locale
top_50_mean_debt <- CollegeScores4yr %>%
  filter(FirstGenPercentile == "Top 50%") %>%
  group_by(Locale) %>%
  summarize(Mean_Debt = round(mean(Debt, na.rm = TRUE)))

# Merge the two summary tables by Locale
result_table <- merge(bottom_50_mean_debt, top_50_mean_debt, by = "Locale", all = TRUE)

# Rename columns to remove underscores
colnames(result_table) <- c("  Locale", "  Mean Debt Bottom 50%", "  Mean Debt Top 50%")

# Print the table
print(result_table)

```{r}
```

# Question 10: How does the relationship between the percent of first-generation students and average debt differ across states? 

# Load libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Read data from the Excel file
CollegeScores4yr <- read_excel("C:/Users/erhof/OneDrive/Desktop/OneDrive/SCHOOL - FALL 23/STAT 353/PROJECT 1/CollegeScores4yr.xlsx")

# Calculate quartiles for the "FirstGen" variable, ignoring missing values
quartiles <- quantile(CollegeScores4yr$FirstGen, probs = c(0, 0.5, 1), na.rm = TRUE)

# Create a new column 'FirstGenPercentile' based on quartiles
CollegeScores4yr$FirstGenPercentile <- cut(CollegeScores4yr$FirstGen, breaks = quartiles, labels = c("Bottom 50%", "Top 50%"), include.lowest = TRUE)

# Calculate mean debt and mean percentage of first-generation students by state
mean_debt_by_state <- CollegeScores4yr %>%
  group_by(State) %>%
  summarize(`mean debt` = mean(Debt, na.rm = TRUE))

mean_firstgen_by_state <- CollegeScores4yr %>%
  group_by(State) %>%
  summarize(`mean first-generation` = mean(FirstGen, na.rm = TRUE))

# Print the top 3 states with the highest mean debt
cat("Top 3 states with the highest mean debt:\n")
top_mean_debt_states <- mean_debt_by_state %>%
  arrange(desc(`mean debt`)) %>%
  head(3)
print(top_mean_debt_states)

# Print the top 3 states with the lowest mean debt
cat("\nTop 3 states with the lowest mean debt:\n")
bottom_mean_debt_states <- mean_debt_by_state %>%
  arrange(`mean debt`) %>%
  head(3)
print(bottom_mean_debt_states)

# Print the top 3 states with the highest mean percentage of first-generation students
cat("\nTop 3 states with the highest mean percentage of first-generation students:\n")
top_mean_firstgen_states <- mean_firstgen_by_state %>%
  arrange(desc(`mean first-generation`)) %>%
  head(3)
print(top_mean_firstgen_states)

# Print the top 3 states with the lowest mean percentage of first-generation students
cat("\nTop 3 states with the lowest mean percentage of first-generation students:\n")
bottom_mean_firstgen_states <- mean_firstgen_by_state %>%
  arrange(`mean first-generation`) %>%
  head(3)
print(bottom_mean_firstgen_states)

# Create a scatterplot for each state
ggplot(CollegeScores4yr, aes(x = FirstGen, y = Debt)) +
  geom_point() +
  facet_wrap(~State) +
  labs(title = "First-Generation Students Percentage vs. Student Debt by State",
       x = "First-Generation Students Percentage",
       y = "Student Debt") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

```{r}
``