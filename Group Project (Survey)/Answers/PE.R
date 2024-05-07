library(readr)

CleanedData <- data <- read_csv("Group Project (Survey)/data.csv")
view(CleanedData)

#Performance Expectancy 1
PE1 <- data$`using Canva for school-related work helps ME complete assignments more quickly.`
length(PE1)
PE1 <- likert_to_numeric(PE1)

# Calculate mean and standard deviation
PE1_Mean <- mean(PE1, na.rm = TRUE)
PE1_SD <- sd(PE1, na.rm = TRUE)
PE1_Mean
PE1_SD

#Performance Expectancy 2
PE2 <- data$`using Canva to organize My schoolwork makes Me more productive.`
length(PE2)

PE2 <- likert_to_numeric(PE2)

# Calculate mean and standard deviation
PE2_Mean <- mean(PE2, na.rm = TRUE)
PE2_SD <- sd(PE2, na.rm = TRUE)
PE2_Mean
PE2_SD

#Performance Expectancy 3
PE3 <- data$`If I use canva for My school projects, It would increase My chances of receiving praise or succeeding academically.`
length(PE3)

PE3 <- likert_to_numeric(PE3)

# Calculate mean and standard deviation
PE3_Mean <- mean(PE3, na.rm = TRUE)
PE3_SD <- sd(PE3, na.rm = TRUE)
PE3_Mean
PE3_SD

merged_mean <- mean(c(PE1_Mean,PE2_Mean, PE3_Mean), na.rm = TRUE)

merged_sd <- sqrt(mean(c(PE1_SD^2,PE2_SD^2, PE3_SD^2), na.rm = TRUE))

print(merged_mean)
print(merged_sd)

summary_data <- data.frame(
  Code = c("PE1", "PE2", "PE3"),
  Mean = c(PE1_Mean, PE2_Mean, PE3_Mean),
  SD = c(PE1_SD, PE2_SD, PE3_SD)
)

# Save the data frame as a CSV file
write.csv(summary_data, "PE.csv", row.names = FALSE)
