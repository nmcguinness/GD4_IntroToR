
#' @title 7_PerformingSurveyAnalysis.R
#' @description This script demonstrates how to load and analyze survey data using basic descriptive statistics in R.
#' @author NMCG
#' @bugs None
#' @keywords summary, mean, median, standard deviation, correlation
#' @seealso https://s3.amazonaws.com/assets.datacamp.com/email/other/Data+Visualizations+-+DataCamp.pdf
#' @seealso https://youtu.be/HPJn1CMvtmI
#' @seealso https://r4ds.had.co.nz/

# Clear Console -----------------------------------------------------------
# Clear the console (helps keep output clean)
cat("\014")

# Load required packages --------------------------------------------------

if (!require("tidyverse")) 
  install.packages("tidyverse")
# Load tidyverse for data wrangling and visualization
library(tidyverse)

# Clear the console (equivalent to Ctrl+L in RStudio) --------------------
cat("\014")

# Load Data ---------------------------------------------------------------

# NOTE: Adjust the file path if needed
# Load the survey dataset (adjust path if needed)
survey_data <- read.csv("data/2022_23_streaming_survey_responses.csv", stringsAsFactors = FALSE)

# View first few rows of the dataset
# Peek at the first few rows to understand data layout
head(survey_data)

# View structure and column names
# View the structure: shows data types and preview of each column
str(survey_data)
# List column names to reference in analysis
names(survey_data)

# Descriptive Stats -------------------------------------------------------

# Summary of numeric columns
# Descriptive statistics for numeric variables
summary(select(survey_data, Age, Subscriptions, Hours.per.Week, Last.Binge.Session..episodes.movies.,
               Would.Recommend..1.10., Mood.Matching.Importance))

# Means and Medians
# Calculate mean of 'Age'
mean_age <- mean(survey_data$Age)
# Calculate median of weekly streaming hours
median_hours <- median(survey_data$Hours.per.Week)

print(paste("Mean Age:", mean_age))
print(paste("Median Streaming Hours per Week:", median_hours))

# Standard Deviation
# Calculate standard deviation of weekly hours
sd_streaming <- sd(survey_data$Hours.per.Week)
print(paste("Standard Deviation of Hours per Week:", round(sd_streaming, 2)))

# Frequency Table for Categorical Variables -------------------------------

# Frequency table for main streaming platforms
platform_freq <- table(survey_data$Main.Platform)
print("Platform usage frequency:")
print(platform_freq)

# Frequency table for binge behavior (more than 5 episodes)
binge_freq <- table(survey_data$Binged.Over.5)
print("Binge (>5 episodes) frequency:")
print(binge_freq)

# Correlation Matrix for Likert Responses ---------------------------------

likert_data <- select(survey_data,
                      Recommends.Good.Shows,
                      Interface.Usability,
                      Feels.Overwhelmed,
                      Helps.Me.Relax)

# Correlation matrix for Likert scale responses
cor_matrix <- cor(likert_data)
print("Correlation Matrix for Likert Items:")
print(round(cor_matrix, 2))

# Visualisation Prep ------------------------------------------------------

# Histogram of weekly streaming hours using base R
hist(survey_data$Hours.per.Week,
     breaks = 10,
     col = 'steelblue',
     border = 'white',
     main = 'Distribution of Weekly Streaming Hours',
     xlab = 'Hours per Week',
     ylab = 'Frequency')

# Boxplot of Age by Main Platform using base R
boxplot(survey_data$Age ~ survey_data$Main.Platform,
        data = survey_data,
        col = 'orange',
        main = 'Age Distribution by Platform',
        ylab = 'Age')

# Scatter plot using base R
plot(survey_data$Mood.Matching.Importance,
     survey_data$Helps.Me.Relax,
     pch = 19,
     col = 'darkgreen',
     main = 'Mood Matching vs Relaxation',
     xlab = 'Mood Matching (1–10)',
     ylab = 'Relaxation Rating (1–5)')
grid(lty='twodash')

# Bar plot of Main Platform usage
barplot(table(survey_data$Main.Platform),
        col = 'skyblue',
        main = 'Most Used Streaming Platforms',
        ylab = 'Number of Users',
        las = 2)

# Pie chart of binge behavior
binge_counts <- table(survey_data$Binged.Over.5)
pie(binge_counts,
    col = c('lightgreen', 'tomato'),
    main = 'Proportion of Users Who Binge (>5 Episodes)')

# Boxplot comparing mood matching importance by binge behavior
boxplot(survey_data$Mood.Matching.Importance ~ survey_data$Binged.Over.5,
        data = survey_data,
        col = c('violet', 'lightcoral'),
        main = 'Mood Matching by Binge Watching Habits',
        xlab = 'Binged Over 5 Episodes?',
        ylab = 'Mood Matching Importance (1–10)')


# Strip chart of hours per week
stripchart(survey_data$Hours.per.Week,
           method = 'jitter',
           pch = 21,
           col = 'blue',
           bg = 'lightblue',
           main = 'Streaming Hours per Week',
           xlab = 'Hours')


# Enhanced Visualisation Prep ------------------------------------------------------

# Enhanced Strip Chart of Streaming Hours per Week
stripchart(survey_data$Hours.per.Week,
           method = "jitter",
           pch = 21,
           col = "blue",
           bg = "lightblue",
           jitter = 0.2,
           main = "Streaming Hours per Week (Individual Respondents)",
           xlab = "Hours Watched per Week",
           ylab = "Respondents (jittered for clarity)",
           xlim = c(0, max(survey_data$Hours.per.Week, na.rm = TRUE))
)

# Add horizontal reference line at median
abline(h = 1, col = "darkgray", lty = "dashed")

# Add annotation text
mtext("Each point represents one survey response.Jitter prevents overlap.", side = 3, line = 0.5, cex = 0.8)

# Enhanced Histogram of Weekly Streaming Hours
hist(survey_data$Hours.per.Week,
     breaks = 20,
     col = "steelblue",
     border = "white",
     main = "Distribution of Weekly Streaming Hours",
     xlab = "Hours Watched per Week (grouped into 2-hour bins)",
     ylab = "Number of Respondents",
     xlim = c(0, 25),
     ylim = c(0, 8)  
)

# Add gridlines for readability
grid(col = "lightgray", 
     lty = "longdash")

# Annotate the plot with additional info
mtext("Note: Data collected from 2022/23 streaming survey (n = 46)", side = 1, line = 2, cex = 0.8)

