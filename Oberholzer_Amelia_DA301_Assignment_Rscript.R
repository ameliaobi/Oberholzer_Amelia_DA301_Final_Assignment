
# Import the tidyverse library.
library(tidyverse)
library('dplyr')
library(tidyr)
require(lattice)
library(magrittr)
library(plotly)
library(patchwork)
library(lubridate)
library(ggplot2)
library (moments)
library(psych)

# Set the plots margin size
par(mfrow=c(1,1))



# Import the data set.

# Import a CSV file.
data <- read.csv(file.choose(), header=T)

# Print the data frame.
data
View(data)

# Identifying null values 
sum(is.na(data$size))

# Quickly identify max and min of global sales
summary(data)

# Use the glimpse() function.
glimpse(data)

# Changing the date
data$Year <- lubridate::ymd(data$Year, truncated = 2L)

# Checking data types
str(data)

# Now I'm going to get an overview of the sales data 
# Drop Ranking, Year, Genre and Publisher columns 
data_sales <- select(data, -Ranking, -Year, -Genre, -Publisher)

# See some histograms to show the distribution of sales
# In different regions

hist(data_sales$NA_Sales,
     xlim = c(0, 50),
     breaks = 9,
     main = "Histrogram - Sales in North America",
     xlab = "",
     col = "red")

hist(data_sales$EU_Sales,
     xlim = c(0, 50),
     breaks = 9,
     main = "Histrogram - Sales in EU",
     xlab = "",
     col = "Blue")

hist(data_sales$Global_Sales,
     xlim = c(0, 50),
     breaks = 9,
     main = "Histogram - Global Sales",
     xlab = "",
     col = "Green")

# Distribution of sales in EU, NA and the World

# Most basic scatter chart
NA_Sales_distribution <- ggplot(data_sales, aes(x=Product, y=NA_Sales)) +
                         geom_point(color="red",size=2) +
                         ggtitle("Scatterplor - North America Sales") 

EU_Sales_distribution <- ggplot(data_sales, aes(x=Product, y=EU_Sales)) +
                         geom_point(color="Blue",size=2) +
                         ggtitle("Scatter Plot - EU Sales") 

Global_Sales_distribution <- ggplot(data_sales, aes(x=Product, y=Global_Sales)) +
                             geom_point(color="green",size=2) +
                             ggtitle("Scatterplot - Global Sales") 

NA_Sales_distribution + EU_Sales_distribution + Global_Sales_distribution

# Viewing the count of sales for a certain game 

# Creating a scatter plot for this information

global_sales_plot <- ggplot(sum_sales_product_global, 
                     aes(x = Product, y = Total, colour = Platform)) +
                     geom_point() +
                     theme(axis.text.x = element_text(angle = 90)) +
                     ggtitle("Total Global Sales of Product ID")


global_sales_plot

# Here we can see which products have higher sales recorded
# depending on their sales platform. 
# Interestingly will has the higest sale of a certain prodcut
# Almost by double all other products 

# Lets break this plot down

# Top 50 and their platform 

ordered_scatter_product = product_analysis[order(product_analysis$Total, decreasing = TRUE),]  
ordered_scatter_product

ordered_scatter_product_top <- head(ordered_product, 50)

global_sales_plot_top <- ggplot(ordered_scatter_product_top, 
                         mapping = aes(x = Product, y = Total, colour = Platform)) +
                         geom_point() +
                         theme(axis.text.x = element_text(angle = 90)) +
                         ggtitle("Total Global Sales of Top 50")
  
# Bottom 50 and their platform 

ordered_scatter_product = product_analysis[order(product_analysis$Total, decreasing = TRUE),]  
ordered_scatter_product

ordered_scatter_product_bottom <- tail(ordered_product, 50)

global_sales_plot_bottom <- ggplot(ordered_scatter_product_bottom, 
                            mapping = aes(x = Product, y = Total, colour = Platform)) +
                            geom_point() +
                            theme(axis.text.x = element_text(angle = 90)) +
                            ggtitle("Total Global Sales of Product ID - Bottom 50")

global_sales_plot_bottom + global_sales_plot_top

# We see here how the bottom and top plots compare. Interestingly PC
# Have the lowest sales performance 
# Whereas Will, NES and GB have higher sales performance

# It is interesting to see how some platforms 
# Are more popular in different regions 

# Barplot showing the total sales for certain platforms

global_sales_platfrom_plot <- ggplot(sum_sales_platform_global, 
                                     aes(x = Platform, y = Total)) +
  geom_bar(stat="identity", colour="green") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Total Global Sales for a Certain Platform")

global_sales_platfrom_plot


# Comparison of most sales in certain platform for EU versus NA Sales 

# Grouping sum of EU sales for different platforms 
sum_sales_platform_EU = data %>%
  group_by(Platform) %>%
  summarise(Total = sum(EU_Sales))

sum_sales_platform_EU 


# Grouping sum of NA sales for different platforms 
sum_sales_platform_NA = data %>%
  group_by(Platform) %>%
  summarise(Total = sum(NA_Sales))

sum_sales_platform_NA

# Creating Bar Plots 

EU_sales_platfrom_plot <- ggplot(sum_sales_platform_EU, 
                                 aes(x = Platform, y = Total)) +
  geom_bar(stat="identity", colour="blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Total EU  Sales for a Certain Platform")

EU_sales_platfrom_plot

# Bar plot for NA

NA_sales_platfrom_plot <- ggplot(sum_sales_platform_NA, 
                                 aes(x = Platform, y = Total)) +
  geom_bar(stat="identity", colour="red") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Total NA Sales for a Certain Platform") 
  
NA_sales_platfrom_plot

# Filtering for highest platform sales in EU and NA for a clear comparison

# First EU 

EU_highest_platform_sales <- sum_sales_platform_EU %>%
  arrange(desc(Total)) %>% 
  group_by(Platform)

EU_highest_platform_sales_top <- head(EU_highest_platform_sales ,5)
EU_highest_platform_sales_top

EU_highest_platform_sales_top_plot <- ggplot(EU_highest_platform_sales_top,
                                             aes(x = Platform, y = Total)) +
  geom_bar(stat="identity", colour="Blue") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Total Top EU  Sales for a Certain Platform")

EU_highest_platform_sales_top_plot

# Now US

NA_highest_platform_sales <- sum_sales_platform_NA %>%
  arrange(desc(Total)) %>% 
  group_by(Platform)

NA_highest_platform_sales_top <- head(NA_highest_platform_sales ,5)
NA_highest_platform_sales_top

NA_highest_platform_sales_top_plot <- ggplot(NA_highest_platform_sales_top,
                                             aes(x = Platform, y = Total)) +
  geom_bar(stat="identity", colour="Red") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Total Top NA Sales for a Certain Platform")

EU_highest_platform_sales_top_plot + NA_highest_platform_sales_top_plot

# Interestingly we see how the Wii is more popular in the EU
# Where as the xbox 360 is more popular in the US 

# Now we're going to observe how sales change over time
# using group_by to find the total global sales per year

# Analyzing individuals products 

# using group_by to find the total global sales per year

sum_sales_platform_global_year = data %>%
  group_by(Year, Platform) %>%
  summarise(Total = sum(Global_Sales))

sum_sales_platform_global_year

sum_sales_platform_global_year_plot <- ggplot(sum_sales_platform_global_year,
                                        aes(x = Year, y = Total)) +
                                        geom_line(stat="identity", colour="Green") +
                                        ggtitle("Total Top Global Sum of Over Time")

sum_sales_platform_global_year_plot 

# Interesting to see that we see seasonal spikes here
# Creating a time series to see seasonal spikes in more detail

# Convert the data into a time series.
# Create a new data frame and assign time series value,
# and specify the 'ts' function.

sum_sales_platform_global_year 

global_sales_time = sum_sales_platform_global_year[c("Year", "Total")]

global_sales_time

# Change the names of columns by specifying the new column names.
colnames(global_sales_time) <- c('date', 'index') 

global_sales_time

global_sales_time_ts_r <- ts(global_sales_time$index,
              start = c(2000, 1),
              # Monthly frequency without missing values in data.
              frequency = 12)

# Sense-check the new object.
# View the data by creating a smaller sample of the visualisation.
plot(global_sales_time_ts_r)


# View the data by creating a smaller sample of the visualisation.
plot(window(global_sales_time_ts_r, 2000, 2006))

# Having a look at seasonal trends in more detail

# Now looking how how the sales of certain platforms change over time

# Show top platforms to see sales changing over time 

top_sales_over_time <- sum_sales_platform_global_year [sum_sales_platform_global_year$Platform %in% c("DS","PS2", "PS3", "PS4", "Wii", "X360"),]

top_sales_over_time

top_sales_over_time_plot <- ggplot(top_sales_over_time,
                            aes(x = Year, y = Total, colour = Platform)) +
                            geom_line() +
                            ggtitle("Total Top Global Sum of Sales Over Time for Top Platforms")

top_sales_over_time_plot


# Now I'm going to check the normality of the data 
# It is possible to do this using a Q-Q plot

# Specify the qqnorm function.
# Draw a qqplot using the Global_Sales.
qqnorm(data$Global_Sales,
       col='green',
       xlab="z Value",
       ylab='Sales')

# Specify the qqline function.
# Add a reference line to the qqplot.
qqline(data$Global_Sales,
       col='black',
       lwd=2) 

# As we see data points deviate by large amount from the line
# Points should be lower according distribution
# Values in tails of the distribution, more extreme sales
# Are not as extreme as we would expect
# Lighter tales 

# Run a Shapiro-Wilk test:
shapiro.test(data$Global_Sales)

# Null hypothsis data is normally distributed
# Small P value we reject the null hypothesis
# There is a very small p value here 
# Here we see the data is not normally distributed 

# Specify the skewness and kurtosis functions.
skewness(data$Global_Sales) 

# Skewness of 4 large amount of positive skewness 
# If the skewness is between -0.5 and 0.5, the data are fairly symmetrical. 
# If the skewness is between -1 and – 0.5 or between 0.5 and 1, 
# the data are moderately skewed. 
# If the skewness is less than -1 or greater than 1, 
# the data are highly skewed.

# Positive Skewness means when 
# the tail on the right side of the distribution is 
# longer or fatter.
# Positive skewness could be caused by inequality of distribution 
# This means the more sales are distribution towards
# The lower end of the distribution 


kurtosis(data$Global_Sales)
# This measures whether tails are heavy or light tailed
# Normal has a kurtosis of 32
# Heavier tails than the normal 
# Some values much higher than predicted by the distribution 
# or their are some outliers


# Is there correlation between sales columns 
sales_data = data%>%
  select(EU_Sales, NA_Sales, Global_Sales)

cor(sales_data)

# See the relationship of correlation in a heat map
corPlot(sales_data, cex=2)


# Hard to see any relationship here
plot(sales_data$EU_Sales, sales_data$NA_Sales)

# A bit more of a relationships here 
plot(sales_data$EU_Sales, sales_data$Global_Sales)

# More of a relationship here 
plot(sales_data$NA_Sales, sales_data$Global_Sales)

# Makes sense that there is more of a relationship 
# Between Global Sales and EU and NA sales 
# Global Sales is calculated from EU and NA sales 

# Creating simple linear regression NA and EU Sales

NA_EU_Sales <- lm(EU_Sales~NA_Sales,
                  data=sales_data)

# lm is linear model
# Just one x variable 

# View the model.
NA_EU_Sales

summary(NA_EU_Sales)

# To some extent these numbers are correlated
# NA Sales accounts for 50% of the variance of EU Sales 

# The p is very small meaning we can reject the null hypthosis 
# That there is no correlation

# Add line-of-best-fit.
abline(coefficients(NA_EU_Sales))


# Moving on to the relationship between
# Global and NA Sales 

EU_Global_Sales <- lm(EU_Sales~Global_Sales,
                      data=sales_data)

# lm is linear model
# Just one x variable 

# View the model.
EU_Global_Sales

summary(EU_Global_Sales)

# P value is small rejecting null hypothesis of no correlation 
# R value is large, this shows that 77% of the variance of global sales 
# is explained by EU Sales

# Moving on to the relationship between
# Global and EU Sales 

NA_Global_Sales <- lm(NA_Sales~Global_Sales,
                      data=sales_data)

# lm is linear model
# Just one x variable 

# View the model.
NA_Global_Sales

summary(NA_Global_Sales)

# Again we see here that the P value is very small
# We can reject null hypothesis
# Were the of no correlation 
# 87% of global sales can be explained by NA sales 


# Multiple linear regression 

# Create a new object and 
# specify the lm function and the variables.
multi_regression = lm(Global_Sales~EU_Sales+NA_Sales, data=sales_data)

# Print the summary statistics.
summary(multi_regression)

# Combined NA and EU Sales account for 97% of the variation in global sales
# Confirmed by the low result of P we can reject null hypothesis of no correlation

# Predicting future global sales 
# Testing the model
sales_data


# Create a new object and specify the predict function.
predictTest = predict(multi_regression, newdata=sales_data,
                      interval='confidence')

# Print the object.
head(predictTest)

# Comparing sample to previous data

# You can also see the lower and upper values defining 
# the confidence interval in our predicted values. 
# The confidence interval in the predict function 
# will help us to gauge the uncertainty in the predictions. 

# Here we can make comparison of the accuracy of the model
# On data set if NA_Sales_sum is 34.02 and EU_Sales_sum is 23.80.
# The model predicts global sales will be 72.774
# This is not too far off the value of 67.84 the
# Actual value 




