# Here I use this markdown document to present 
# the insights I found by analysing the sales data from Turtle Games. 

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

# First I do some data cleaning to prepare the data for analysis. 

# Set the plots margin size
par(mfrow=c(1,1))

# Import the data set.

# Import a CSV file.

data <- read.csv(file.choose(), header=T)

data <- read.csv("/Users/ameliaoberholzer/Documents/R Assignment/turtle_sales.csv", header=TRUE, stringsAsFactors=FALSE)


# Identifying null values 
sum(is.na(data$size))

# Quickly identify max and min of global sales
summary(data)

# Use the glimpse() function.
glimpse(data)

# Changing the date
data$Year <- lubridate::ymd(data$Year, truncated = 2L)

# Changing product

data$Product <- as.character(data$Product)

# Checking data types
str(data)

# Now I'm going to get an overview of the sales data 
# Drop Ranking, Year, Genre and Publisher columns 
data_sales <- select(data, -Ranking, -Year, -Genre, -Publisher)


# I will now start to create simple visulisations 
# to understand how individual products from Turtle Games affect sales. 


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

# These graphs show the distribution of sales across different regions

# Distribution of sales in EU, NA and the World

# Most basic scatter chart
NA_Sales_distribution <- ggplot(data_sales, aes(x=Product, y=NA_Sales)) +
  geom_point(color="red",size=2) +
  theme(plot.title = element_text(size = 7)) +
  ggtitle("Scatterplot - North America Sales") 

EU_Sales_distribution <- ggplot(data_sales, aes(x=Product, y=EU_Sales)) +
  geom_point(color="Blue",size=2) +
  theme(plot.title = element_text(size = 7)) +
  ggtitle("Scatterplot - EU Sales") 

Global_Sales_distribution <- ggplot(data_sales, aes(x=Product, y=Global_Sales)) +
  geom_point(color="green",size=2) +
  theme(plot.title = element_text(size = 7)) +
  ggtitle("Scatterplot - Global Sales") 

NA_Sales_distribution + EU_Sales_distribution + Global_Sales_distribution


# Here most of the products have sales in between 0 and 5 million 
# however some products have sold exceptionally well. 
# I will go ahead to identify top selling products as well 
# as products that havent sold as well. 



# Use group by function() to get the sum
# Of sales for certain platform 

sum_sales_product_global <- data %>%
  group_by(Product, Platform, Global_Sales) %>% 
  summarise(Total = sum(Global_Sales))

# Viewing the count of sales for a certain game 
# Creating a scatter plot for this information

global_sales_plot <- ggplot(sum_sales_product_global, 
                            aes(x = Product, y = Total, colour = Platform)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Total Global Sales of Product ID")
global_sales_plot

# Here we can see which products have higher sales 
# recorded depending on their sales platform.
# Interestingly Wii has the highest sale of a certain product. 
# Almost by double all other products. 
# In general this product has been a top seller at Turtle Games. 


# Top 15 and their platform 
ordered_scatter_product = sum_sales_product_global[order(sum_sales_product_global$Total, decreasing = TRUE),]  
ordered_scatter_product
ordered_scatter_product_top <- head(ordered_scatter_product, 15)
global_sales_plot_top <- ggplot(ordered_scatter_product_top, 
                                mapping = aes(x = Product, y = Total, colour = Platform)) +
  geom_point() +
  theme(plot.title = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Global Sales of Top 15 Products")

# Bottom 15 and their platform 
ordered_scatter_product = sum_sales_product_global[order(sum_sales_product_global$Total, decreasing = TRUE),]  
ordered_scatter_product
ordered_scatter_product_bottom <- tail(ordered_scatter_product, 15)
global_sales_plot_bottom <- ggplot(ordered_scatter_product_bottom, 
                                   mapping = aes(x = Product, y = Total, colour = Platform)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(plot.title = element_text(size = 7)) +
  ggtitle("Global Sales of Bottom 15 Products")
global_sales_plot_bottom + global_sales_plot_top

# We see here how the bottom and top plots compare. 
# Interestingly PC have the lowest sales performance.
# Whereas Will, NES and GB have higher sales performance. 
# You can also see the individual product numbers. 
# Turtle Games can use their directory to identify which product
# corresponds to the number to gain more information.
# It is interesting to see how some platforms are more popular 
# in different regions. I will now go on to investigate this! 

  
# Use group by function() to get the sum
# Of sales for certain platform 
sum_sales_platform_global <- data %>%
  group_by(Platform, Global_Sales) %>% 
  summarise(Total = sum(Global_Sales))

# Barplot showing the total sales for certain platforms
# On some version this bar plot comes out with horizontal lines
# I'm not sure why this is the case
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


# I will now filter out the highest platform sales in EU and NA 
# for a clear comparison

# First EU 
EU_highest_platform_sales <- sum_sales_platform_EU %>%
  arrange(desc(Total)) %>% 
  group_by(Platform)
EU_highest_platform_sales_top <- head(EU_highest_platform_sales ,5)
EU_highest_platform_sales_top
EU_highest_platform_sales_top_plot <- ggplot(EU_highest_platform_sales_top,
                                             aes(x = Platform, y = Total)) +
  geom_bar(stat="identity", colour="Blue") +
  theme(plot.title = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Total Top EU Sales for a Certain Platform")
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
  theme(plot.title = element_text(size = 7)) +
  ggtitle("Total Top NA Sales for a Certain Platform")
EU_highest_platform_sales_top_plot + NA_highest_platform_sales_top_plot

# Interestingly we see how the Wii is more popular in the 
# EU whereas the Xbox 360 is more popular in the US. 
# This information can information Turtle Games on
# where to focus marketing when new products have been 
# released from these platforms. 
# Now were going to observe how sales 
# change over time using group_by to find the total global sales per year. 

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


# Interesting to see that we see seasonal spikes here.
# Turtle Games product sales are more successful 
# during certain times of the year. 


# Creating a time series to see seasonal spikes in more detail...


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


# We see here that sales spike around the end and the beginning of the year. Turtle games should push sales and marketing campaigns during these times. Perhaps it could be the case that new games are released during this time of year?
  
# https://www.ps4playstation4.com/ps4-release-date-countdown-begins

# This article contains release dates of PS1, PS2, PS3 and PS4 platforms. 
# Interestingly November is a popular release date. 
# This is in time for Christmas. 
# This indicates that gaming companies 
# like to have big releases before Christmas, the gift giving season, 
# to improve sales. Turtle games should focus on improving sales 
# around this time. 

# Having a look at seasonal trends in more detail to 
# see how the sales of certain platforms changes over time.

# Show top platforms to see sales changing over time 

top_sales_over_time <- sum_sales_platform_global_year [sum_sales_platform_global_year$Platform %in% c("DS","PS2", "PS3", "PS4", "Wii", "X360"),]

top_sales_over_time

top_sales_over_time_plot <- ggplot(top_sales_over_time,
                                   aes(x = Year, y = Total, colour = Platform)) +
  geom_line() +
  ggtitle("Total Top Global Sum of Sales Over Time for Top Platforms")

top_sales_over_time_plot

# It is interesting to see how the sale of certain platforms 
# over time. We see when games were in higher demand they 
# have the highest sales. 

# As seen by this article Wii has now been discontinued due 
# to other platforms being released. This shows how Turtle Games 
# needs to understand and create a marketing strategy around new 
# game releases to maximize profit. 

# https://www.lifewire.com/slow-painful-death-of-the-nintendo-wii-2498653#:~:text=Some%20consoles%2C%20like%20the%20PlayStation,golden%20child%20and%20walked%20away.

# I am now going to check the reliability of the sales data. 

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


# As we see data points deviate by large amount from the line. 
# Points should be lower according distribution. 
# Values in tails of the distribution are not as 
# extreme as we would expect. Therefore the qqplot shows 
# that the sales data has lighter tales. 


# Run a Shapiro-Wilk test:
shapiro.test(data$Global_Sales)


# Null hypothesis of the Shapiro-Wilk test is 
# that the data is normally distributed. 
# If there is a small P value we reject the null hypothesis. 
# There is a very small p value here. Here we see the data is not 
# normally distributed 

# Specify the skewness and kurtosis functions.
skewness(data$Global_Sales) 

# Skewness of 4 large amount of positive skewness. 
# If the skewness is between -0.5 and 0.5, the data are 
# fairly symmetrical. If the skewness is between -1 and – 0.5 
# or between 0.5 and 1, the data are moderately skewed. 
# If the skewness is less than -1 or greater than 1,
# the data are highly skewed.

# Positive Skewness means when the tail on the right side 
# of the distribution is longer or fatter. Positive skewness 
# could be caused by inequality of distribution This means the
# more sales are distribution towards the lower end of the distribution.

kurtosis(data$Global_Sales)


# This measures whether tails are heavy or light tailed. 
# The data has a kurtosis of 32. This means the data has heavier
# tails than normal. Some values much higher than predicted by 
# the distribution or their are some outliers. 

# I am now going to see to explore the relationship
# between the sales data using single and multiple linear regression. 

# Is there correlation between sales columns 
sales_data = data%>%
  select(EU_Sales, NA_Sales, Global_Sales)

cor(sales_data)

# We see here that all the sales data is strongly correlated. 
# The strongest correlation exits between global sales 
# data and EU or North America data. 

# Here we will try to plot the relationship between the data 

# Hard to see any relationship here
plot(sales_data$EU_Sales, sales_data$NA_Sales)

# A bit more of a relationships here 
plot(sales_data$EU_Sales, sales_data$Global_Sales)

# More of a relationship here 
plot(sales_data$NA_Sales, sales_data$Global_Sales)


# Makes sense that there is more of a relationship between 
# Global Sales and EU or NA sales. Global Sales is calculated
# from EU and NA sales 


# Creating simple linear regression NA and EU Sales

NA_EU_Sales <- lm(EU_Sales~NA_Sales,
                  data=sales_data)

# lm is linear model
# Just one x variable 

# View the model.
NA_EU_Sales

summary(NA_EU_Sales)


# To some extent these numbers are correlated.
# NA Sales accounts for 50% of the variance of EU Sales.
# The p is very small meaning we can reject the null hypothesis 
# that there is no correlation



# Moving on to the relationship between
# Global and NA Sales 

EU_Global_Sales <- lm(EU_Sales~Global_Sales,
                      data=sales_data)

# lm is linear model
# Just one x variable 

# View the model.
EU_Global_Sales

summary(EU_Global_Sales)


# P value is small rejecting null hypothesis of no correlation. 
# R value is large, this shows that 77% of the variance of global 
# sales is explained by EU Sales. 

# Moving on to the relationship between
# Global and EU Sales 

NA_Global_Sales <- lm(NA_Sales~Global_Sales,
                      data=sales_data)

# lm is linear model
# Just one x variable 

# View the model.
NA_Global_Sales

summary(NA_Global_Sales)



# Again we see here that the P value is very small. 
# We can reject null hypothesis of no correlation. 
# 87% of the variance of global sales can be explained by NA sales


# Multiple linear regression 

# Create a new object and 
# specify the lm function and the variables.
multi_regression = lm(Global_Sales~EU_Sales+NA_Sales, data=sales_data)

# Print the summary statistics.
summary(multi_regression)

# Combined NA and EU Sales account for 97% of
# the variation in global sales. 
# Confirmed by the low result of the P value we 
# can reject null hypothesis of no correlation. 

# Turtle Games should focus on increasing and sustaining sales 
# in the EU and NA as these regions account for a large proportion of sales. Turtle Games could also investigate and find what causes the remaining 3% of variance. This 3% could account for new emerging markets. 

# Now Im going to test the accuracy of the model. 

# Predicting future global sales 
# Testing the model
head(sales_data)
# Create a new object and specify the predict function.
predictTest = predict(multi_regression, newdata=sales_data,
                      interval='confidence')
# Print the object.
head(predictTest)

# Here we can make comparison of the accuracy of the model. 
# If NA_Sales_sum is 34.02 and EU_Sales_sum is 23.80 
# the model predicts global sales will be 72.77. 
# This is not too far off the value of 67.84 the actual value 
# You can also see the lower and upper values 
# defining the confidence interval in our predicted values. 
# The confidence interval in the predict function 
# will help us to gauge the uncertainty in the predictions. 
