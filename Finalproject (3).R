#Airbnb Final 
#Saxa 6
#Names: Taylor Washington, Brian Zhang, Jason Harris, Andrew Harper

# Load necessary libraries for data manipulation, visualization, and statistical testing
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stargazer)
install.packages("lmtest")
install.packages("car")
library(lmtest)
library(car)

#Load datasets for Airbnb Listings and Reviews
listingdata = read.csv("Listings.csv")
reviewdata = read.csv("Reviews.csv")


#1 Report your exploratory analysis of the data. This can include data visualization,
#summary tables, changes made to the data, or any other insightful findings about the data.

# Merge the listing and review data on the 'id' and 'listing_id' columns, respectively
complete_data <- left_join(listingdata, reviewdata, by = c("id" = "listing_id"))
# Calculate the total number of missing values in the entire dataset
# 512 Missing values in total.
total_missing <- sum(is.na(complete_data))

# Calculate the mean of the host acceptance rate, excluding NA values
mean_acceptance_rate <- mean(complete_data$host_acceptance_rate, na.rm = TRUE)

# Replace NA values in the host acceptance rate with the calculated mean
complete_data_draft <- complete_data %>%
  mutate(host_acceptance_rate = ifelse(is.na(host_acceptance_rate), mean_acceptance_rate, host_acceptance_rate))

# Replace NA values in room_type with appropriate values based on the number of bathrooms
complete_data_draft <- complete_data_draft %>%
  mutate(room_type = case_when(
    is.na(room_type) & grepl("private", bathrooms, ignore.case = TRUE) ~ "Private room",
    is.na(room_type) & grepl("shared", bathrooms, ignore.case = TRUE) ~ "Shared room",
    is.na(room_type) ~ "Entire home/apt",
    TRUE ~ room_type
  ))

# Calculate the mean number of bedrooms, rounding up to the nearest integer, excluding NA values
mean_bedrooms <- ceiling(mean(complete_data_draft$bedrooms, na.rm = TRUE))

# Remove non-numeric characters from the 'bath' column and convert it to numeric
# Creates a new column 'bath_n' for the number of bathrooms
complete_data_draft$bath_n <- as.numeric(gsub("[^0-9.]", "", complete_data_draft$bath))

# Replace NA or empty values in the bedrooms column based on the number of beds
complete_data_draft <- complete_data_draft %>%
  mutate(bedrooms = case_when(
    is.na(bedrooms) & beds > 1 ~ mean_bedrooms,
    is.na(bedrooms) & beds == 1 ~ 1,
    bedrooms == "" & beds > 1 ~ mean_bedrooms,
    bedrooms == "" & beds == 1 ~ 1,
    TRUE ~ bedrooms
  ))

# Print the updated dataframe to the console
print(complete_data_draft)


# Check if there are any missing values in each column of the updated dataframe
apply(complete_data_draft, 2, anyNA)

# Identify the rows with missing values in the 'avg_rating' column
missing_row_avg_rating = which(is.na(complete_data_draft$avg_rating))

# Replace missing values in the 'avg_rating' column with the mean of the column values
complete_data_draft$avg_rating[missing_row_avg_rating] = mean(complete_data_draft$avg_rating, na.rm = TRUE)

# Check again for any missing values in each column
apply(complete_data_draft, 2, anyNA)

# Print the names of the columns in the updated dataframe
names(complete_data_draft)


str(complete_data_draft)
summary(complete_data_draft)
colSums(is.na(complete_data_draft))
# Example for 'price'
ggplot(complete_data_draft, aes(x = price)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Prices", x = "Price", y = "Count")
# Example for 'neighborhood'
# Display neighborhoods in descending order by counts
ggplot(complete_data_draft, aes(x = reorder(neighborhood, -table(neighborhood)[neighborhood]))) +
  geom_bar(fill = "pink", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Neighborhoods", x = "Neighborhood", y = "Count")
# Create and examine correlation matrix
cor_matrix <- cor(complete_data_draft[, sapply(complete_data_draft, is.numeric)], use = "complete.obs")
cor_matrix

ggplot(complete_data_draft, aes(x = neighborhood, y = price)) +
  geom_boxplot(fill = "purple", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Prices by Neighborhood", x = "Neighborhood", y = "Price")

summary_stats <- complete_data_draft %>%
  group_by(neighborhood, room_type) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    sd_price = sd(price, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

#2 Which combination of neighborhood and room type has the highest average price? 
#Which one has the lowest? Which combination has the highest variability? Which combination has the lowest?

# Calculate average price and variability
summary_stats <- complete_data_draft %>%
  group_by(neighborhood, room_type) %>%
  summarise(
    avg_price = mean(price, na.rm = TRUE),
    price_sd = sd(price, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(price_sd)) # Ensure no NA values in price_sd

# Highest and lowest average price
highest_avg <- summary_stats %>% filter(avg_price == max(avg_price))
lowest_avg <- summary_stats %>% filter(avg_price == min(avg_price))

# Highest and lowest variability (use slice_max and slice_min to handle ties)
highest_variability <- summary_stats %>% slice_max(price_sd, with_ties = FALSE)
lowest_variability <- summary_stats %>% slice_min(price_sd, with_ties = FALSE)

# Output the highest and lowest averages and variabilities
list(
  highest_avg = highest_avg,
  lowest_avg = lowest_avg,
  highest_variability = highest_variability,
  lowest_variability = lowest_variability
)

#3 Write an R function that takes three arguments: a confidence level, the name of a numerical variable, 
#and a data frame. The function should return the confidence interval for the average value of the specified variable 
#at the given confidence level. Apply this function to compute a 95% confidence interval for the average price of the 
#listings in the dataset. Provide an interpretation of the computed confidence interval in the context of the Airbnb listings.

#Function to calculate confidence interval for a specified variable
confidence_interval=function(cl,dfandn){
  mean_nv=mean(dfandn,na.rm=TRUE) #finds the mean of the numerical variable given
  n=length(dfandn) #finds the n value for the confidence interval
  se_nv=sd(dfandn,na.rm=TRUE)/sqrt(n) #finds the standard error
  degrees_freedom=(n-1) #degrees of freedom
  Em=qt(cl, degrees_freedom)*(se_nv)  #Margin of Error
  lower_nv=mean_nv -Em #Calculates the lower bound of the interval
  upper_nv=mean_nv +Em #Calculates the upper bound of the interval
  c(lower_nv,upper_nv) #confidence interval
}
#Compute 95% confidence interval for 'price' and interpret results
confidence_interval(.95,complete_data_draft$price)
#Interpretation of the computed confidence interval in terms of the Airbnb listing would be that the confidence interval is the range within which we are 95% confident that the
#true average price of Airbnb lisitng falls. So the listing would fall betwen $199 and $210.

#4 Test whether the average price of all listings in the population is more than $200 (at 95% level of confidence). 
#What is the statistical conclusion based on this result? Is this conclusion in line with the estimated confidence interval reported in question 3?
#Hypothesis Testing One Sample Test


#Perform hypothesis testing: is the average price of listings > $200?
#Null hypothesis= mu0=200
#Alternative hypothesis=m>200
mu0=200 #the null hypothesis
xbar=mean(complete_data_draft$price,na.rm=TRUE) #mean of the population
sigma=sd(complete_data_draft$price,na.rm=TRUE) #standard deviation of the population
n=length(complete_data_draft$price) #total amount of listing
degreeoff=(n-1) #degree of freedom

# Calculate the t-test statistic and critical value
t_test=(xbar-mu0)/(sigma/sqrt(n)) #t-test calculation
t_test
critical_value=qt(.975,degreeoff) #critical value calculation
critical_value

#Conclusion: Since t statistic is 1.46 is lower than the critical value of t=1.645, fail reject the null hypothesis concluding there is
#no significant evidence to say the price is greater than $200
#The conclusion is not in line with confidence interval since the null hypothesis is in the reported confidence interval.

#5 Visualize price to test for normality and comment on the results (diagnostics plots generated for this question
#will be counted as only 1 plot in your report - see below for details).

# Histogram for Price
# indicates an asymmetric skewed right distribution vs a symmetric normal distribution
# This indicates that price is not normally distributed.
hist(complete_data_draft$price, col = "darkmagenta", main = "Price Distribution", xlab = "Price")


# Density plot for Price
# As seen for the histogram, density plot indicates an asymmetric skewed right distribution vs a symmetric normal distribution.
plot(density(complete_data_draft$price), main = "Density Plot Price", col = "blue", lwd = 2)

# quantile-quantile plot to check for normality of the Price attribute
# The points of on the qq plot deviate significanlty from the diagonal line. This is an indication that Price is not normally distributed.
qqnorm(complete_data_draft$price, main = "Q-Q Plot of Price")
qqline(complete_data_draft$price, col = "red")

# Shapiro-wilk test for normality
# p-value is less than 0.05, so there is sufficient evidence that the Price is not normally distributed.
shapiro.test(complete_data_draft$price)

#6 What’s the best simple linear regression model for “price” of the listings based on R-squared and residual standard error? 
#Compare/present the results of the tested models as a table in your report. (Note: You should not manually create the comparison 
#table in your submitted report. Your code should generate the table.

# Simple linear regression models for predicting 'price' using various predictors
lm_beds <- lm(price ~ beds, complete_data_draft)
lm_baths<-lm(price~bath_n, complete_data_draft)
lm_rating <- lm(price ~ avg_rating, complete_data_draft)
lm_accommodates <- lm(price ~ accommodates, complete_data_draft)
lm_min_nights <- lm(price ~ min_nights, complete_data_draft)

# Compare regression models using stargazer to generate a table
td=stargazer(lm_beds,lm_baths, lm_rating, lm_accommodates, lm_min_nights,
             type = "text", 
             title = "",
             dep.var.labels = "",
             covariate.labels = c("Bed Regression", "Bath Regression","Rating Regression", "Accommodates Regression", "Minimum Nights Regression"),
             keep.stat = c("n", "rsq", "adj.rsq", "ser"))

#7 Implement a multiple linear regression model for the “price” of the listings. Report the regression coefficients 
#and measures of fit, and write an interpretation of the regression coefficients in the context of this model. 
#Are there any violations of model assumptions?
names(complete_data)
names(complete_data_draft)

# I want treat host_since as a numeric variable that represents the duration of time a host has been active
# Converting host_since into a date
complete_data_draft$host_since <- as.Date(complete_data_draft$host_since, format="%m/%d/%Y")
# Calculating the duration in years since the host first started hosting
complete_data_draft$host_duration <- as.numeric(difftime(Sys.Date(), complete_data_draft$host_since, units = "days")) / 365
#After running this code, the new column host_duration will contain numeric values representing the number of years each host has been active on Airbnb, based on their host_since start date.
#This value can now be used as a numeric feature in regression models or other analyses.
print(complete_data_draft)
# Fitting a model
mult_reg <- lm(price ~ neighborhood + host_duration + superhost +
                 host_acceptance_rate + host_total_listings + room_type +
                 accommodates + bedrooms + beds +
                 min_nights + total_reviews + avg_rating+bath_n, complete_data_draft)

# Summary of the model to get coefficients and fit measures
summary(mult_reg)
# Generate a summary table using stargazer
stargazer(mult_reg, type = "text",
          dep.var.caption = "Dependent Variable: Price",
          dep.var.labels.include = T,
          report = "vc*",  # To include coefficients, standard errors, and significance levels
          df = F,          # To exclude degrees of freedom from the output
          single.row = T,  # To present the output in a single row for each predictor
          keep.stat = c("ser", "rsq", "adj.rsq", "f"),  # To include SER, R-squared, adjusted R-squared, and F-statistic
          title = "Multilinear Regression Model Summary",
          column.labels = c("Model Results"))  # Optional: you can add column labels for clarity

# Heteroscedasticity (constant residuals), Linearity (Red line should be flat along zero), Multicollinearity, Independence/Normality of residuals

plot(mult_reg, which = 1)  # Residuals vs. Fitted plot
plot(mult_reg, which = 5) # Residuals vs. Leverage Plot

#Small violation of both Linearity, Heteroscedaticty, and Normality of Residuals



# Fit the linear model (replace with actual formula and data) 
# Convert categorical variables to factors
complete_data_draft$superhostcat <- as.factor(complete_data_draft$superhost)
complete_data_draft$neighborhoodcat <- as.factor(complete_data_draft$neighborhood)
complete_data_draft$room_typecat <- as.factor(complete_data_draft$room_type)

#Implement a multiple linear regression model for the “price” of the listings
lm_model <- lm(price ~ neighborhoodcat + host_duration + superhostcat +
                 host_acceptance_rate + host_total_listings + room_typecat +
                 accommodates + bath_n + bedrooms + beds + min_nights +
                 total_reviews + avg_rating, complete_data_draft)
print(lm_model)

# Perform the Breusch-Pagan test for heteroscedasticity
bp_test <- bptest(lm_model)
print(bp_test)

# Plot the residuals vs fitted values to visually inspect for heteroscedasticity
plot(lm_model$fitted.values, resid(lm_model),
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")
lines(smooth.spline(lm_model$fitted.values, resid(lm_model)), col = "blue", lwd = 2)


#8 Are there any multicollinearity concerns among the independent variables selected for the multiple regression model of question 7? Explain.

# Check for multicollinearity in the model using Variance Inflation Factor (VIF)
vif_values <- vif(lm_model)
print(vif_values)

# Identify multicollinearity issues (VIF > 10 suggests high multicollinearity)
if(any(vif_values > 5)){
  cat("Some variables have high multicollinearity.\n")
} else {
  cat("Multicollinearity is within acceptable limits.\n")
}

#9 Summary/Recommendation Section: Write a summary/recommendation section based on your findings. In 1-2 paragraphs, provide an investment 
#recommendation or market analysis summary for Airbnb in Washington, DC, based on the data you analyzed. Use insights from your analysis to 
#justify your recommendation. (Note: You should start your submitted report with this summary.)

#This analysis of Washington, DC's Airbnb market identifies critical drivers of revenue optimization for hosts and offers actionable insights into 
#strategic investments. The data reveals that hosting in premium neighborhoods such as Georgetown, Dupont Circle, and Shaw leads to a price premium of $27, $21, and $13,
#respectively, in nightly rates. Furthermore, hosts earning the title of Superhost and maintaining an acceptance rate above 90% show elevated nightly rates of $22 and $34,
#respectively, indicating that host reputation plays a discernible role in driving revenue.

#Furthermore, property features play a critical role in maximizing revenue. Each additional bedroom and bathroom adds approximately $43 to the nightly rate, emphasizing the demand 
#for accommodations that can host larger groups and offer enhanced amenities. Hosts who focus on optimizing property capacity and features, alongside maintaining a high service standard,
#are well-positioned to achieve higher occupancy rates and profitability. These insights provide a strategic framework for hosts looking to thrive in Washington, DC's competitive short-term 
#rental market.

