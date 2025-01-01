# Airbnb Listings Analysis Portfolio

## About
This project demonstrates a comprehensive analysis of Airbnb listings in Washington, DC, focusing on identifying factors that influence price variability and providing actionable recommendations for optimizing revenue. The project showcases data manipulation, statistical analysis, and visualization techniques using R.

---

## Tools & Technologies
- **Programming Language**: R
- **Libraries**:
  - Data manipulation: `dplyr`, `tidyverse`
  - Data visualization: `ggplot2`
  - Statistical modeling: `stargazer`, `lmtest`, `car`
- **Software**: RStudio for script development and analysis.

---

## Skills Demonstrated
### **1. Data Wrangling and Cleaning**
- Merged datasets (`listings` and `reviews`) using `left_join`.
- Addressed missing data with calculated means and logical replacements.
- Converted non-numeric and categorical variables into usable formats for analysis.

### **2. Exploratory Data Analysis (EDA)**
- Generated summary statistics for critical variables like price, room type, and neighborhoods.
- Visualized price distributions, neighborhood trends, and correlations.
- Computed a correlation matrix to explore relationships between numerical variables.

### **3. Data Visualization**
- Created histograms and density plots to illustrate price distributions.
- Visualized neighborhood price variability with boxplots.
- Assessed normality using quantile-quantile plots and Shapiro-Wilk tests.

### **4. Statistical Analysis**
- Estimated confidence intervals for prices using custom R functions.
- Conducted hypothesis testing to evaluate whether average nightly prices exceeded $200.
- Analyzed and validated assumptions of normality and homoscedasticity.

### **5. Predictive Modeling**
- Built simple linear regression models to predict price based on individual features like beds, baths, and average ratings.
- Developed a multiple linear regression model incorporating property features, host attributes, and room type.
- Compared models using R-squared, adjusted R-squared, and residual standard error.

### **6. Diagnostics & Model Validation**
- Performed Breusch-Pagan tests for heteroscedasticity.
- Visualized residuals to test assumptions of linear regression models.
- Assessed multicollinearity using Variance Inflation Factors (VIF).

### **7. Business Insights & Recommendations**
- Identified premium neighborhoods like Georgetown and Dupont Circle as high-revenue areas.
- Highlighted the importance of Superhost status and high acceptance rates for increasing revenue.
- Quantified the impact of additional bedrooms and bathrooms on price.
- Recommended investment strategies for optimizing property revenue in Washington, DC.

---

## Outcomes
- Delivered a comprehensive analysis integrating statistical modeling and business insights.
- Provided a framework for Airbnb hosts to optimize revenue through data-driven decisions.
- Demonstrated advanced proficiency in R programming and data analysis.


