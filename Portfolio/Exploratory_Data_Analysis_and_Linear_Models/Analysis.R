# List of required libraries
required_packages <- c("tidyverse", "GGally", "ggfortify", "cluster","ggpubr","ggplot2","ggcorrplot","car")
# Load all libraries
lapply(required_packages, library, character.only = TRUE)
data <- read_csv('dataset/index_of_economic_freedom_2024.csv')

glimpse(data)

colnames(data)
#
#1] "Country"                "Region"                 "Year"                  
# [4] "Overall_Score"          "Property_Rights"        "Government_Integrity"  
#[7] "Judicial_Effectiveness" "Tax_Burden"             "Government_Spending"   
#[10] "Fiscal_Health"          "Business_Freedom"       "Labor_Freedom"         
#[13] "Monetary_Freedom"       "Trade_Freedom"          "Investment_Freedom"    
#[16] "Financial_Freedom"     

summary(data)
# total na value for columnwise
colSums(is.na(data))

# check the data type of columns
str(data)
# dara cleaning
#Country                 Region                   Year 
#                     0                      0                      0 
#         Overall_Score        Property_Rights   Government_Integrity 
#                     8                      2                      2 
#Judicial_Effectiveness             Tax_Burden    Government_Spending 
#                     2                      7                      8 
#         Fiscal_Health       Business_Freedom          Labor_Freedom 
#                     8                      7                      8 
#      Monetary_Freedom          Trade_Freedom     Investment_Freedom 
#                     8                      6                      7 
#     Financial_Freedom 
#                     7 


# There are 8 missing values in Overall_Score, 2 in Property_Rights, 2
# in Government_Integrity, 2 in Judicial_Effectiveness, 7 in Tax_Bur
# in Government_Spending, 8 in Fiscal_Health, 7 in Business_Freedom, 8 in Labor_Freedom, 
# 8 in Monetary_Freedom, 6 in Trade_Freedom, 7 in Investment_Freedom, 7 in Financial_Freedom.

# The data types of the columns are:
# Country: chr
# Region: chr
# Year: int
# Overall_Score: num
# Property_Rights: num
# Government_Integrity: num
# Judicial_Effectiveness: num
# Tax_Burden: num
# Government_Spending: num
# Fiscal_Health: num
# Business_Freedom: num
# Labor_Freedom: num
# Monetary_Freedom: num
# Trade_Freedom: num
# Investment_Freedom: num
# Financial_Freedom: num


# Replace NA values with column mean for numeric columns
data[] <- sapply(data, function(x) if(is.numeric(x)) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)  # Replace NA with mean
  return(x)
} else {
  return(x)  # Non-numeric columns remain unchanged
})
# now, check if any NA values are left
sum(is.na(data))
 # [1] 0
 # No NA values are left in the data.

data = na.omit(data)

# sorting overall socre from highest to lowest
data<-data %>%
 arrange(desc(Overall_Score)) 

 # visualisation 

 # Boxplot by Region'
visualise_box_plot <- function(by) {
  # Check if the specified column exists in the dataframe
  if (!by %in% names(data)) {
    stop(paste("Column", by, "not found in the dataframe."))
  }
  
  # Ensure the column is numeric for plotting
  col_data <- as.numeric(data[[by]])
  
  ggplot(data, aes(x = Region, y = col_data, fill = Region)) +
    geom_boxplot() +
    labs(
      title = paste("Distribution of", by, "by Region"),
      x = "Region",
      y = by
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# overall score
visualise_box_plot("Overall_Score")

# Property_Rights
visualise_box_plot("Property_Rights")



"
 key Observation
Europe Stands Out:
Europe appears to have the highest median overall score among all regions.
The spread (interquartile range) is relatively moderate, suggesting most countries in this region perform consistently well.

Asia-Pacific:
This region also shows a relatively high median, close to Europe but with more variability in scores.
There are a few low outliers, indicating countries with significantly lower scores.

Americas:
The Americas have a moderately high median score, but the interquartile range is larger than Europe, indicating more variability.
There are outliers at the very low end of the score spectrum.

Middle East/North Africa:
This region has a lower median score than Europe, Asia-Pacific, and the Americas.
The variability in scores is notable, but there are fewer extreme outliers.

Sub-Saharan Africa:
Sub-Saharan Africa has the lowest median overall score among all regions.
It also exhibits high variability, with a number of countries scoring very low, as shown by the outliers.

Outliers:
Extreme low scores are present in several regions, particularly in Sub-Saharan Africa, the Americas, and Asia-Pacific.
These outliers may represent countries with significant socio-economic or political challenges."

region<- data %>%
  distinct(Region)
print(region)

" Region                  
  <chr>                   
1 Asia-Pacific            
2 Europe                  
3 Middle East/North Africa
4 Sub-Saharan Africa      
5 Americas "




visualise_by_country <- function(df, country) {
  ggplot(df, aes(x = as.numeric(Overall_Score), y = reorder(Country, as.numeric(Overall_Score)))) + 
    labs(
      title = paste("Overall Score by Country in", country),
      y = "Country",
      x = "Overall Score"
    ) +
    # Add bar chart
    geom_col(fill = "skyblue", alpha = 0.8) +
    # Add jitter plot to show individual scores
    geom_jitter(color = "darkblue", size = 2, width = 0.2, height = 0) +
    # Add line chart
    geom_line(aes(group = 1), color = "black", size = 1) +
    theme_minimal()
}

# Filter data for Sub-Saharan Africa
asia_pacific <- data[data$Region == "Asia-Pacific", ]
europ <- data[data$Region=="Europe",]
americas <- data[data$Region=="Americas",]

visualise_by_country(americas,"Americas")
visualise_by_country(europ,"Europe")
visualise_by_country(asia_pacific, "Asia-Pacific")

#1  scatter matrix 
# get rid of "Cuba", "North Korea" 
data = filter(data,!(Country %in% c("Cuba", "North Korea")))
# getting rid  of categorical variable 
# Select only the 12 pillar variables
pillar_columns <- c("Overall_Score","Property_Rights", "Government_Integrity", "Judicial_Effectiveness", 
                    "Tax_Burden", "Government_Spending", "Fiscal_Health", 
                    "Business_Freedom", "Labor_Freedom", "Monetary_Freedom", 
                    "Trade_Freedom", "Investment_Freedom", "Financial_Freedom")


# Prepare the data
pillar_data <- data[,pillar_columns]

# scatter matrix
ggpairs(pillar_data) +
  ggtitle("scatter matrix") 

# hypothesis testing between overal score and property rights
cor.test(pillar_data$Overall_Score,pillar_data$Property_Rights)
#  single-predictor linear model would “best” predict overall
single_model <- lm(Overall_Score~Property_Rights,data = pillar_data)
# summary of linear single predictor model 
summary(single_model)

#diagnostic plot
autoplot(single_model,data = pillar_data,colour='Property_Rights')

# ploting fitted line 
ggplot(pillar_data, aes(y=Overall_Score, x=Property_Rights)) +
  geom_point()+ggtitle("Fitted line betweeen overall score and property right") +
  geom_smooth(method=lm, se=TRUE)



# Quetion B
# Model #1: The linear model using only government spending and labor freedom as
 #predictors.
model1 = lm(Overall_Score~Government_Spending+Labor_Freedom,data = pillar_data)
# cor between Judicial_Effectiveness and Business_Freedom
cor.test(pillar_data$Judicial_Effectiveness,pillar_data$Business_Freedom)

summary(model1)

autoplot(model1,data = pillar_data)

ggplot(pillar_data, aes(y=Overall_Score, x=Government_Spending+Labor_Freedom)) +
  geom_point()+ggtitle("Fitted line betweeen overall score and Government_Spending+Labor_Freedom") +
  geom_smooth(method=lm, se=TRUE) +
  ylab("Overall Score") +
  xlab("Government Spending + Labor Freedom")


# Model #2: The best two-predictor model using any of the 12 pillar variables.

model2 = lm(Overall_Score~Judicial_Effectiveness+Business_Freedom,data = pillar_data)
summary(model2)
#Variance Inflation Factor Interpretation
vif(model2) 

autoplot(model2)


# Model #3: The best four-predictor model using any of the 12 pillar variables.
model3 = lm(Overall_Score~Trade_Freedom+Investment_Freedom+Property_Rights+Government_Spending,data = pillar_data)
summary(model3)
autoplot(model3,data = pillar_data)

ggplot(pillar_data, aes(y=Overall_Score, x=Trade_Freedom+Investment_Freedom+Property_Rights+Government_Spending)) +
  geom_point()+ggtitle("Fitted line betweeen overall score and Trade_Freedom+Investment_Freedom+Property_Rights+Government_Spending") +
  geom_smooth(method=lm, se=TRUE) +
  ylab("Overall Score") +
  xlab("Trade_Freedom+Investment_Freedom+Property_Rights+Government_Spending")

# Model #4: The best linear model using any subset of variables from Pillar #1 and Pillar #2 only

pillar1_pillar2 = select(pillar_data, Property_Rights:Fiscal_Health)

glimpse(pillar1_pillar2)

ggpairs(pillar1_pillar2) +
  ggtitle("scatter matrix") 

# Start with a full model using all variables from Pillars #1 and #2
full_model <- lm(Overall_Score ~ Property_Rights + Government_Integrity + Judicial_Effectiveness + 
                   Tax_Burden + Government_Spending + Fiscal_Health, data = pillar_data)

# Perform stepwise regression (both directions)
model4 <- step(full_model, direction = "both", trace = 0)

summary(model4)



ggplot(pillar_data, aes(y=Overall_Score, x=Property_Rights + Government_Integrity + Judicial_Effectiveness + 
                          Tax_Burden + Government_Spending + Fiscal_Health)) +
geom_point()+ggtitle("Fitted line betweeen overall score and
                       Property_Rights + Government_Integrity + Judicial_Effectiveness + 
                   Tax_Burden + Government_Spending + Fiscal_Health") +
geom_smooth(method=lm, se=TRUE) +
ylab("Overall Score") +
xlab("Property_Rights + Government_Integrity + Judicial_Effectiveness + 
                   Tax_Burden + Government_Spending + Fiscal_Health")


#model 5


# Create a dataset with the region and selected variables from each pillar
model_data <- data[, c("Region", "Overall_Score","Property_Rights", "Government_Integrity", "Judicial_Effectiveness", 
                       "Tax_Burden", "Government_Spending", "Fiscal_Health", 
                       "Business_Freedom", "Labor_Freedom", "Monetary_Freedom", 
                       "Trade_Freedom", "Investment_Freedom", "Financial_Freedom")]

# Build a full model with all variables included (with at most two variables from each pillar)
full_model <- lm(Overall_Score ~ Region +Government_Integrity + Judicial_Effectiveness + Business_Freedom + 
                   Government_Spending + Labor_Freedom + Tax_Burden + 
                   Financial_Freedom + Monetary_Freedom + Trade_Freedom + Investment_Freedom, data = model_data)

# Perform stepwise regression to find the best model (both directions, considering AIC)
model5 <- step(full_model, direction = "both", trace = 0)

# Summary of the best model
summary(model5)

vif(model5)
autoplot(best_model,data = model_data)


ggplot(pillar_data, aes(y=Overall_Score, x= Government_Integrity + Judicial_Effectiveness + Business_Freedom + 
                          Government_Spending + Labor_Freedom + Tax_Burden + 
                          Financial_Freedom + Monetary_Freedom + Trade_Freedom + Investment_Freedom)) +
  geom_point()+ggtitle("Fitted line betweeen overall score and
                       Property_Rights + Government_Integrity + Judicial_Effectiveness + 
                   Tax_Burden + Government_Spending + Fiscal_Health") +
      geom_smooth(method=lm, se=TRUE) +
  ylab("Overall Score") +
  xlab("Property_Rights + Government_Integrity + Judicial_Effectiveness + 
                   Tax_Burden + Government_Spending + Fiscal_Health")



#AIC
AIC(model1, model2, model3,model4,model5)
# c Comparison 
