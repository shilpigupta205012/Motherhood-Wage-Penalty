# libraries
library(knitr)
library(dplyr)
library(ggplot2)

# Importing the data
motherhood <- read.csv("CSVData.csv")

# Taking care of missing data
motherhood %>%  summarise_all((funs(sum(is.na(.))))) 

# here it can be seen that experience have largest number of missing values.
# It is not suitable to remove such a large number of observations. So lets try to fill them
motherhood$experience = ifelse(is.na(motherhood$experience),
                               ave(motherhood$experience,
                                   FUN = function(x) mean(x, na.rm = TRUE)),
                               motherhood$experience)

#after filling the column we have less missing values; lets just ignore them;
motherhood = motherhood[complete.cases(motherhood),]
sum(is.na(motherhood))



## Working and understanding our dataset
kable(prop.table(table( motherhood$numChildren)),
      caption = "Proportion of women with different number of children",
      col.names = c("No. of children","Proportion of women"))
# """We can see that in this dataset around 57% of women have no children,
# 22% women have one child, 14% have 2 children and only 6% women have more than 2 children.


##Now lets creating a new variable which will define if the women is a mother or not,
#irrespective of the no. of children.
motherhood$isMother <- ifelse(motherhood$numChildren>0,1,0)
kable(prop.table(table( motherhood$isMother)),
      caption = "Proportion of women having atleast one child",
      col.names = c("Child(Yes-1/No-0)","Proportion of women"))
# """As it can be clearly seen in the table that 57% women have no children,
# while 43% women have atleast one child"""


# Now lets look at the proportion of women with or without children working full time.
kable(prop.table(table( motherhood$isMother,motherhood$fullTime)),
      caption = "Proportion of women with/wiithout children working full time",
      col.names = c("Women not working full time","Women working full time"))
# Here, we can see that 37% of women without children work full time however only 
# 28% of women work full time when they are mothers.





# Scatterplots
a <- ggplot(motherhood,aes(experience,wage))+geom_point()+ 
  labs( x = "Experience", y = "Wages",title ="Experience and wage plot")
a
#In the plot above it can be seen that as the level of experience increases the wages increases.

boxplot(wage ~ isMother, data = motherhood, xlab = "Is woman a mother?",
        ylab = "Wages", main = "Boxplot showing wages of women with or without children",
        col = c("green","red"),   names = c("No","Yes"))
#Here, it can be easily interpreted from this boxplot that mean wages are almost same 
#even if the woman is a mother,
#However the maximum wages of women without children are much higher than those with a child




## Calculating the difference in mean wages between women with and without children.
t.test( motherhood$wage~motherhood$isMother , var.equal = TRUE)
#So, it's clear that we cannot reject the null hypothesis and thus state that 
#there is no significance difference between the means, (t = 1.9578, df = 2431, P > 0.05).


# after checking for the effect of motherhood on wages and
# getting that it is clearly not the case whether or not a woman has children copletely
# determines her hourly wage level. lets investigate the other relationships;
# i.e. to see if the motherhood wage penalty has changed overtime?

#the data we are using includes information from two surveys
# one is 2004 an another is in 2009.
# lets see 5he degree to which motherhood wage penalty has changed overtime.
model1 <- lm(wage ~ isMother+y2009+experience, data = motherhood)
summary(model1)

model2 <- lm(wage ~ isMother+y2009+(isMother*y2009)+experience, data = motherhood)
summary(model2)


#Models have negative effect of motherhood on wages, i.e. if the woman becomes a mother than her wages decreases, However model 2 have more negative effect than model 1 but the variable is not significant for model2 but is significant for model1.

#Coefficients Shows the regression beta coefficients and their statistical significance. Predictor variables, that are significantly associated to the outcome variable, are marked by stars.
summary(model)$coeff

motherhood %>% summarise(
  ME = mean(model1$residuals),
  MSE = mean((model1$residuals)^2)
)

motherhood %>% summarise(
  ME = mean(model2$residuals),
  MSE = mean((model2$residuals)^2)
)

# 
# Models have negative effect of motherhood on wages, i.e. if the woman becomes a mother than her wages decreases, However model 2 have more negative effect than model 1 but the variable is not significant for model2 but is significant for model1.
# 
# b.)Based on model 2
# the model is:
#   wage = 6.43855 + (-0.21525)*isMother + (3.94868)*y2009 + 0.35646*experience + (-2.27870)*(isMother:y2009)
# 
# 1. A woman without children, interviewed in 2004, with 10 months of relevant job experience. 
# ```{r}
# wage1 = 6.43855 + (-0.21525)*0 + (3.94868)*0 + 0.35646*10 + (-2.27870)*(0)
# wage1
# ```
# wage of woman without children, interviewed in 2004, with 10 months of relevant job experience is 10.00315
# 
# 2. A woman with children, interviewed in 2004, with 10 months of relevant job experience
# 
# ```{r}
# wage2 = 6.43855 + (-0.21525)*1 + (3.94868)*0 + 0.35646*10 + (-2.27870)*(1*0)
# wage2
# ```
# wage of a woman with children, interviewed in 2004, with 10 months of relevant job experience is 9.7879
# 
# 3. A woman without children, interviewed in 2009, with 10 months of relevant job experience
# 
# ```{r}
# wage3 = 6.43855 + (-0.21525)*0 + (3.94868)*1 + 0.35646*10 + (-2.27870)*(1*0)
# wage3
# ```
# wage of a woman without children, interviewed in 2009, with 10 months of relevant job experience is 13.95183.
# 
# 4. A woman with children, interviewed in 2009, with 10 months of relevant job experien
# 
# ```{r}
# wage4 = 6.43855 + (-0.21525)*1 + (3.94868)*1 + 0.35646*10 + (-2.27870)*(1*1)
# wage4
# ```
# wages of a woman with children, interviewed in 2009, with 10 months of relevant job experience is 11.45788.
# 
# c.)
# yes, the size of motherhood penalty have changed overtime, wages have increased overtime irrespective of motherhood.
# 
# 
