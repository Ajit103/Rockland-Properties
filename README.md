# Rockland-Properties
Exploratory Data Analysis for housing data in Rockland. 

# Scenario
Rockland, Ontario is a community located 40 km east of Ottawa on the Ottawa River. On
January 1, 1988 the Town of Rockland was amalgamated with Clarence Township, forming a
city with two areas. A family of four is interested in purchasing a home in Rockland – they have
a budget of $350 000. They want to investigate the area and learn about homes on the market
before making a decision to purchase. A random sample of 169 dwellings is taken from the MLS
listings and the following variables are recorded: list price, type of dwelling (apartment, house,
mobile, or row/townhouse), number of bedrooms, number of bathrooms, title (freehold or
condo), and area (Clarence-Rockland or Rockland). The data is given in the file “Rockland.csv”.

# Recommend the type of house for families based on their budget and number of members using Hypothesis Testing, Anova Test and t-test.

# Import dataset
cs<- read_csv("Rockland.csv")

								   	                  
# Price range of a home in Rockland and affordability.							

# Checking conditions:

1. Independence assumption
2. Randomization condition
3. 10% condition
4. Nearly normal condition:
hist(cs$List.Price, main = "List Price of homes")
From the histogram we can see that the data is unimodal and it is not extremely skewed.
So we can use t-test as the sample is large i.e. greater than 40

t.test(cs$List.Price, conf.level = 0.95)
People should expect to pay 306134 to  355607 to buy home in Rockland.
With a budget of $350,000, the family can afford to buy a home as their
budget is way more than the average price of homes($330870) in Rockland.

#######################Next Test##########################

We can also check if the family can afford a home in Rockland using Hypothesis test.
The hypothesis are
Ho: mu = 350000
Ha: mu > 350000
t.test(cs$List.Price, mu = 350000, alternative = "greater")
From the t-test we can see the p-value(0.9356) is greater than alpha(0.05), we fail to reject the null hypothesis.
Hence, we can see there is no evidence that the average price of home in Rockland is greater than $350,000
So, the family can afford a home in Rockland with a budget of $350,000.

								   	                  
# Comparison of List Prices for the different types of homes							

# To compare the price of different types of house, we can do one-way anova
# Checking for the conditions

Checking for the Independence Condition:
1. Independence Assumption
2. Randomization Condition
3. Independent Group Assumption

# Checking for the equal variance condition:
boxplot(cs$List.Price~cs$Type, data = cs,
        main="Comparison of List Price of homes by type of homes",
        ylab="List Price",
        xlab="Types of Homes", 
        col=c( "red", "orange","yellow", "green" ))
There does seem to be some difference in variation in list price by type of homes.
But the spread is not very different, so we can use the anova test.

# Residuals vs. Predicted Plot 
cs.anova <- aov(cs$List.Price ~ cs$Type)
plot(cs.anova)
The red line is close to straight line but as the fitted values increase the line deviates downward.

# Checking for the normal population assumptions:
1.boxplot for comparison
From the above boxplot, we can see that there is no extreme skewness and 
they are not all skewed in same direction.

2.Histogram of residuals plot:
hist(cs.anova$residuals,
     main="Histogram of Residuals",
     xlab="Residuals")
The histogram of residuals plot is unimodal and not extremely skewed so we can say it is nearly normal.

3.Normal Probability (QQ)plot
plot(cs.anova)
The QQ plot is closely a straight line with a few outliers at the ends.

# Barplot of the mean price for each type of homes
library(ggplot2)
library(Hmisc)
ggplot(cs, aes(cs$Type, cs$List.Price, fill=cs$Type))+  
  stat_summary(fun.y=mean, geom="bar")+  
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+ 
  labs(x="type", y="listprice", title="Results of price Test")+  
  scale_fill_brewer(palette="Set3")

# ANOVA Test
Hypothesis:
Ho: mu(Apt)=mu(House)=mu(Mob)=mu(Row/TH)
HA: At least mean price for one type of home is different

cs.anova<- aov(cs$List.Price~cs$Type, data = cs)
summary(cs.anova)
Here, F-value = 9.516 and p-value = 7.83e-06 which is less than alpha = 0.05. Hence, we reject the null hypothesis.
We can conclude that the average price for at least one type of home is different than the others.

TukeyHSD(cs.anova)
Mobile-Apartment  p=0.021
Mobile-House      p=0.0004
Row/Townhouse-House p= 0.002
We can see that the list price for "Mobile house" is different than that of "Apartment" and "House".
Also, the price for "Townhouse" and "House" is also different.

# Bar plot
ggplot(cs, aes(x = cs$Type, y = cs$List.Price, fill=cs$Type))+  
  stat_summary(fun.y=mean, geom="bar")+  
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+ 
  labs(x="type", y="listprice", title="Comparison of price")+  
  scale_fill_brewer(palette="Set3")
From the bar plot, we can see that the price for Mobile homes is very less compared to others.
The houses have the highest list price.

								   	                  
# Compare the price of home by Area							

To compare the price by area, we can use anova test

# Checking for the conditions

Checking for the Independence Condition:
1. Independence Assumption
2. Randomization Condition
3. Independent Group Assumption

# Checking for the equal variance condition:
boxplot(cs$List.Price~cs$Area,data = cs,
        main="Comparison of List Price of homes by area",
        ylab="List Price",
        xlab="Area", 
        col=c( "red", "green" ))

There does seem to be some difference in variation in list price by area.
But the spread is not very different, so we can use the anova test.

# Residuals vs. Predicted Plot 
plot(cs.anova)
The red line is close to straight line. 

# Checking for the normal population	assumptions:
1.boxplot for comparison
From the above boxplot, we can see that there is no extreme skewness and 
they are not all skewed in same direction.

2.Histogram of residuals plot:
cs.anovaArea<- aov(cs$List.Price~cs$Area, data = cs)
hist(cs.anovaArea$residuals,
     main="Histogram of Residuals",
     xlab="Residuals")
The histogram of residuals plot is unimodal and not extremely skewed so we can say it is nearly normal.

3.Normal Probability (QQ)plot
plot(cs.anova)
The QQ plot is closely a straight line with a few outliers at the ends.

# Anova test
# Hypothesis:
Ho: mu(Rockland)=mu(Clarence)
HA: At least mean price for one one area is different

cs.anovaArea<- aov(cs$List.Price~cs$Area, data = cs)
summary(cs.anovaArea)
From the anova, we can see that the p-value (0.62) is greater than alpha.
We fail to reject the null hypothesis.
We can say that the list price for homes are not different for the two areas in the city.

#######################Next Test##########################

To compare the price by area, we can use two-sample t-test for the mean list price 

rockland<-subset(cs, Area=="Rockland", select=List.Price)
clarence<-subset(cs, Area=="Clarence-Rockland", select=List.Price)

hist(rockland$List.Price, main = "Price for homes in Rockland Area",
     xlab = "List Price", ylab = "Number of homes", col = "blue", breaks = 20)
hist(clarence$List.Price,main = "Price for homes in Clarence-Rockland Area",
     xlab = "List Price", ylab = "Number of homes", col = "red")

# Hypothesis:
Ho: mu(Rockland)=mu(Clarence)
HA: At least mean price for one one area is different

t.test(clarence$List.Price,rockland$List.Price, 	
       alternative="two.sided",
       paired = FALSE,
       conf.level = 0.95,
       var.equal=FALSE)
From the two-sample t-test, we can see that the p-value(0.6156) is greater than alpha.
We fail to reject the null hypothesis.
We can conclude that the average price of homes for the two given areas is not significantly different.


							   	                  
# Finding patterns in the types of homes available							

To find 	the patterns	in	the	types	of	homes	available	that	will	have	
enough	bedrooms	for	the	family	of	four, I am conducting Two way ANOVA test

# Checking for the conditions

# Checking for the Independence Condition:
1. Independence Assumption
2. Randomization Condition
3. Independent Group Assumption

# Checking for the equal variance condition:
Create boxplots for each of the factors
boxplot(List.Price~Type, cs, col=c("blue", "red","green", "orange"), 
        ylab="Price of home", main="Price of home by type")
boxplot(List.Price~as.factor(Number.of.Bedrooms), cs,col=c("blue", "red","green", "orange", "brown","purple"), 
        ylab="Price of home", main="Price of home by number of bedrooms")
boxplot(List.Price~Type*as.factor(Number.of.Bedrooms), cs, col=c("blue", "red"), 
        ylab="Price of home", main="Price of home by type and number of bedrooms")

There does seem to be some difference in variation in list price by area.
But the spread is not very different, so we can use the anova test.

# Residuals vs. Predicted Plot 

patterns.anova<- aov(cs$List.Price~ cs$Type*as.factor(cs$Number.of.Bedrooms))
plot(patterns.anova)
The red line is close to straight line. 

# Checking for the normal population	assumptions:
1.boxplot for comparison
From the above boxplot, we can see that there is no extreme skewness and 
they are not all skewed in same direction.

2.Histogram of residuals plot:
hist(patterns.anova$residuals,
     main="Histogram of Residuals",
     xlab="Residuals")
The histogram of residuals plot is unimodal and not extremely skewed so we can say it is nearly normal.

3.Normal Probability (QQ)plot
plot(patterns.anova)
The QQ plot is closely a straight line with a few outliers at the ends.

Ho: the mean price of homes for different types(factor A) are equal.
Ho: the mean price of homes for different number of bedrooms(factor B) are equal.
Ho: the effects of types of home (factor A) are constant across the levels of number of bedrooms (factor B)
Ha: At least one mean is different than the others

patterns.anova<- aov(cs$List.Price~ cs$Type*as.factor(cs$Number.of.Bedrooms))
summary(patterns.anova)
TukeyHSD(patterns.anova)
#5-3  182425.451   26610.96 338239.9 0.0116607
#House:5-Row/Townhouse:3          2.935918e+05    69685.711  517497.93 0.0006980
#House:4-Row/Townhouse:3          1.704563e+05    20255.504  320657.07 0.0092428
#House:5-Mobile:3                 4.782250e+05    49477.447  906972.55 0.0119938
#House:5-Mobile:2                 4.789750e+05    50227.447  907722.55 0.0117187
#House:5-House:2                  2.369370e+05     4415.993  469458.01 0.0401193
#House:5-Apartment:2              2.477461e+05     7385.087  488107.06 0.0349104


ggplot(cs, aes(as.factor(cs$Number.of.Bedrooms), cs$List.Price,fill = as.factor(cs$Number.of.Bedrooms)))+
  stat_summary(fun.y=mean, geom="bar", fill="blue")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+
  labs(x="Number of bedroom", y="List price",
       title="List price by number of bedrooms")

ggplot(cs, aes(cs$Type, cs$List.Price, fill = cs$Type))+
  stat_summary(fun.y=mean, geom="bar", fill="blue")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2)+
  labs(x="Type of home", y="List price",
       title="List price by type of home")



ggplot(cs, aes(as.factor(cs$Number.of.Bedrooms), cs$List.Price, fill=cs$Type))+
  stat_summary(fun.y=mean, geom="bar", position="dodge")+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", position="dodge")+
  labs(x="Number of bedroom", y="List Price")+
  ggtitle("List price by type of house and number of bedrooms")


