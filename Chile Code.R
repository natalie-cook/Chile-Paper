library(readxl)
library(tidyverse)
library(stargazer)

#read in World Bank data
data<- read_excel("Data_Extract_From_World_Development_Indicators.xlsx")

#create a dummy variable for time periods "before" and "after" the treatment
#in this case, it is pre-neoliberal econ policy and post
data$TIME <- ifelse(data$YEAR<1973, 0, 1)

#create a dummy variable to identify the group exposed to the treatment
#in this case, Argentina is the control group and Chile is the treatment group
data$GROUP <- ifelse(data$COUNTRY=="Argentina", 0, 1)

#create the interaction variable for time and group
data$DID <- data$TIME*data$GROUP

#use a linear regression model to calculate the DID estimate
didreg <- lm(data$GDP~data$TIME + data$GROUP + data$DID, data = data)
summary(didreg)
#make a nice output table
stargazer(didreg)

#make a nice GDP scatterplot
ggplot(data=data, mapping=aes(x=YEAR, y=GDP, color=COUNTRY))+
       geom_point()

#make a nice GINI scatterplot
ggplot(data=data, mapping=aes(x=YEAR, y=GINI, color=COUNTRY))+
  geom_point()
#it needs some work

