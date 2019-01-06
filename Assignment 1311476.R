setwd("C:/Users/shillar.mpofu/Desktop/DS Assignments/Technical/MODELLING IN R - III")
getwd()
crime = read.csv('COBRA-YTD2017.csv')
crime
View(crime)
summary(crime)
str(crime)


summary(is.na(crime))
table(crime$MaxOfnum_victims)
table(crime$loc_type)

crime$MaxOfnum_victims = as.numeric(as.character(crime$MaxOfnum_victims))
crime$loc_type = as.numeric(as.character(crime$loc_type ))

crime[!complete.cases(crime$MaxOfnum_victims),'MaxOfnum_victims'] = mean(crime$MaxOfnum_victims,na.rm = TRUE)
crime[!complete.cases(crime$loc_type),'loc_type'] = mean(crime$loc_type,na.rm = TRUE)

#a. Find out top 5 attributes having highest correlation (select only Numeric features).
library(dplyr)
atlanta_crime <- crime %>% select (c('MI_PRINX','offense_id','beat','MinOfucr','x','y','loc_type','MaxOfnum_victims'))
atlanta_crime
summary(atlanta_crime)
str(atlanta_crime)

library(gclus)
high_cor <- atlanta_crime# get data
high_cor
high_cor.r <- abs(cor(high_cor)) # get correlations
high_cor.col <- dmat.color(high_cor.r) # get colors
# reorder variables so those with highest are closest to the diagonal
high_cor.o <- order.single(high_cor.r)
cpairs(high_cor, high_cor.o, panel.colors=high_cor.col, gap=.5,
       main="Variables Ordered and Colored by Correlation" )

#top 5 attributes = loc_type , beat, offense_id , Minofucr and y 




#b. Find out top 3 reasons for having more crime in a city.

crime_reasons = table(crime$UC2.Literal) 
crime_reasons
barplot(height = crime_reasons[order(crime_reasons,decreasing = TRUE)] ,col = 'navy blue')
#top 3 reasons = LARCENY-FROM VEHICLE,LARCENY-NON VEHICLE and   AUTO THEFT 

#c. Which all attributes have correlation with crime rate?

crime_cor <-cor(atlanta_crime)
crime_cor
library(corrplot)
corrplot(crime_cor, order= "AOE", method= "circle",addCoef.col = "red")
#offense id , loc_type , bea
