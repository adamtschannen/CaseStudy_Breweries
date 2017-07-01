---
  title: "Breweries and Beer Case Study"
author: "Adam Tschannen"
date: "6/29/2017"
output: html_document
---
  
  "a. Introduction to the project. The introduction should not start with For my project I .""

With the data sets of Beers and Breweries we analyzed and found the number of breweries in each state, the median alcohol content and international bitterness unit for each state, which state has the maximum alcoholic beer, which state has the most bitter beer, a summary of the Alcohol by volume and the relationship between the bitterness of the beer and its alcoholic content? 
```{r}
# read in csv files 
brew<- read.csv(file="~/Downloads/Breweries_CaseStudy1.csv")
beer<- read.csv(file="~/Downloads/Beers_CaseStudy1.csv")
```

Below we can see the number of breweries in each state in in ascending order with Colorado having the most number of breweries with 47 and DC, North Dakota, South Dakota and West Virginia having the least number of breweries with 1. 

```{r}
#part1
#count number of breweries in each state
#I also plotted just for visualization 
BrewPerState <- table(brew$State)
sort(BrewPerState)
plot(BrewPerState,ylab = "number of breweries", ylim=c(0,50))
```

DC  ND  SD  WV  AR  DE  MS  NV  AL  KS  NH  NJ  TN  HI  KY  NM  SC  UT  WY  IA  
1   1   1   1   2   2   2   2   3   3   3   3   3   4   4   4   4   4   4   5   

ID LA  NE  RI  OK  AK  GA  MD  CT  ME  MO  MT  VT  AZ  MN  FL  OH  NY  VA  IL  NC 
5  5   5   5   6   7   7   7   8   9   9   9  10  11  12  15  15  16  16  18  19   

WI IN  MA  WA  PA  TX  OR  MI  CA  CO 
20 22  23  23  25  28  29  32  39  47

Next, we merged the beer data with breweries data by brewery id. We did this so we would have all of our data on one sheet. To check that we executed this step correctly we printed first 6 observations and the last six observations to check the merged file.

The first step in merging was making sure that both data sets had the same variable name 'Brewery_id' so that we could merge them. 
```{r}
#this names the first column 'Brewery_id' so we can merge the files
names(brew)[1]<-"Brewery_id"
```
Next, we merged the data using our join function with library package plyr.
```{r}
# use library plyr
#join or 'merge' the two data sets of brew and beer by their common column 'Brewery_id'
library(plyr)
brewbeer <- join(x=brew, y=beer, by= 'Brewery_id')
```
Once we successfully merged the two data sets, I checked the structure of the data and labeled all of the missing variable names. 
```{r}
#look at the structure of the data to find what column names we are missing
str(brewbeer)
#label the missing columns 
names(brewbeer)[3]<-"City"
names(brewbeer)[4]<-"State"
names(brewbeer)[2]<-"Brewering_Company"
```

finally, we print out the first and last 6 observations to check the merged file.
```{r}
#fing the top 6 and bottom six rows for the new data set brewbeer
head(brewbeer, n=6)
tail(brewbeer, n=6)
```

Now, to see how many missing variables are in our data, we report the number of NA’s in each column.
```{r}
#find all of the missing values, 'NA', in each column
sapply(brewbeer, function(x) sum(is.na(x)))
```
What we found is that there were 62 missing values in ABV and 1005 missing values in IBU. 

Next, we found the median alcohol content and international bitterness unit for each state. 
```{r}
#use library doby
library(doBy)
#finds the median ABV per state and the median IBU per state
MedABVperState<-summaryBy(ABV ~ State, data = brewbeer, FUN = median, na.rm = TRUE)
MedIBUperState<-summaryBy(IBU ~ State, data = brewbeer, FUN = median, na.rm = TRUE)
#finds the range of the medians above
range(MedABVperState$ABV.median)
range(MedIBUperState$IBU.median,na.rm=TRUE)
```
To get a visualization of our findings we then plotted our data in the form of a bar chart. 
```{r}
#use library ggplot2
library(ggplot2)
#plot our findings 
MedABVperStatePlot<-ggplot(brewbeer, aes(x=factor(State), y=ABV)) + stat_summary(fun.y="median", geom="bar", na.rm = TRUE)
MedIBUperStatePlot<-ggplot(brewbeer, aes(x=factor(State), y=IBU)) + stat_summary(fun.y="median", geom="bar", na.rm =TRUE)
```

We found CO has the maximum alcoholic beer and OR  has the most bitter beer.
```{r}
#Find the State with the max ABV and IBU
StateWMaxABV<-brewbeer[which.max(brewbeer$ABV),] 
StateWMaxIBU<-brewbeer[which.max(brewbeer$IBU),]
```

Then to find a statistical summary for ABV (Alcohol by volume) variable we found the below stats.
```{r}
#summarize ABV
summary(brewbeer$ABV)
```
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.00100 0.05000 0.05600 0.05977 0.06700 0.12800

To find the relationship between the bitterness of the beer and its alcoholic content we made a scatter plot and checked its correlation .
```{r}
#finding the relationship between ABV and IBU
plot<-plot(brewbeer$ABV, brewbeer$IBU,xlab = "Alcohol Content", ylab = "bitterness of the beer",abline(lm(brewbeer$IBU ~brewbeer$ABV), col="red"))
#check for correlation
alcbitcor<-cor(brewbeer$ABV, brewbeer$IBU,use = "pairwise.complete.obs", method = "pearson")
#find confidence intervals and test of the correlation 
alcbitcortest<-cor.test(brewbeer$ABV, brewbeer$IBU,use = "pairwise.complete.obs", method = "pearson")
```


