---
title: "Breweries and Beer Case Study"
author: "Adam Tschannen"
date: "6/29/2017"
output: html_document
---


With the data sets of Beers and Breweries we analyzed and found the number of breweries in each state, the median alcohol
content and international bitterness unit for each state, which state has the maximum alcoholic beer, which state has the
most bitter beer, a summary of the Alcohol by volume and the relationship between the bitterness of the beer and its alcoholic
content.

```{r}
# read in csv files 
brew<- read.csv(file="~/Downloads/Breweries_CaseStudy1.csv")
beer<- read.csv(file="~/Downloads/Beers_CaseStudy1.csv")
```

Below we can see the number of breweries in each state in in ascending order with Colorado having the most number of breweries
with 47 and DC, North Dakota, South Dakota and West Virginia having the least number of breweries with 1. 

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

Next, we merged the beer data with breweries data by 'Brewery_id'. We did this so we would have all of our
data on one sheet. To check that we executed this step correctly we printed the first six observations and 
the last six observations to check the merged file.

The first step in merging was making sure that both data sets had the same variable name 'Brewery_id' so 
that we could merge them. 
```{r}
#part 2
#this names the first column 'Brewery_id' so we can merge the files
names(brew)[1]<-"Brewery_id"
```
Next, we merged the data using our join function with library package plyr.
```{r}
#part 2a
# use library plyr
#join or 'merge' the two data sets of brew and beer by their common column 'Brewery_id'
library(plyr)
brewbeer <- join(x=brew, y=beer, by= 'Brewery_id')
```
Once we successfully merged the two data sets, I checked the structure of the data and labeled all of the 
missing variable names. 

```{r}
#part 2b
#look at the structure of the data to find what column names we are missing
str(brewbeer)
#label the missing columns 
names(brewbeer)[3]<-"City"
names(brewbeer)[4]<-"State"
names(brewbeer)[2]<-"Brewering_Company"
```

finally, we print out the first and last 6 observations to check the merged file.
```{r}
#part 2c
#fing the top 6 and bottom six rows for the new data set brewbeer
head(brewbeer, n=6)
tail(brewbeer, n=6)
```

Now, to see how many missing variables are in our data, we report the number of NA’s in each column.
```{r}
#part 3
#find all of the missing values, 'NA', in each column
sapply(brewbeer, function(x) sum(is.na(x)))
```
What we found is that there are 62 missing values in ABV and 1005 missing values in IBU. 

Next, we found the median alcohol content and international bitterness unit for each state. 
```{r}
#part4
#use library doby
library(doBy)
#finds the median ABV per state and the median IBU per state
MedABVperState<-summaryBy(ABV ~ State, data = brewbeer, FUN = median, na.rm = TRUE)
MedABVperState
MedIBUperState<-summaryBy(IBU ~ State, data = brewbeer, FUN = median, na.rm = TRUE)
MedIBUperState
#finds the range of the medians above(this is more so for my conclusion) 
range(MedABVperState$ABV.median)
range(MedIBUperState$IBU.median,na.rm=TRUE)
```
To get a visualization of our findings we plotted our data in the form of a bar chart. 
```{r}
#part 4 continue
#use library ggplot2
library(ggplot2)
#plot our findings 
MedABVperStatePlot<-ggplot(brewbeer, aes(x=factor(State), y=ABV)) + stat_summary(fun.y="median", geom="bar", na.rm = TRUE)
MedABVperStatePlot
MedIBUperStatePlot<-ggplot(brewbeer, aes(x=factor(State), y=IBU)) + stat_summary(fun.y="median", geom="bar", na.rm =TRUE)
MedIBUperStatePlot

```

We found Colorado has the maximum alcoholic beer and Oregon has the most bitter beer.
```{r}
#part 5
#Find the State with the max ABV and IBU
StateWMaxABV<-brewbeer[which.max(brewbeer$ABV),] 
StateWMaxABV
StateWMaxIBU<-brewbeer[which.max(brewbeer$IBU),]
StateWMaxIBU
```

Then to find a statistical summary for ABV (Alcohol by volume) variable we found the below stats.
```{r}
#part 6
#summarize ABV
summary(brewbeer$ABV)
```
  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.00100 0.05000 0.05600 0.05977 0.06700 0.12800

To find the relationship between the bitterness of the beer and its alcoholic content we made a scatter plot 
and checked its correlation and we found there is .67 correlation with confidence intervals of .64 and .69 which 
means the correlation is statistically significant. 

```{r}
#part 7
#finding the relationship between ABV and IBU
plot<-plot(brewbeer$ABV, brewbeer$IBU,xlab = "Alcohol Content", ylab = "bitterness of the beer",abline(lm(brewbeer$IBU ~brewbeer$ABV), col="red"))
#check for correlation
alcbitcor<-cor(brewbeer$ABV, brewbeer$IBU,use = "pairwise.complete.obs", method = "pearson")
alcbitcor
#find confidence intervals and test of the correlation 
alcbitcortest<-cor.test(brewbeer$ABV, brewbeer$IBU,use = "pairwise.complete.obs", method = "pearson")
alcbitcortest
```
 
In conclusion, we found that Colorado was the State with the most breweries with
47 and DC, North Dakota, South Dakota and West Virginia were tied for the 
State with the least number of breweries. We found that the range of median 
alcohol by volume is between 4% and 6.25% while the range of median International
Bitterness Unit is between 19 and 61. The state with the max alcohol by volume 
is Colorado with 12.8% while the state with the max International Bitterness Unit
was Oregon with 138. Furthermore,  we found a summary of the alcohol by volume and that 
the min was %.1 the first quartile was  %.5000 the median is %5.6 the mean is  
%5.98 the third quartile %6.7 and the max is  %12.8. Finally, we found that 
there is a relatively strong correlation of .67 between alcohol by volume and the 
bitterness of beer of which is statistically significant with confidence intervals between  (0.6407982, 0.6984238).


