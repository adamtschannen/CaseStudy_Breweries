---
  title: "Breweries and Beer Case Study"
author: "Adam Tschannen"
date: "6/29/2017"
output: html_document
---
  
With the data sets of Beers and Breweries we analyized and found the number of breweries in each state, the median alcohol content and international bitterness unit for each state, which state has the maximum alcoholic beer, which state has the most bitter beer, a summary of the Alcohol by volume and the relationship between the bitterness of the beer and its alcoholic content? 
```{r}
# read in csv files 
brew<- read.csv(file="~/Downloads/Breweries_CaseStudy1.csv")
beer<- read.csv(file="~/Downloads/Beers_CaseStudy1.csv")
```

Below we can see the number of breweries in each state in in accending order with Colorado having the most amount of breweries with 47 and DC, North Dakota, South Dakota and West Virginia having the least amount of breweries with 1. 

```{r}
#part1
#count number of breweries in each state
#i ploted jsut for visualization 
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

Next we merged the beer data with breweries data by brewery id. We did this so we would have all of our data on one sheet. To check that we executedt this step correctly we printed first 6 observations and the last six observations to check the merged file.

The first step in merging was making sure that both data sets had the same variable name 'Brewery_id' so that we could merge them. 
```{r}
names(brew)[1]<-"Brewery_id"
```
Next we merged the data using our join fucnion with library package plyr.
```{r}
library(plyr)
brewbeer <- join(x=brew, y=beer, by= 'Brewery_id')
```
Once we succesfully merged the two data sets I checked there structure of the data and labeled all of the missing variable names. 
```{r}
str(brewbeer)
names(brewbeer)[3]<-"City"
names(brewbeer)[4]<-"State"
names(brewbeer)[2]<-"Brewering_Company"
```

finally, we print out the first and last 6 observations to check the merged file.
```{r}
head(brewbeer, n=6)
tail(brewbeer, n=6)
```

Now, to see how many missing varibales are in our data, we report the number of NA’s in each column.
```{r}
sapply(brewbeer, function(x) sum(is.na(x)))
```
What we found is that there were 62 missing values in ABV ad 1005 missing values in IBU. 

Next we found the median alcohol content and international bitterness unit for each state. 
```{r}
library(doBy)
MedABVperState<-summaryBy(ABV ~ State, data = brewbeer, FUN = median, na.rm = TRUE)
MedIBUperState<-summaryBy(IBU ~ State, data = brewbeer, FUN = median, na.rm = TRUE)
```
To get a visualization of our findinds we then plotted our data in the form of a bar chart. 
```{r}
library(ggplot2)
MedABVperStatePlot<-ggplot(brewbeer, aes(x=factor(State), y=ABV)) + stat_summary(fun.y="median", geom="bar", na.rm = TRUE)
MedIBUperStatePlot<-ggplot(brewbeer, aes(x=factor(State), y=IBU)) + stat_summary(fun.y="median", geom="bar", na.rm =TRUE)
```

We found CO has the maximum alcoholic beer and OR  has the most bitter beer.
```{r}
StateWMaxABV<-brewbeer[which.max(brewbeer$ABV),] 
StateWMaxIBU<-brewbeer[which.max(brewbeer$IBU),]
```

Then to find a statistcal summary for ABV (Alcohol by volume) variable we found the below stats.
```{r}
summary(brewbeer$ABV)
```
Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
0.00100 0.05000 0.05600 0.05977 0.06700 0.12800

To find the relationship between the bitterness of the beer and its alcoholic content we made a scatter plot and checked its correlation .
```{r}
plot<-plot(brewbeer$ABV, brewbeer$IBU,xlab = "Alcohol Content", ylab = "bitterness of the beer",abline(lm(brewbeer$IBU ~brewbeer$ABV), col="red"))
#check for correlation
alcbitcor<-cor(brewbeer$ABV, brewbeer$IBU,use = "pairwise.complete.obs", method = "pearson")

```

