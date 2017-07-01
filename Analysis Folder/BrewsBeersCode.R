
# read in csv files 
brew<- read.csv(file="~/Downloads/Breweries_CaseStudy1.csv")
beer<- read.csv(file="~/Downloads/Beers_CaseStudy1.csv")

#part1
#count number of breweries in each state
#i ploted jsut for visualization 
BrewPerState <- table(brew$State)
sort(BrewPerState)
plot(BrewPerState,ylab = "number of breweries", ylim=c(0,50))

#part 2
# change the column name Brew_id in Breweries_CaseStudy1 data set
# to match the column name Brewery_id in the Beers_CaseStudy1 data
# set
names(brew)[1]<-"Brewery_id"    #option 1

#part 2a
#joining the two data sets
#need library plyr for this code
library(plyr)
brewbeer <- join(x=brew, y=beer, by= 'Brewery_id')

str(brewbeer)
names(brewbeer)[3]<-"City"
names(brewbeer)[4]<-"State"
names(brewbeer)[2]<-"Brewering_Company"

#part 2b
#printing out first and last 6 obs in the new merged data set
head(brewbeer, n=6)
tail(brewbeer, n=6)

#part 3- reportt the number of na'sin each column 
sapply(brewbeer, function(x) sum(is.na(x)))

#part 4 -Compute the median alcohol content and international
#bitterness unit for each state. Plot a bar chart to compare.
library(doBy)
MedABVperState<-summaryBy(ABV ~ State, data = brewbeer, FUN = median, na.rm = TRUE)
MedIBUperState<-summaryBy(IBU ~ State, data = brewbeer, FUN = median, na.rm = TRUE)

library(ggplot2)
MedABVperStatePlot<-ggplot(brewbeer, aes(x=factor(State), y=ABV)) + stat_summary(fun.y="median", geom="bar", na.rm = TRUE)
MedIBUperStatePlot<-ggplot(brewbeer, aes(x=factor(State), y=IBU)) + stat_summary(fun.y="median", geom="bar", na.rm =TRUE)

#part 5 -Which state has the maximum alcoholic beer? Which state has the most bitter beer?

#the code finds the max value in a column and then pulls the whole row
StateWMaxABV<-brewbeer[which.max(brewbeer$ABV),] 
StateWMaxIBU<-brewbeer[which.max(brewbeer$IBU),]


#part 6- Summary statistics for ABV (Alcohol by volume) variable
summary(brewbeer$ABV)

#part 7- Is there a relationship between the bitterness of the beer
#and its alcoholic content? Draw a scatter plot.

#plots alcoholic content vs bitterness
plot<-plot(brewbeer$ABV, brewbeer$IBU,xlab = "Alcohol Content", ylab = "bitterness of the beer",abline(lm(brewbeer$IBU ~brewbeer$ABV), col="red"))
#finds the correlation between acloholic content and bitterness of beer
alcbitcor<-cor(brewbeer$ABV, brewbeer$IBU,use = "pairwise.complete.obs", method = "pearson")


#check
alcVSbit<-ggplot(data=brewbeer, aes(x=brewbeer$ABV, y=brewbeer$IBU)) + geom_point(shape=1) + geom_smooth(method=lm) + ggtitle("Bitterness of Beer and Alcohol Content Reationship")+labs(x="Alcoholic Content", y="bitterness of Beer") + xlim(0,.15)+ylim(0,150)















