
# read in csv files 
brew<- read.csv(file="~/Downloads/Breweries_CaseStudy1.csv")
beer<- read.csv(file="~/Downloads/Beers_CaseStudy1.csv")

#part1
#count number of breweries in each state
#plot for visualization 
BrewPerState <- table(brew$State)
plot(BrewPerState,ylab = "number of breweries", ylim=c(0,50))

#part 2
# change the column name Brew_id in Breweries_CaseStudy1 data set
# to match the column name Brewery_id in the Beers_CaseStudy1 data
# set
names(brew)[1]<-"Brewery_id"    #option 1
names(brew) <- c("Brewery_id")  #option 2 

#part 2a
#joining the two data sets
#need library plyr for this code
library(plyr)
brewbeer <- join(x=brew, y=beer, by= 'Brewery_id')

#part 2b
#printing out first and last 6 obs in the new merged data set
head(brewbeer, n=6)
tail(brewbeer, n=6)

#part 3- reportt the number of na'sin each column 
sapply(brewbeer, function(x) sum(is.na(x)))

#part 4 -Compute the median alcohol content and international
#bitterness unit for each state. Plot a bar chart to compare.

MedianABVperState<-ggplot(brewbeer, aes(x=factor(State), y=ABV)) + stat_summary(fun.y="median", geom="bar", rm.na=TRUE)
MedianIBUPerState<-ggplot(brewbeer, aes(x=factor(State), y=IBU)) + stat_summary(fun.y="median", geom="bar", rm.na=TRUE)

#part 5 -Which state has the maximum alcoholic beer? Which state has the most bitter beer?

#the code finds the max value in a column and then pulls the whole row
StateWMaxABV<-brewbeer[which.max(brewbeer$ABV),] 
StateWMaxIBU<-brewbeer[which.max(brewbeer$IBU),]


alcmax<-max(brewbeer[,"ABV"], na.rm = TRUE) #na.rm=TRUE removes na values 
IBUmax<-max(brewbeer$IBU, na.rm = TRUE)

#part 6- Summary statistics for ABV (Alcohol by volume) variable
summary(brewbeer$ABV)

#part 7- Is there a relationship between the bitterness of the beer
#and its alcoholic content? Draw a scatter plot.

#plots alcoholic content vs bitterness
plot<-plot(brewbeer$ABV, brewbeer$IBU,xlab = "Alcohol Content", ylab = "bitterness of the beer",abline(lm(brewbeer$IBU ~brewbeer$ABV), col="red"))
#finds the correlation between acloholic content and bitterness of beer
cor(brewbeer$ABV, brewbeer$IBU,use = "pairwise.complete.obs", method = "pearson")

















