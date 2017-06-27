
# read in csv files 
brew<- read.csv(file="~/Downloads/Breweries_CaseStudy1.csv")
beer<- read.csv(file="~/Downloads/Beers_CaseStudy1.csv")

#count number of breweries in each state
#plot for visualization 
BrewPerState <- table(brew$State)
plot(BrewPerState,ylab = "number of breweries", ylim=c(0,50))

