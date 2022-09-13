# Load the data
setwd("C:/Users/garre/desktop/DATA630")
wages <- read.csv("wages.csv", as.is = FALSE)

# Explore the data
summary(wages)
str(wages)
View(wages)

#Load the packages
library("arules")
library("arulesViz")
library("tidyverse")
library("gridExtra")

#Data preprocessing
#Following columns are continuous numbers.Utilized cut function for binning.
wages$age<-cut(wages$age, breaks =c(0, 19, 30, 45, 64), 
               labels=c('Teen', 'Young', 'Adult', 'Older Adult'))

wages$wage<-cut(wages$wage, breaks =c(0, 5.25, 9.024, 11.25, 44.5), 
                labels=c('Low Wage', 'Medium Wage','High Wage',
                         'Very High Wage'))

wages$experience<-cut(wages$experience, breaks =c(-1, 5, 10, 15, 25, 56), 
                      labels=c('Very Low Experience','Low Experience',
                               'Medium Experience','High Experience',
                               'Very High Experience'))

wages$education<-cut(wages$education, breaks =c(0, 11, 12, 15, 16, 18), 
                     labels=c('Some High School','High School Diploma',
                              'Some College','4-Year Degree',
                              'More than 4-Year Degree'))

wages$sex<-factor(wages$sex, levels=c(0,1),labels=c('Male', 'Female'))


wages$race<-factor(wages$race, levels=c(1,2,3),
                   labels=c('Other','Hispanic','White'))

wages$occupation<-factor(wages$occupation, levels=c(1,2,3,4,5,6),
                         labels=c('Management','Sales','Clerical','Service',
                                  'Professional','Other'))

#Removing categories not applicable to this analysis
wages$south<-NULL
wages$union<-NULL
wages$sector<-NULL
wages$marital_status<-NULL

#Exploratory Data Analysis
p1 <- ggplot(wages, aes(sex, ..count..,fill=sex)) + geom_bar() + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p2 <- ggplot(wages, aes(race, ..count..,fill=race)) + geom_bar() + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p3 <- ggplot(wages, aes(age, ..count..,fill=age)) + geom_bar()+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p4 <- ggplot(wages, aes(education, ..count..,fill=education)) + geom_bar()+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p5 <- ggplot(wages, aes(experience, ..count..,fill=experience)) + geom_bar()+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

p6 <- ggplot(wages, aes(occupation, ..count..,fill=occupation)) + geom_bar()+ 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3)

p7 <- ggplot(wages, aes(wage, ..count..)) + geom_bar(aes(fill=sex),
                                                     position='dodge')
p8 <- ggplot(wages, aes(wage, ..count..)) + geom_bar(aes(fill=race),
                                                     position='dodge')
p9 <- ggplot(wages, aes(wage, ..count..)) + geom_bar(aes(fill=age),
                                                     position='dodge')
p10 <- ggplot(wages, aes(wage, ..count..)) + geom_bar(aes(fill=education),
                                               position='dodge')
p11 <- ggplot(wages, aes(wage, ..count..)) + geom_bar(aes(fill=experience),
                                               position='dodge')
p12 <- ggplot(wages, aes(wage, ..count..)) + geom_bar(aes(fill=occupation),
                                               position='dodge')

grid.arrange(p7,p8,p9,p10,p11,p12,nrow=3)

#Run the Apriori method with default parameters
rules<-apriori(wages)

#Preview the first 10 rules
inspect(rules[1:10])

#Run the summary command on rules
summary(rules)

#Adjusting support and confidence options
rules <- apriori(wages, parameter= list(minlen=2, supp=0.15, conf=0.85))

#Sorting
#sort rules by lift descending and preview the first 10 rules
inspect(head(sort(rules, by="lift"),10))
#sort rules by lift ascending
inspect(head(sort(rules, by="lift", decreasing = F),10))


#Rules Pruning
#Create a copy of the rules sorted by lift
rules.sorted <- sort(rules, by="lift")

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
redundant <- redundant[redundant == TRUE]

# remove redundant rules
rules.pruned <- rules.sorted[!is.redundant(rules.sorted, 
                                           measure = "confidence")]
inspect(head(rules.pruned, 10))

#how many rules remain after pruning
length(rules.pruned)
summary(rules.pruned)
length(rules.sorted[is.redundant(rules.sorted, measure = "confidence")])
inspect(rules.pruned)

#Visualizing the rules

#Scatterplot
plot(rules.pruned)
plot(rules.pruned, method="graph")
plot(rules.pruned,method="grouped", shading="confidence",  measure="support")
plot(rules.pruned, shading="order", control=list(main = "Two-key plot"))





