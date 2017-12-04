#load files 

train <- read.csv('train.csv', header = TRUE)
test <- read.csv('test.csv', header = TRUE)

# combine two datasets but test set doesn't have survive so, added
# as none where survived information is not available.add a survived 
# variable
survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# merge two data sets

data.merged <- rbind(train, survived)

# check the structure of R object and display
str(data.merged)

# survived variable changed to factor
# passenger class is also changed to factor by using $ sign

data.merged$Survived <- as.factor(data.merged$Survived)
data.merged$Pclass <- as.factor(data.merged$Pclass)

# check the servived or not servived date using table function
table(data.merged$Survived)
table(data.merged$Pclass)

#load ggplot2 for graphical presentation
library(ggplot2)

# Initial hypothesis: upper level passenger has higher chances of survivel

train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + stat_count(width = 0.7) +
  xlab("Pclass") + 
  ylab ("Total Number of Passenger") +
  labs(fill = "Survived")

# check the head command, print out some first names in the dataset

head(as.character(train$Name))
length(unique(as.character(data.merged$Name)))
duplicate.names <- as.character(data.merged[which(duplicated(as.character(data.merged$Name))), 'Name'])
data.merged[which(data.merged$Name %in% duplicate.names),]

misses <- data.merged[which(str_detect(data.merged$Name, "Miss")),]

# data extration based on title: 

extractName <- function(Name){
  Name <- as.character(Name)
  if(length(grep("Miss.", Name)) > 0) {
    return ("Miss.")
  } else if(length(grep("Master.", Name))> 0) {
    return("Master.")
  } else if (length(grep("Mr.", Name)) > 0){
    return("Mr.")
  } else if (length(grep("Mrs.", Name)) > 0){
    return("Mrs.")
  }else {
    return("Other.")
  }
}

titles <- NULL
for(i in 1:nrow(data.merged)){
  titles <- c(titles, extractName(data.merged[i, "Name"]))
}

data.merged$title <- as.factor(titles)


# survival label data

ggplot(data.merged[1:891,], aes(x = title, fill = Survived)) +
  geom_bar(stat = "count") +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

table(data.merged$Sex)

ggplot(data.merged[1:891,], aes(x = Age, fill = Survived)) +
  geom_bar(stat = "count") +
  facet_wrap(~Sex + Pclass) +
  geom_bar(width = 10) +
  xlab("Age") +
  ylab("Total Count")
  

# Master counted as boys. Hypothesis, or proxy for male children

boys <- data.merged[which(data.merged$title == "Master."),]
summary(boys$Age)
ggplot(boys[boys$Survived != "None",], aes(x = Age, fill = Survived)) +
  geom_bar(stat = "count") +
  facet_wrap(~Sex + Pclass) +
  geom_bar(width = 10) +
  xlab("Age of Master.") +
  ylab("Total Count")

# Hypothesis has failed here, when we chech the Mean, Median and max values, 
# it shows women not the female kids.
misses <- data.merged[which(data.merged$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  geom_bar(stat = "count") +
  facet_wrap(~Sex + Pclass) +
  geom_bar(width = 10) +
  xlab("Age of Misses") +
  ylab("Total Count")

misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

data.merged[data.merged$Embarked == '', "Embarked"] <- 'S'





