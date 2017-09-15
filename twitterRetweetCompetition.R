#Kaggle Competition #2
#Predicting number of twitter retweets
#Chai Grindean

#Read Files
#setwd("C:/Users/.....

train <- read.csv('train.csv')
test <- read.csv('test.csv')


#library(lubridate)
#train$Time<-ymd_hms(train$Time) #change to lubridates format
#str(train)

#Creating Column for Retweet_count
test$Retweet_count <- ''
#=====================================================================
#Getting Word count for text column
#Might be able to use wordcount as a predictive power.

#Check out to use without data.frame
train_word_count <-data.frame(Char_count=apply(train,2,nchar)[,2]) #Word Count for Train
train <- cbind(train, train_word_count)
test_word_count<-data.frame(Char_count=apply(test,2,nchar)[,2]) #Word count for Test
test <- cbind(test, test_word_count)
#=====================================================================
#Append test and train together into dataframe title 'full'
train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE
full <- rbind(train,test)

#=====================================================================
#Extract Hashtags
#install.packages('stringr') #instill packages if needed
#install.packages('dplyr')
library(stringr)
library(dplyr)
full <- full %>%
  mutate (Hashtag = str_extract(Text,"#+[a-zA-z]+"))
#=====================================================================
#Plot to visulize basic information
plot(train$Id,train$Retweet_count, main = 'ID & Retweets', xlab = 'ID', ylab = 'Retweets')
plot(train$Time, train$Retweet_count, main = 'Time & Retweets', xlab = "Date", ylab = 'Retweets')
summary(train)

#=====================================================================
#More Packages, remove comment and install if needed
#install.packages('NLP')
library(NLP)
#install.packages('tm')
library(tm)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("SnowballC")
library(SnowballC)
#install.packages("wordcloud")
library(wordcloud)


#testv2 <- read.csv('test.csv', stringsAsFactors = FALSE) #used for testing removal of stem words
trainCorpus <- Corpus(VectorSource(train$Text))
trainCorpusTest <- Corpus(VectorSource(train$Text))
#convert corpus into plain text
trainCorpus <- tm_map(trainCorpus, PlainTextDocument)
#Remove Stopwords & Punctuation
trainCorpus <- tm_map(trainCorpus, removePunctuation)
trainCorpus <- tm_map(trainCorpus, removeWords, stopwords('english'))
#Convert words to stem word
#Noticed that remove stem, removed a lot of words ending in 'e'
#testv2Corpus <- tm_map(testv2Corpus, stemDocument)
#convert to utf-8 to fix symbols
trainCorpus <- iconv(trainCorpus, to = "utf-8")
#plot WordCloud to show top 50 most used words.
wordcloud(trainCorpus, max.words = 50, random.order = FALSE, colors = brewer.pal(6,"Dark2"), scale = c(2,0.7))
#Could use these for some predictive power
#keywords = nigeriadecides, nigeria, election/s, boko, presidential.
#END OF WORDCLOUD

#=====================================================================
#Term-Document Matrix
#Showed total word count
#Potentially can be used for predictive model
dtm<- TermDocumentMatrix(trainCorpusTest)
m <- as.matrix(dtm)
v <-sort(rowSums(m),decreasing = TRUE)
most_used_words <- data.frame(word = names(v),freq=v)
most_used_words[1:100,] #top 100 most used words

#=====================================================================*
#Split the data back out
train <-full[full$IsTrainSet==TRUE,]
test <- full[full$IsTrainSet==FALSE,]

#=====================================================================
#Categorical Casting - Change to Factors to work with Predictive Model
train$Id <-as.factor(train$Id)
train$Time <-as.factor(train$Time)
#train$Char_count <- as.factor(train$Char_count) MIGHT NEED LATER
train$Hashtag <- as.factor(train$Hashtag)
#=====================================================================
#Predictions #1 GLM with just Char_Count
train$Retweet_count <- as.integer(train$Retweet_count)
my_glm <- glm(Retweet_count ~ Char_count, data=train, family = "poisson") #Model #1
prediction1 <- predict(my_glm, newdata = test)
solution1 <- data.frame(Id=test$Id, Retweet_count= prediction1)
write.csv(solution1, file="solution1.csv",row.names = FALSE) #Score of 67.65123 on Kaggle

#Prediction #2*****
model2 <- lm(Retweet_count ~ Char_count, data=train) #Model #2
my_lm <-predict.lm(model2, newdata = test, interval = "predict")
solution2 <- data.frame(Id = test$Id, Retweet_count = my_lm[1:2000,1])
write.csv(solution2, file="solution2.csv",row.names = FALSE) #WOOHOO TOP 10! Score = 64.41816
#You made the top ten by improving your score by 3.23307. 
#You just moved up 20 positions on the leaderboard.
#=====================================================================
#Random Forest Model
#install.packages("randomForest")
library(randomForest)
Retweet_equation <- "Retweet_count ~ Char_count + Hashtag"
Retweet_formula <-as.formula(Retweet_equation)
#Creating Forest
#Omit missing NA to predict with Hashtag****
train_na_omit<-na.omit(train$Hashtag)
randomForest.model <- randomForest(formula = Retweet_formula, data = train,na.action = "na.exclude", ntree=5000, mtry=5, nodesize =0.01 *nrow(train))
#=====================================================================
#Creating Descision Tree #attempting to fix with na.exclude
library(rpart)
#install.packages("rattle")
library(rattle)
library(rpart.plot)
my_tree <-rpart(Retweet_count ~ Char_count + Hashtag, data = train, na.action = "na.exclude", method = "class") #R freezes
my_tree_predictions <-predict(my_tree, newdata = test, type = "class")

#=====================================================================
#CONCLUSION
#Overall I was able to extract the character count, thinking maybe I would see if longer tweets would get more/less retweets
#I also took out the Hashtags in order to see if perhaps a certain # was getting a bunch of retweets
#The biggest issue I came across was trying to make a model that would work with the different factors I.E char, factor, int
#If I were able too, I would have liked to use a predictive model using the Time (everytime I used Time it froze and crashed my RStudio)
#As well as using the features I pulled. The only one I was able to apply was a glm with just char count.

