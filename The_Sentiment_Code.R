library(tidyverse)
library(sentimentr)
library(tm)
library(SentimentAnalysis)
library(wordcloud)
library(textir)
library(tidyr)
library(NLP)
library(syuzhet)
library(twitteR)
library (stringr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(devtools)
library(pacman)
library(dplyr)
library(plyr)
library(ROAuth)
library(RTextTools)
library(e1071)
library(SnowballC)
library(SentimentAnalysis)
library(httr)
getwd()
setwd("/Users/bosco")

#READ THE FILE FROM DIRECTORY
Sentiment_Excel<-read.csv("/Users/bosco/Trump.csv",stringsAsFactors = FALSE, header=TRUE,row.names=NULL,encoding = "UTF-8")
View(Sentiment_Excel)
str(Sentiment_Excel)

#CONVERT THE DATAFRAME TO CORPUS
#Sentiment_Corpus<-Corpus(VectorSource(Sentiment_Excel$ModeofT.TransportKK.Outcome1.Mensaservice,(encoding = "UTF-8"),+(readerControl = list(language = "en_US"))))
(Sentiment_Corpus<-VCorpus(VectorSource(Sentiment_Excel$text)))
View(Sentiment_Corpus)
inspect(Sentiment_Corpus)

#REMOVE ALL EXTRA WHITE SPACE 
(Sentiment_whitespace<-tm_map(Sentiment_Corpus, stripWhitespace))
View(Sentiment_whitespace)

#TRANSFROM ALL LETTER TO LOWER CASE 
(Sentiment_lower_case<-tm_map(Sentiment_whitespace, content_transformer(tolower)))
View(Sentiment_lower_case)
View(Sentiment_lower_case[11])

#REMOVE ALL STOP WORDS
(Sentiment_stop_words<-tm_map(Sentiment_lower_case, removeWords, stopwords("english")))


#REMOVE PUNCTUATION
(Sentiment_stop_words<-tm_map(Sentiment_stop_words, removePunctuation))

#STEMMING THE DOCUMENT
(Sentiment_stemming<-tm_map(Sentiment_stop_words, stemDocument))
View(Sentimen_stemming)

#REMOVE NUMBERS
(Sentiment_stop_words<-tm_map(Sentiment_stop_words,removeNumbers))
View(Sentiment_stop_words)

#EXPORT THE PROCESEED FILE.
Sentiment_stop_words <- tm_map(Sentiment_stop_words, PlainTextDocument)
#save in indiv text file
writeCorpus(Sentiment_stop_words)
#write 1 file
tmp <- Sentiment_stop_words[1]

dataframe<-data.frame(text=unlist(sapply(Sentiment_stop_words, `[`, "content")), stringsAsFactors=F)
write.csv(dataframe, "Age_LT25.csv")


#TERM DOCUMENT MATRIX/DOCUMENT TERM MATRIX (OTHER WAY ROUND)
(Sentiment_term<-DocumentTermMatrix(Sentiment_stemming))%>%removeSparseTerms(1 -(5/length(Sentiment_stemming)))
View(Sentiment_term)
class(Sentiment_term)

#DEFINING THE FREQUENCY
(Sentiment_freq<-sort(colSums(as.matrix(Sentiment_term)), decreasing = TRUE))
(Sentiment_df<-data.frame(Word=names(Sentiment_freq), Frequency=Sentiment_freq))
View(Sentiment_freq)
View(Sentiment_df)
# CREATE FREQUENCY TABLE
(tableSS <- data.frame(word = names(Sentiment_freq),
                       absolute.frequency = Sentiment_freq,
                       relative.frequency = Sentiment_freq/length(Sentiment_freq)))
View(tableSS)

#FIND FREQUENT OCCURING WORDS (NUMBER CAN BE CHANGED)
findFreqTerms(Sentiment_term, 10)


#PLOTING MOST FREQUENT WORD (World cloud in bar form)
subset(Sentiment_df, Frequency>200)    %>%
  ggplot(aes(Word, Frequency)) +
  geom_bar(stat="identity", fill="green2", colour="darkgreen") +ggtitle("Trump Tweet Used Words frequency ")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

#TERM DOCUMENT MATRIX [(Sentiment_term<-DocumentTermMatrix(Sentimen_stemming))]
(Sentiment_Doc<-TermDocumentMatrix(Sentiment_stemming))
str(Sentiment_Doc)
inspect(Sentiment_Doc)

#CREATING WORLD CLOUD
(Sentiment_Wordcloud<-as.matrix(Sentiment_Doc))
(Sentiment_Wordcloud<-sort(rowSums(Sentiment_Wordcloud),decreasing = TRUE))
(Sentiment_Wordcloud<-data.frame(word=names(Sentiment_Wordcloud),freq=Sentiment_Wordcloud))
(head(Sentiment_Wordcloud,10))
set.seed(1056)
Sentiment_Wordcloud<-wordcloud(words = Sentiment_Wordcloud$word,freq=Sentiment_Wordcloud$freq,
                               min.freq=,max.words=400,random.order=FALSE,rot.per=0.35,title.size = 1.5,colors=brewer.pal(8,"Dark2"))




#FIND TERM CORRELATION (TO IDENTIFY THE WORDS ASSOCITED TO THE QUESTION MAIN PROBLEM)
(Words_Assocciation<-findAssocs(Sentiment_Doc, "fake", 0.30))
View(Sentiment_Doc)


################################## Polarity and Emotion Detection #######################################

(Dtweet<-read.csv("/Users/bosco/Trump.csv",stringsAsFactors = FALSE, header=TRUE,row.names=NULL,encoding = "UTF-8"))
View(Dtweet)
class(Dtweet)

############################## POLARITY ANALYSIS ##########################################


catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

# Now we will transform all the words in lower case using catch.error function we just created above and with sapply function
Dtweet = sapply(Dtweet, catch.error)

# Also we will remove NAs, if any exists, from bjp_txt (the collected and refined text in analysis)
Dtweet = Dtweet[!is.na(Dtweet)]

# also remove names (column headings) from the text, as we do not want them in the sentiment analysis
names(Dtweet) = NULL


# classify polarity
class_polarity= classify_polarity(Dtweet, algorithm="bayes")
# get polarity best fit
polarity = class_polarity[,4]


# data frame with results polarity
tweet_df = data.frame(text=Dtweet, polarity=polarity,
                      stringsAsFactors=FALSE)

# sort data frame emotion
tweet_df = within(tweet_df,
                  polarity <- factor(polarity, levels=names(sort(table(polarity), decreasing=TRUE))))

# Ploting polarity
ggplot(tweet_df, aes(x=polarity)) +geom_bar(aes(y=..count.., fill=polarity))+xlab("Polarity Categories") + ylab("Polarity Count")+ggtitle("Sentiment Analysis of Students Response Polarity ")
View(tweet_df)

############################## EMOTION ANALYSIS ##########################################

# Clasiify emotion 
class_emotion = classify_emotion(Dtweet, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emotion[,7]

# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# data frame with results emotion
tweet_df = data.frame(text=Dtweet, emotion=emotion,
                      stringsAsFactors=FALSE)

# sort data frame emotion
tweet_df = within(tweet_df,
                  emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# Ploting emotion
ggplot(tweet_df, aes(x=emotion)) +geom_bar(aes(y=..count.., fill=emotion))+xlab("Emotions Categories") + ylab("Emotion Count")+ggtitle("Sentiment Analysis of Students Emotions")

################################# EMOTION AND REASON ###########################################

emos = levels(factor(tweet_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = Dtweet[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, color = pal,
                 scale = c(2,.5), random.order = FALSE, title.size = 1.5)




