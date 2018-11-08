

setwd("~/Ryerson Cert/capstone/R code")

install.packages("twitteR")
install.packages("ROAuth")
library("twitteR")
library("ROAuth")
install.packages("wordcloud")
install.packages("tm")
install.packages("xlsx")
# install.packages("XLConnect")
library("wordcloud")
library("tm")
library("xlsx")
# library("XLConnect")
install.packages("rtweet")
library("rtweet")
install.packages("httpuv")
library("httpuv")
install.packages("caret")

library(RTextTools)
library(e1071)
library(dplyr)
library(caret)



### fetching tweets ###


library("ROAuth")
library("wordcloud")
library("tm")

my_consumer_key <- "p75Cz4TobtdXCNdPONZYqHLoj"
my_consumer_secret <- "YUwK98QpqhhhcfPUppixexln3rdtSOVjLDwLmUatZK01wOnmHb"
my_access_token <- '1044791484812283904-Ej3p6fOAZUbTcRhUMdBU9Anx9MdR4w'
my_access_secret <- 'bfzDZJ93QX0smCGEbDA3ixb1IvkFdmNqWYvCrmsKq4YlJ'


#--------------------------using rTweet
create_token(
  app = "Lesley Milley Text Analytics",
  consumer_key = my_consumer_key,
  consumer_secret = my_consumer_secret,access_token = NULL, access_secret = NULL)


search_string <- '@jen_keesmaat OR jenkeesmaat OR johntory OR keesmaat OR #keesmaatformayor OR jen_keesmaat OR #votejohntory OR #keesmaat4mayor OR 
(#topoli OR topoli OR toronto OR toelxn OR #toronto OR #toelxn (#tory OR tory OR jen OR jennifer)) '

no.of.tweets <- 5000
# uncomment line below to collect more tweets
#tweets <- search_tweets(search_string, n=no.of.tweets,  lang="en", type = 'recent')

# import new data
#below run around 5 pm?
tweetsOct19<- search_tweets(search_string, n=no.of.tweets,  lang="en", type = 'recent')
# below run at 2 pm 
tweetsOct21<- search_tweets(search_string, n=no.of.tweets,  lang="en", type = 'recent')
# 6 pm on 22nd
tweetsOct22<- search_tweets(search_string, n=no.of.tweets,  lang="en", type = 'recent')
# at noon on 23
tweetsOct23<- search_tweets(search_string, n=no.of.tweets,  lang="en", type = 'recent')

# dont touch the two below. these are for saving
tweetssave<-tweets
tweetsOct15<-tweets

saveRDS(tweets, file="Oct15tweets.rds")
# tweets_restore <- readRDS("Oct15tweets.rds")
saveRDS(tweetsOct19, file="Oct19tweets.rds")
saveRDS(tweetsOct21, file="Oct21tweets.rds")
saveRDS(tweetsOct22, file="Oct22tweets.rds")
saveRDS(tweetsOct23, file="Oct23tweets.rds")

head(tweets)
class(tweets)
str(tweets)


#-------------------------------------------------------------------------------------------------------------



install.packages("openxlsx", dependencies = TRUE)
library ("openxlsx")

openxlsx::write.xlsx(as.data.frame(tweets), file= "Oct15.xlsx", sheetName="Sheet1", row.names=TRUE,  col.names=TRUE, append=FALSE)

# import the annotated tweets this is the rolled up version

annotatedtweet <- read.xlsx("Oct15 edited data to import sheet only one column.xlsx",  1)
table(annotatedtweet$Class)
summary(annotatedtweet)

# import the annotated tweets this maintians the sentiment for john and jen

annotatedtweetfull <- read.xlsx("Oct15 edited data single column all categories.xlsx",  1)
table(annotatedtweetfull$class)
summary(annotatedtweetfull)


annotatedtweetfull2 <- read.xlsx("Oct15 edited data single column all categories v2.xlsx",  1)
table(annotatedtweetfull2$class)
summary(annotatedtweetfull)

#create limited data frame from df "tweets"



fields_to_include <- read.xlsx("Columns.xlsx",  1)
fields_to_include


fields_to_include_with_quotes<-(fields_to_include[['field']])



tweets_most_limited_extended<-tweets[,fields_to_include_with_quotes]


# attach the new columns

# link to annotatedtweetfull
# tweets_for_modelling <- merge(tweets_most_limited,annotatedtweetfull2,by=c("status_id") )
tweets_for_modelling_extended <- merge(tweets_most_limited_extended,annotatedtweetfull2,by=c("status_id") )

# check

# check<-anti_join(tweets_most_limited,annotatedtweetfull2,by=c("status_id") )
# check_reverse<-anti_join(annotatedtweetfull2,tweets_most_limited,by=c("status_id") )

check<-anti_join(tweets_most_limited_extended,annotatedtweetfull2,by=c("status_id") )
check_reverse<-anti_join(annotatedtweetfull2,tweets_most_limited_extended,by=c("status_id") )

# fix the number that excel has truncated - only do once 
# the section below is required as Excel has truncated one of the numbers

tweets_most_limited_extended[1470,"status_id"]
annotatedtweetfull2[1470,"status_id"]
annotatedtweetfull2[1470,"status_id"]<-tweets_most_limited_extended[1470,"status_id"]

check2<-anti_join(tweets_most_limited,annotatedtweetfull2,by=c("status_id") )
check_reverse2<-anti_join(annotatedtweetfull2,tweets_most_limited,by=c("status_id") )

tweets_for_modelling <- merge(tweets_most_limited,annotatedtweetfull2,by=c("status_id") )

# bring in the sentistrength

sentistrength <- read.xlsx("sentiresults.xlsx",  1)

summary(sentistrength)

tweets_for_modelling_extended_ss <- merge(tweets_for_modelling_extended,sentistrength,by=c("status_id") )

check<-anti_join(tweets_for_modelling_extended,sentistrength,by=c("status_id") )
(missing_id<-as.numeric(check$status_id))

tweets_most_limited_extended[1470,"status_id"]
sentistrength[1470,"status_id"]
sentistrength[1470,"status_id"]<-tweets_most_limited_extended[1470,"status_id"]

check_reverse<-anti_join(sentistrength, tweets_for_modelling_extended,by=c("status_id") )
wrong_id<-as.numeric(check_reverse$status_id)

tweets_for_modelling_extended_ss <- merge(tweets_for_modelling_extended,sentistrength,by=c("status_id") )







#--------------------------------------------------------------------------------------------------------
# # Determining the AFINN sentiment score

library(dplyr)
tweets_for_modelling_tidy <-data.frame(tweets_for_modelling)
# install.packages("stringr")
# 
# 
# library(stringi)
# #install.packages(
#   "https://github.com/gagolews/stringi/raw/master/devel/winbuilder/3.5/stringi_1.2.4.zip",
#   repos = NULL,
# )

# install.packages("https://github.com/gagolews/stringi/blob/master/devel/winbuilder/3.4/stringi_1.2.4.zip?raw=true",type = "win.binary" , repos=NULL)

install.packages("~/R/win-library/3.4/stringi_1.2.4.zip", repos=NULL, type = "win.binary")

library(stringr)
install.packages("tidytext")
library(tidytext)

tweets_book1<-tweets_for_modelling_tidy %>% unnest_tokens(word,text)

afinn<-tweets_book1 %>% inner_join(get_sentiments("afinn")) %>%
  group_by(status_id) %>%
  summarise(afinn_sum =sum(score)) %>%
  mutate(method = "AFINN")

# putting back with the original file

tweets_for_modelling_features <- merge(tweets_for_modelling_extended_ss,afinn,by=c("status_id") ,all.x=TRUE)

# preparing the other features
tweets_for_modelling_features2<-tweets_for_modelling_features

# adding in a flag for a Toronto location
table(tweets_for_modelling_features2$location)
locationwords<-c("Toronto","Etobicoke","Scarborough", "North York", "East York")


# tweets_for_modelling_features2$Toronto<-ifelse(grepl(locationwords,tweets_for_modelling_features2$location,ignore.case=TRUE),TRUE,FALSE)

tweets_for_modelling_features2$Toronto<-ifelse(grepl(paste(locationwords, collapse = "|"),tweets_for_modelling_features2$location,ignore.case=TRUE),TRUE,FALSE)

table(tweets_for_modelling_features2$location[tweets_for_modelling_features2$Toronto==TRUE])




# flagging if the twitter account was made more than a month before the elecion

str(tweets_for_modelling_features2$account_created_at)

recent_date <- as.POSIXct("2018-08-31 23:59:59")

tweets_for_modelling_features2$new_account<-ifelse(tweets_for_modelling_features2$account_created_at>recent_date,TRUE,FALSE)

# Adding in a flag for mentions

tweets_for_modelling_features2$reply_flag<-ifelse(!is.na(tweets_for_modelling_features2$reply_to_user_id) ,TRUE,FALSE)

tweets_for_modelling_features2$mentions_flag<-ifelse( !is.na(tweets_for_modelling_features2$mentions_user_id) ,TRUE,FALSE)

# adding in a flag if a URL is present

tweets_for_modelling_features2$URL_flag<-ifelse( !is.na(tweets_for_modelling_features2$urls_url) ,TRUE,FALSE)

# limiting the set to relevant features

tweets_for_modelling_features_final<-tweets_for_modelling_features2[,c("Toronto","new_account","reply_flag","mentions_flag","URL_flag","afinn_sum","is_quote","is_retweet","favorite_count","media_type","retweet_count","followers_count","friends_count","statuses_count","verified","class","ss_Positive","ss_Negative")]

# Modelling Section with Dataset 1 (Features ------------------------------------------------------------------------)

# Split into Test and Training ensuring both are stratified for the categories


# For Training run 10 x 10 Folds for Naive Bayes and Support Vector Machine. For the Training data use stratified folds



# Do a paired comparison 


# repeat above for the Bag of Words data 

#---------------------------------------------------------------------------------------------------------------
# Following section is preliminary investigation

# Trying the bag of words  model

tweets_as_text <- iconv(tweets_for_modelling$text, 'UTF-8', 'ASCII')


tweets_as_text_corpus <- Corpus(VectorSource(tweets_as_text))

#clean up
tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, content_transformer(tolower)) 

tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, content_transformer(removePunctuation))

tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, content_transformer(removeNumbers))

tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, content_transformer(stripWhitespace))

# tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, PlainTextDocument)



dtm <- DocumentTermMatrix(tweets_as_text_corpus)

inspect(dtm[40:50, 10:15])

# partition the data
train_corpus<-tweets_as_text_corpus[train_ind ]
test_corpus<-tweets_as_text_corpus[-train_ind ]

train_dtm <- dtm[train_ind, ]
test_dtm <- dtm[-train_ind, ]

# feature selection

dim(train_dtm)

fivefreq <- findFreqTerms(train_dtm, 5)
length((fivefreq))


# Use only most frequent words (fivefreq) to build the DTM

train_dtm_over5 <- DocumentTermMatrix(train_corpus, control=list(dictionary = fivefreq))

dim(train_dtm_over5)


test_dtm_over5 <- DocumentTermMatrix(test_corpus, control=list(dictionary = fivefreq))

dim(test_dtm_over5)

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
train_dtm_over5_binary <- apply(train_dtm_over5, 2, convert_count)
test_dtm_over5_binary <- apply(test_dtm_over5, 2, convert_count)


# Train the classifier (naive bayes with laplace )
system.time( classifier <- naiveBayes(train_dtm_over5_binary, train_df$class, laplace = 1) )

# Use the NB classifier we built to make predictions on the test set.
system.time( pred <- predict(classifier, newdata=test_dtm_over5_binary) )

# Create a truth table by tabulating the predicted class labels with the actual class labels 
table("Predictions"= pred,  "Actual" = test_df$class )

# Prepare the confusion matrix
conf.mat <- confusionMatrix(pred, test_df$class)

conf.mat


# Sandbox below for investigating the data **********************************************************************

# afinn below ----------------------------------------------------------



# library(caTools)
# tweets_for_modelling_with_AFINN$spl=sample.split(tweets_for_modelling_with_AFINN,SplitRatio=0.7)

smp_size <- floor(0.75 * nrow(tweets_for_modelling_with_AFINN))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(tweets_for_modelling_with_AFINN)), size = smp_size)

train_df <- tweets_for_modelling_with_AFINN[train_ind, ]
test_df <- tweets_for_modelling_with_AFINN[-train_ind, ]

# model AFINN - compare models ---------------------------------------------------

boxplot(afinn_sum ~ class, data =tweets_for_modelling_with_AFINN)

# the data is skewed positive
hist(tweets_for_modelling_with_AFINN$afinn_sum)

# use a kruskal wallis test to see if there are differences in categories

tweets_for_modelling_with_AFINN$class<-as.factor(tweets_for_modelling_with_AFINN$class)
kruskal.test(tweets_for_modelling_with_AFINN$afinn_sum ~ tweets_for_modelling_with_AFINN$class)

# results above
# 
# Kruskal-Wallis rank sum test
# 
# data:  tweets_for_modelling_with_AFINN$afinn_sum by tweets_for_modelling_with_AFINN$class
# Kruskal-Wallis chi-squared = 835.96, df = 5, p-value < 2.2e-16
# significant 

# 

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

with(tweets_for_modelling_with_AFINN, table(afinn_sum, class)) # not as helpful as the boxplot
with(tweets_for_modelling_with_AFINN, do.call(rbind, tapply(!is.na(afinn_sum), class, function(x) c(M = mean(x), SD = sd(x)))))

tweets_for_modelling_with_AFINN$class2 <- relevel(tweets_for_modelling_with_AFINN$class, ref = "neutral_mixed")
test <- multinom(class2 ~ afinn_sum, data = tweets_for_modelling_with_AFINN)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

## extract the coefficients from the model and exponentiate
exp(coef(test))

# predicted probabilities

head(pp <- fitted(test))


# naive bayes on afinn ------------------------------------------------------------------------------



# naive bayes on the words
install.packages("RTextTools")
library(RTextTools)

mat= create_matrix(tweets_for_modelling_with_AFINN$text, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

mat = as.matrix(mat)

classifier = naiveBayes(mat[1:160,], as.factor(sentiment_all[1:160]))
predicted = predict(classifier, mat[161:180,]); predicted

table(sentiment_test, predicted)
recall_accuracy(sentiment_test, predicted)




# naive bayes on the words -----------------------------------------------------------------------------
install.packages("RTextTools")
library(RTextTools)

mat= create_matrix(tweets_for_modelling_with_AFINN$text, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

mat = as.matrix(mat)

classifier = naiveBayes(mat[1:160,], as.factor(sentiment_all[1:160]))
predicted = predict(classifier, mat[161:180,]); predicted

table(sentiment_test, predicted)
recall_accuracy(sentiment_test, predicted)



#----------------------------------------------------------------------------------------
# cleaning the tweets outside of a corpus

clean_tweet = gsub("&amp", "", unclean_tweet)
clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet = gsub("@\\w+", "", clean_tweet)
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)

clean_tweet = gsub("http\\w+", "", clean_tweet)
clean_tweet4 <- str_replace_all(clean_tweet3, "https://t.co/[a-z,A-Z,0-9]*","")
clean_tweet5 <- str_replace_all(clean_tweet4, "http://t.co/[a-z,A-Z,0-9]*","")

clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 

#get rid of unnecessary spaces
clean_tweet <- str_replace_all(clean_tweet," "," ")
# Get rid of URLs
clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# Take out retweet header, there is only one
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")   




# corpus.clean <- corpus %>%
# tm_map(content_transformer(tolower)) %>% 
# tm_map(removePunctuation) %>%
# tm_map(removeNumbers) %>%
# tm_map(removeWords, stopwords(kind="en")) %>%
# tm_map(stripWhitespace)



# 
# word cloud of all words
#-----------------------------------------------------------------------------------------



tweets_as_text <- iconv(tweets_for_modelling$text, 'UTF-8', 'ASCII')


tweets_as_text_corpus <- Corpus(VectorSource(tweets_as_text))

#clean up
tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, content_transformer(tolower)) 

tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, removePunctuation)
#tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, function(x)removeWords(x,stopwords()))

tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, removeNumbers)
tweets_as_text_corpus <- tm_map(tweets_as_text_corpus, stripWhitespace)


wordcloud(tweets_as_text_corpus)
length(tweets_as_text_corpus)










 
  