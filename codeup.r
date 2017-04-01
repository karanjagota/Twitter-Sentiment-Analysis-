library(tm)
library(wordcloud)
library(RSentiment)

a1<- read.csv("Election_tweets.csv")

# removing moise 
a1$TweetText<- gsub("RT"," ",a1$TweetText)
a1$TweetText<- gsub("#", " " , a1$TweetText)
a1$TweetText<- gsub("@","  ", a1$TweetText)
a1$TweetText<- gsub("[.]"," ",a1$TweetText)
a1$TweetText<- gsub("[_]","  ",a1$tweets)
a1$TweetText<- gsub("[:]", " " , a1$TweetText)
a1$TweetText<- gsub("[!]", " ", a1$TweetText)
a1$TweetText<- gsub("[?]" , " " , a1$TweetText)
a1$TweetText<- gsub("[,]" , " ", a1$TweetText)
a1$TweetText<- gsub('http.*\\s*', ' ', a1$TweetText)

# Inorder to remove unicode you can uncomment below code..  
# a1$TweetText <- sapply(a1$TweetText,function(row) iconv(row, "latin1", "ASCII", sub=" "))


id<- c(1:8356)
b<- unique(a1$TweetText)

df_new<- data.frame(ID= id, Tweets = b)

corpus_ofwords <- Corpus(VectorSource(df_new$Tweets))

corpus_ofwords<- tm_map(corpus_ofwords, tolower)

corpus_ofwords<- tm_map(corpus_ofwords, PlainTextDocument)

document_term_matrix <- DocumentTermMatrix(corpus_ofwords)

# creating Document term matrix .. 
copy_document_term_matrix <- as.matrix(document_term_matrix)

frequency <- colSums(copy_document_term_matrix)

# Calculating sentiment score .. 
sentiments_up = calculate_sentiment(names(frequency))
sentiments_up = cbind(sentiments_up, as.data.frame(frequency))
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]

# making wordcloud .. 
set.seed(415)
wordcloud(words, frequency,max.words = 200,scale = c(10,0.5))

# before making sentiment word cloud with sentiment score one has to remove emoji code from tweets which has been comented above.... 
wordcloud(sent_pos_up$text,sent_pos_up$freq,scale =c(10,0.5))

wordcloud(sent_neg_up$text,sent_neg_up$freq,scale =c(10,0.5) )
