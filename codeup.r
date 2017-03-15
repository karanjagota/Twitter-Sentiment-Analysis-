library(tm)
library(wordcloud)

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

# making wordcloud .. 
set.seed(415)
wordcloud(words, frequency,max.words = 200,scale = c(10,0.5))


