pw <- {
  "pass"
}
setwd("/Users/ashimdatta/Enterprise/notifications")
getwd()
library("DBI")
library("RPostgreSQL")
library("sqldf")
library("ggplot2")
library("gridExtra")
library("plyr")
library("ngram")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library("tau")
library("qdap")
library("ROAuth")
require("RCurl")
library("stringr")

library("ggmap")
library("dplyr")
library("plyr")
library("RWeka")
library("RNetLogo")


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "databasename",
                 host = "server", port = 1111,
                 user = "user", password = pw)
text_content<-dbGetQuery(con,"select userid, (created at time zone 'UTC') AT TIME ZONE 'PDT' as created, checkinid, commentid, comments as text from public.ratings_usercheckincomments
      where   created > now() - interval '24 hour'  
                         and char_length(comments)<=250            
                         union all
                         
                         select b.userid, (a.created at time zone 'UTC') AT TIME ZONE 'PDT' as created, a.checkinid, 0 as commentid, a.notes as text from public.ratings_usercheckinnotes a
                         
                         join
                         ratings_usercheckins c
                         on a.checkinid=c.checkinid
                         join authdb_is_users b
                         on c.userid=b.useriD
                         where   a.created > now() - interval '24 hour'
                         and char_length(a.notes)<=250
                         ")
# Create corpus
corpus=Corpus(VectorSource(text_content$text))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

## text cleaning
text=text_content$text
text_list=lapply(text, function(x) iconv(x, "latin1", "ASCII", sub=""))
text_list=lapply(text, function(x) gsub("htt.*",' ',x))
text=unlist(text)
text_content$text=text

## positive negative 
positives= readLines("positive-words.txt")
negatives = readLines("negative-words.txt")

positives2=readLines("positive_2gram.txt")
negatives2 = readLines("negative_2gram.txt")

positives3=readLines("positive_emot.txt")
negatives3 = readLines("negative_emot.txt")


## sentiment scores- Word Match
sentiment_scores = function(text, positive_words, negative_words, .progress='none'){
  scores = laply(text,
                 function(text, positive_words, negative_words){
                   text = gsub("[[:punct:]]", "", text)    # remove punctuation
                   text = gsub("[[:cntrl:]]", "", text)   # remove control characters
                   text = gsub('[[:digit:]]+', '', text)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   text = sapply(text, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(text, "\\s+")
                   words = unlist(word_list)
                   # compare words to the dictionaries of positive & negative terms
                   positive.matches = match(words, positives)
                   negative.matches = match(words, negatives)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive.matches = !is.na(positive.matches)
                   negative.matches = !is.na(negative.matches)
                   # final score
                   score = sum(positive.matches) - sum(negative.matches)
                   return(score)
                 }, positive.matches, negative.matches, .progress=.progress )
  return(scores)
}

## sentiment scores- Phrase match
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2,max=2))

sentiment_scores2 = function(text, positive_2gram, negative_2gram, .progress='none'){
  scores = laply(text,
                 function(text, positive_2gram, negative_2gram){
                   text = gsub("[[:punct:]]", "", text)    # remove punctuation
                   text = gsub("[[:cntrl:]]", "", text)   # remove control characters
                   text = gsub('[[:digit:]]+', '', text)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   text = sapply(text, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word.list2 = BigramTokenizer(text)
                   words2 = unlist(word.list2)
                   # compare words to the dictionaries of positive & negative terms
                   positive.matches = match(words2, positives2)
                   negative.matches = match(words2, negatives2)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive.matches = !is.na(positive.matches)
                   negative.matches = !is.na(negative.matches)
                   # final score
                   score = sum(positive.matches) - sum(negative.matches)
                   return(score)
                 }
                 , positive.matches, negative.matches)
                 .progress=.progress 
  return(scores)
}

## sentiment scores- Emoji match
sentiment_scores3 = function(text, positives3, negatives3, .progress='none'){
  scores = laply(text,
                 function(text, positives3, negatives3){
                   #text = gsub("[[:punct:]]", "", text)    # remove punctuation
                   #text = gsub("[[:cntrl:]]", "", text)   # remove control characters
                   #text = gsub('[[:digit:]]+', '', text)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   text = sapply(text, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   #word_list3 = str_split(text, "\\s+")
                   word_list3 <- do.call(c,strsplit(text," "))
                   words3 = unlist(word_list3)
                   # compare words to the dictionaries of positive & negative terms
                                                               #grep(postive_emot,words3)
                   positive.matches = match(words3, positives3)
                   negative.matches = match(words3, negatives3)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive.matches = !is.na(positive.matches)
                   negative.matches = !is.na(negative.matches)
                   # final score
                   score = sum(positive.matches) - sum(negative.matches)
                   return(score)
                 }, positive.matches, negative.matches, .progress=.progress )
  return(scores)
}


## sentiment scores- Word Match- emoji
sentiment_scores3 = function(text, positive_words, negative_words, .progress='none'){
  scores = laply(text,
                 function(text, positive_words, negative_words){
                   text = gsub("[[:punct:]]", "", text)    # remove punctuation
                   text = gsub("[[:cntrl:]]", "", text)   # remove control characters
                   text = gsub('[[:digit:]]+', '', text)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   text = sapply(text, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(text, "\\s+")
                   words = unlist(word_list)
                   # compare words to the dictionaries of positive & negative terms
                   positive.matches = match(words, positives3)
                   negative.matches = match(words, negative_words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive.matches = !is.na(positive.matches)
                   negative.matches = !is.na(negative.matches)
                   # final score
                   score = sum(positive.matches) - sum(negative.matches)
                   return(score)
                 }, positive.matches, negative.matches, .progress=.progress )
  return(scores)
}


score1 = sentiment_scores(text, positives, negatives, .progress='text')

score2 = sentiment_scores2(text, positives2, negatives2, .progress='text')

score3 = sentiment_scores3(text, positives3, negatives3, .progress='text')


text_content$score1=score1
text_content$score2=score2

write.csv(text_content,"text_test.csv")

header<-cbind("Pulse ", as.character(Sys.Date()))

hist(score,xlab=" ",main=header,
     border="black",col="skyblue")



ggplot(text_content, aes(x=created,y=score,color=score)) + 
  geom_line() +
  scale_color_gradient2(midpoint=0, low="red", mid="white",
                        high="green", space ="Lab" )+
  theme(panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(colour = "grey40"),
        panel.grid.minor = element_blank())+
  theme(axis.text.x = element_text(hjust = .3, size = 8),
        axis.title=element_text(size=8))+
  xlab("Time of the post in Pacific time") + ylab("Pulse") +
  ggtitle(header)

filename<-paste("pulse_on_",as.character(today),".csv")
write.csv(text_content, filename)



