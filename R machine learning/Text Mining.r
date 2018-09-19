getwd()
setwd("/Users/michaelz/Document/Data Science Camp/R machine learning")
sms_raw = read.csv("sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)

sms_raw$type = factor(sms_raw$type)
str(sms_raw)
table(sms_raw$type)
prop.table(table(sms_raw$type))

install.packages("tm")
library(tm)

?VCorpus

getSources()
getReaders()

?VectorSource
sms_corpus = VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
as.character(sms_corpus[[1]])

sms_corpus[1]
sms_corpus[[1]]

lapply(sms_corpus[1:3], as.character)
corpus_clean = tm_map(sms_corpus, content_transformer(tolower)) # all to lower case
corpus_clean = tm_map(corpus_clean, removeNumbers) # remove numbers
corpus_clean = tm_map(corpus_clean, removeWords, stopwords())
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
as.character(corpus_clean[[1]])
?stopwords

sms_dtm = DocumentTermMatrix(corpus_clean)
sms_dtm







sms_corpus_test = corpus_clean[4170:5599]
