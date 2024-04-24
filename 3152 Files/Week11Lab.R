setwd("D:/Desktop/R files")
cname = file.path(".", "Tute_Q3")
#week 11
library(slam)
library(tm)
library(SnowballC)

cname
print(dir(cname))
docs = Corpus(DirSource(cname))
docs[["Doc1.txt"]][["content"]]

#Tokenisation
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))
docs[["Doc1.txt"]][["content"]]

#Filter words
# Remove stop words and white space
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)

# Stem
docs <- tm_map(docs, stemDocument, language = "english")
#Create document term matrix
dtm <- DocumentTermMatrix(docs)
dtm = as.data.frame(as.matrix(dtm))
write.csv(dtm, "dtm.csv")

dtm


#4
setwd("D:/Desktop/R files/3152 Files")
UFO = read.csv("UFOsample.csv", header = FALSE)
UFO = data.frame(doc_id = row.names(UFO), text =UFO[1])
colnames(UFO) = c('doc_id', 'text')
docs = Corpus(DataframeSource(UFO))
docs[["3"]][["content"]]
print(summary(docs))
#Tokenise
# Hyphen to space, ref Williams
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))
#Filter Words
# Remove stop words and white space
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs[["1"]][["content"]]
# Stem
docs <- tm_map(docs, stemDocument, language = "english")
#Create document term matrix
dtm <- DocumentTermMatrix(docs)
docs[["1"]][["content"]]
#Remove sparse terms
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.99999999) # removing sparse
dtms = as.matrix(dtms)
write.csv(dtms, "dtms_UFO.csv")
dtms_ufo = UFO = read.csv("dtms_UFO.csv", header = FALSE)
dtms_ufo
#cluster
distmatrix = dist(scale(dtms))
fit = hclust(distmatrix, method = "ward.D")
plot(fit)
plot(fit, hang = -1)

dtmm = as.matrix(dtm)
dtmm[,5] # term asleep



#5
setwd("D:/Desktop/R files")
#Build corpus on usenet data
cname = file.path(".", "mini_newsgroups_mixedup")
docs = Corpus(DirSource((cname)))
print(summary(docs))
# Specific transformations
# Hyphen to space, ref Williams
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "-")
# cantaloupe to space, ref Williams
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "cantaloupe")
# newsgroup to space, ref Williams
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "newsgroup")

# Tokenise
#inspect(docs[1])
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))
# Remove stop words and white space
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
# Stem
docs <- tm_map(docs, stemDocument, language = "english")
#Create document term matrix
dtm <- DocumentTermMatrix(docs)
dim(dtm)

# Check Word frequencies, ref Williams
freq <- colSums(as.matrix(dtm))
length(freq)
ord = order(freq)
freq[head(ord)]
freq[tail(ord)]

# Frequency of frequencies, ref Williams
head(table(freq), 10)
tail(table(freq), 10)
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.65)
dim(dtms)

inspect(dtm)
findFreqTerms(dtm, lowfreq = 10)
dtms = as.matrix(dtms)
write.csv(dtms, "dtms.csv")

#cluster
distmatrix = dist(scale(dtms))
fit = hclust(distmatrix, method = "ward.D")
cutfit = cutree(fit, k = 20)
plot(fit)
plot(fit, hang = -1)

dtms[cutfit == 1,]


#Week 11
install.packages(c("igraph", "igraphdata"))
library(igraph)
library(igraphdata)
G1 = graph.formula(A-B, B-C, C-D, D-E, E-F, F-A, A-C, A-D, A-E)





