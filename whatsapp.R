library(stringr)
library(NLP)
library(tm)
library(beepr)
library(RColorBrewer)
library(wordcloud)

# Read chat into R
d = readLines("chat.txt")

len = length(d)

# Regular Expression pattern to extract conversation only.
# i.e. remove the names and timestamps appended to the start of each line
r <- ".*?:\\s(.*)"

# Initialize an empty character vector
txtdf = character(0)

# Create a vector with extracted conversation
for(i in 1:len){
  m <- str_match(d[i], r)
  txtdf <- c(txtdf, m[2])
}

# Convert the conversation into a corpus
txt.corpus <- Corpus(VectorSource(txtdf), readerControl = list(language = "en"))

# Simple Processing
txt.corpus <- tm_map(txt.corpus, removePunctuation)
txt.corpus <- tm_map(txt.corpus, content_transformer(tolower))
txt.corpus <- tm_map(txt.corpus, stemDocument)
txt.corpus <- tm_map(txt.corpus, removeNumbers)



# Remove stop words
add_ons <- c("media", "omitted") # additional stop words
txt.corpus <- tm_map(txt.corpus, function(x) removeWords(x, c(stopwords("english"))))

TDM <- TermDocumentMatrix(txt.corpus,control = list(removePunctuation = TRUE,stopwords = TRUE))

# Create Term Document Matrix
txt.tdm <- TermDocumentMatrix(txt.corpus)

# Remove non frequent words
# 40 times is used. Need to play around with this number
freq_terms <- findFreqTerms(txt.tdm, 40)
txt.m <- as.matrix(txt.tdm[freq_terms,])

# Count the frequency of each word
txt.v <- sort(rowSums(txt.m),decreasing=TRUE)
txt.d <- data.frame(word = names(txt.v),freq=txt.v)
table(txt.d$freq)

# Set colour palatte 
col_pal <- brewer.pal(8,"Dark2")

# Create wordcloud and save it as a png file. Dimension used optimised for A4 size printing.
png("word_cloud.png", width=3508, height=2480)
wordcloud(txt.d$word, txt.d$freq, scale=c(5,1),min.freq=2,
          max.words=300, random.order=F, rot.per=.3, colors=col_pal)
dev.off()

# Inform you that the wordcloud is created 
beep(4)
