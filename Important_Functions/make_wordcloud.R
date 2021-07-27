library(tm)
library(wordcloud)

make_cloud <- function(txt_vector, txt_filters){
  dtm <- TermDocumentMatrix(Corpus(VectorSource(txt_vector)))
  m <- as.matrix(dtm)
  v <- sort(rowSums(m), decreasing = TRUE)
  # Filter out "Ale" b/c it skews the entire cloud
  d <- data.frame(word = names(v), freq = v) %>% filter(word != txt_filters)
  # Make the cloud
  wordcloud(word = d$word, freq = d$freq, min.freq = 2, 
            max.words = 150, random.order = FALSE, rot.per= 0.35,
            colors = brewer.pal(8, "Dark2"))
}

## FUNCTIONALITY
make_cloud(df$native.country, txt_filters = c("united-states", "mexico"))
