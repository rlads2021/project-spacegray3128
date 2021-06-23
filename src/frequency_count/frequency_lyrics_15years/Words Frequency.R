#library 會出現 error 的 packages
library(tm)
library(tidyr)
library(textclean)
library(qdap)
library(tidytext)
library(textdata)
library(devtools)

##############
library(wordcloud2)
library(wordcloud)

#前期處理
songs_source <- DataframeSource(song_data)
songs_corpus <- VCorpus(songs_source)

songs_corpus <- songs_corpus %>% 
    tm_map(removePunctuation) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, c(stopwords("en"),"dont",
                          "can","just", "cant", "get", "got")) %>%
    tm_map(content_transformer(replace_number)) %>%
    tm_map(content_transformer(bracketX)) %>%
    tm_map(stripWhitespace) 


#analyze lyrics on music 

#Stem document
clean_corpus <- tm_map(songs_corpus, stemDocument)
#Create Term Document Matrix
clean_tdm    <- TermDocumentMatrix(clean_corpus)
#Converting TDM to matrix for Analysis
clean_m      <- as.matrix(clean_tdm)

#create word frequency charts
frequency_words <- freq_terms(
    song_data$text, top = 20, at.least = 3, stopwords = 
        c(stopwords("english"), "dont", "can", "get", "got"))

frequency_words %>%
    ggplot( aes(x=WORD, y=FREQ)) +
    geom_segment( aes(x= reorder(WORD,desc(FREQ)), xend=WORD, y=0, yend=FREQ)
                  ,color="skyblue", size=1) +
    geom_point(color="blue", size=4, alpha=0.6) +
    theme_minimal()+
    theme(text = element_text(size = 10),
          plot.title = element_text(size = 16,color = "#ff5a5f", face = "bold"
                                    ,margin = margin(b = 7)),
          plot.subtitle = element_text(size = 10, color = "darkslategrey"
                                       , margin = margin(b = 7))) +
    labs(x = "歌詞用字", y = "頻率", title = "2006年-2020年前百大金曲常用詞統計")