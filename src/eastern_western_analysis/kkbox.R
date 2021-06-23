library(httr)
library(rvest)
library(readr)
library(dplyr)
library(tibble)
library(jiebaR)
library(ggplot2)
library(stringr)
library(tidytext)

# 函數: kkbox 每 1~2 年的圖
draw <- function(path, language){

  # 讀入斷詞
  engine <- worker()

  if (language == "ch"){
    # 將正負情緒詞語讀入
    positive <- unique(read_csv("positive.txt"))
    negative <- unique(read_csv("negative.txt"))
    positive <- data.frame(word = positive, sentiments = "positive")
    negative <- data.frame(word = negative, sentiemtns = "negative")
    colnames(negative) = c("word","sentiment")
    colnames(positive) = c("word","sentiment")
    LIWC_ch <- rbind(positive, negative)
  }
  if (language == "en"){
    # 將正負情緒詞語讀入
    English_dict <- get_sentiments("bing")
  }

  songs <- read_csv(path)
  text <- vector()
  
  for (i in c(1:100)){
    print(i)
    html <- content(GET(songs$連結[i]))
    lyric <- html %>% html_nodes("p.lyrics") %>% html_text()
    test <- unlist(strsplit(lyric, "\n"))
    lyric <- test[!str_detect(test, "編曲|作詞|作曲")]
    lyric <- paste(lyric[lyric != ""], collapse = " ")
    text[i] <- lyric
  }

  # 斷詞
  for (i in c(1:100)){
    print(i)
    seg <- segment(text[i], engine)
    text[i] <- paste0(seg, collapse = "\u3000")
  }
  
  # 做成 dataframe
  docs_df <- tibble::tibble(
    rank = seq_along(songs$歌曲),
    content = text
  )
  
  tidy_text_format <- docs_df %>%
    unnest_tokens(output = "word", input = "content",
                  token = "regex", pattern = "\u3000")
  
  if (language == "ch"){
    # 根據每首歌排名去分析正負向詞語
    emotion_dict <- tidy_text_format %>%
      inner_join(LIWC_ch, by = "word")
  }
  if (language == "en"){
    # 根據每首歌排名去分析正負向詞語
    emotion_dict <- tidy_text_format %>%
      inner_join(English_dict, by = "word")
  }

  return(emotion_dict)
}

song2021 <- draw("./kkbox/song1.csv", "ch")
song2020 <- draw("./kkbox/song2.csv", "ch")
song2019 <- draw("./kkbox/song3.csv", "ch")
song2018 <- draw("./kkbox/song4.csv", "ch")
song_en2021 <- draw("./kkbox/english1.csv", "en")
song_en2020 <- draw("./kkbox/english2.csv", "en")
song_en2019 <- draw("./kkbox/english3.csv", "en")
song_en2018 <- draw("./kkbox/english4.csv", "en")

# kkbox 中文歌
ch_song <- list(song2018, song2019, song2020, song2021)
for (i in seq_along(ch_song)){
  ch_song[[i]]$year <- i + 2017
  ch_song[[i]]$sentiment[ch_song[[i]]$sentiment == "positive"] <- "ch_positive"
  ch_song[[i]]$sentiment[ch_song[[i]]$sentiment == "negative"] <- "ch_negative"
}

# kkbox 英文歌
en_song <- list(song_en2018, song_en2019, song_en2020, song_en2021)
for (i in seq_along(en_song)){
  en_song[[i]]$year <- i + 2017
  en_song[[i]]$sentiment[en_song[[i]]$sentiment == "positive"] <- "en_positive"
  en_song[[i]]$sentiment[en_song[[i]]$sentiment == "negative"] <- "en_negative"
}

# 合併起來
years <- data.frame()
for (i in c(ch_song, en_song)){
  years <- rbind(years, i)
}

# 畫圖
ggplot(data = years) +
  geom_bar(aes(x = year, fill = sentiment),
           position = "dodge") +
  scale_fill_brewer(palette="YlOrRd") +
  ggtitle(label = "kkbox 2018~2021 正負向情緒比較")
