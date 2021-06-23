library(httr)
library(rvest)
library(readr)
library(dplyr)
library(tibble)
library(jiebaR)
library(ggplot2)
library(stringr)
library(tidytext)

sing <- function(path, language){

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
  
  for (i in c(1:20)){
    print(i)
    html <- content(GET(songs$連結[i]))
    lyric <- html %>% html_nodes("p.lyrics") %>% html_text()
    test <- unlist(strsplit(lyric, "\n"))
    lyric <- test[!str_detect(test, "編曲|作詞|作曲")]
    lyric <- paste(lyric[lyric != ""], collapse = " ")
    text[i] <- lyric
  }
  
  
  for (i in c(1:20)){
    print(i)
    seg <- segment(text[i], engine)
    text[i] <- paste0(seg, collapse = "\u3000")
  }
  
  # 做成 dataframe
  docs_df <- tibble::tibble(
    name = songs$歌手,
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

BTS           <- sing("./singer/BTS.csv", "en")
Drake         <- sing("./singer/Drake.csv", "en")
J_Cole        <- sing("./singer/J Cole.csv", "en")
J_Sheon       <- sing("./singer/J Sheon.csv", "en")
Dua_Lipa      <- sing("./singer/Dua Lipa.csv", "en")
Luke_Combs    <- sing("./singer/Luke Combs.csv", "en")
The_weekend   <- sing("./singer/The Weeknd.csv", "en")
Taylor_Swift  <- sing("./singer/Taylor Swift.csv", "en")
Ariana_Grande <- sing("./singer/Ariana Grande.csv", "en")
Justin_Bieber <- sing("./singer/Justin Bieber.csv", "en")
Billie_Eilish <- sing("./singer/Billie Eilish.csv", "en")

熱狗          <- sing("./singer/MC HotDog.csv", "ch")
八三么        <- sing("./singer/83.csv", "ch")
張惠妹        <- sing("./singer/aMEI.csv", "ch")
陳奕迅        <- sing("./singer/Eason.csv", "ch")
梁靜茹        <- sing("./singer/Fish Leong.csv", "ch")
告五人        <- sing("./singer/five.csv", "ch")
鄧紫棋        <- sing("./singer/GEM.csv", "ch")
蕭敬騰        <- sing("./singer/Jam Hsiao.csv", "ch")
周杰倫        <- sing("./singer/Jay.csv", "ch")
林俊傑        <- sing("./singer/JJ.csv", "ch")
蔡依林        <- sing("./singer/Jolin.csv", "ch")
五月天        <- sing("./singer/May.csv", "ch")

en_singers <- data.frame()
for (i in list(BTS, Drake, J_Cole, J_Sheon, Dua_Lipa, Luke_Combs,
               The_weekend, Taylor_Swift, Ariana_Grande,
               Justin_Bieber, Billie_Eilish)){
  en_singers <- rbind(en_singers, i)
}

ggplot(data = en_singers) +
  geom_bar(aes(x = name, fill = sentiment),
           position = "dodge") +
  ggtitle(label = "Billboard Artist 100") +
  coord_flip()

ch_singers <- data.frame()
for (i in list(熱狗, 八三么, 張惠妹, 陳奕迅, 梁靜茹,
               告五人, 鄧紫棋, 蕭敬騰, 周杰倫, 林俊傑,
               蔡依林, 五月天)){
  ch_singers <- rbind(ch_singers, i)
}

ggplot(data = ch_singers) +
  geom_bar(aes(x = name, fill = sentiment),
           position = "dodge") +
  ggtitle(label = "華語樂壇知名歌手") +
  coord_flip()
