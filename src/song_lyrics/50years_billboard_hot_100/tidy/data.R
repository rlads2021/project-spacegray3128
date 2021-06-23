library(httr)
library(rvest)
library(stringr)


#用gather轉換
df_artists %>%
    mutate(Rank = 1:100) %>%
    gather("Year", "Artist", 1:50) -> df_Artist

df_titles %>%
    mutate(Rank = 1:100) %>%
    gather("Year", "Title", 1:50) -> df_Title

df_lyrics %>%
    mutate(Rank = 1:100) %>%
    gather("Year", "Lyrics", 1:50) -> df_Lyrics



#最終的Dataframe
df_All <- df_Title %>%
    mutate(Artist = df_Artist$Artist) #,
#Lyrics = df_Lyrics$Lyrics)
