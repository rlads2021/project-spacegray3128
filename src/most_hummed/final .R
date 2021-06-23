library(ggplot2)
library(httr)
library(magrittr)
library(dplyr)
library(stringr)
library(tidytext)
library(rvest)
#擷取2020 google統計前一百手被哼唱歌曲
A <- GET("https://www.billboard.com/",
         path ="charts/year-end/top-hummed"
)
html<-content(A)
rank <-html%>%
  html_nodes(".chart-element__rank__number")%>%
  html_text()
name <-html%>%
  html_nodes(".color--primary")%>%
  html_text()

artist <-html%>%
  html_nodes(".color--secondary")%>%
  html_text()
artist2 <-as.list(artist)
Artist <-artist[artist!=("\n                                                    ")]
#製表
data <-tibble(Ranking=rank,
              Song=name,
              artist=Artist)
#製作詞頻表
data2 <-data%>%
  unnest_tokens(word,Song)%>%
  anti_join(stop_words)%>%
  count(word)%>%
  filter(n>1)%>%
  arrange(desc(n))
#製圖
ggplot(data2)+
  geom_bar(aes(word,n),
           stat="identity")+
  labs(title = "The word appeared most in the title of
                 Google's top 100 hummed songs")
#調整資料
name[9]  <-"Bang"
name[10] <-"Sing About Me I m Dying of Thirst"
name[17] <-"Savage Love"
name[26] <-"Let Me In"
name[30] <-"Sunflower"
name[35] <-"I Wanna Dance With Somebody"
name[60] <-"Someone Gets Hurt"
name[91] <-"Kion's Lament"
name[80] <-"Wait"
name[82] <- "Simon Says"
name[83] <-"Life s BeenGood"
name[85] <- "Blue"
name[86] <- "Rockin Around The Christmas Tree"
name[91] <- "Kion Lament"
name[100] <- "You Can t Always Get What You Want"


#製作單字表
library(readr)
axcgx_lgzbg <- read_csv("~/Downloads/axcgx-lgzbg.csv")
level <-axcgx_lgzbg
as.list(level)
level <-level[is.na(level)==F]
level <-level[-c(1:10)] 

vocabulary_table <-tibble(vocabulary=level)
i_DONT_NEED<-grep("\\d{2,3}",vocabulary_table$vocabulary)
#把頁數資料去除
vocabulary_table2 <-vocabulary_table[-i_DONT_NEED,]
vocabulary_table3 <-vocabulary_table2%>%
  unnest_tokens(word,vocabulary)
#斷詞
i_DONT_NEED2<-grep("\\d{1,2}",vocabulary_table3$word)
#把型如(1,2)的項目拿掉
vocabulary_table4<-vocabulary_table3[-i_DONT_NEED2,]
vocabulary_table5<-vocabulary_table4%>%
  filter(word!=("s"),word!=("ment"))
#去除一些字尾
#以下分類單字
grep("zoo",vocabulary_table5$word)
level1 <-vocabulary_table5$word[1:1119]
grep("zebra",vocabulary_table5$word)
level2 <-vocabulary_table5$word[1120:2221]
grep("zone",vocabulary_table5$word)
level3 <-vocabulary_table5$word[2222:3302]
grep("youthful", vocabulary_table5$word)
level4 <-vocabulary_table5$word[3303:4467]
grep("zoom", vocabulary_table5$word)
level5 <-vocabulary_table5$word[4468:5552]
grep("zeal", vocabulary_table5$word)
level6 <-vocabulary_table5$word[5553:6679]
letter <-tolower(data$Song)

#用來分類的函數
group <-function(x){
  if(x%in%level1)return(1)
  else if(x%in%level2)return(2)
  else if(x%in%level3)return(3)
  else if(x%in%level4)return(4)
  else if(x%in%level5)return(5)
  else if(x%in%level6)return(6)
  else return(7)
}
str_letter <- strsplit(letter, " ")
grouping<-list()
grouping2 <-list()
mean_grouping2 <-list()
for (i in 1:100) {
  for(j in seq_along(str_letter[[i]])){
    grouping[[i]]<- str_letter[[i]]
    grouping[[i]][j] <- str_letter[[i]][j]
  }
}
for (i in seq_along(grouping)) {
  for (j in seq_along(grouping[[i]])) {
    grouping2[[i]] <-group(grouping[[i]])
    grouping2[[i]][j]<- group(grouping[[i]][j])
    mean_grouping2[i] <- mean(grouping2[[i]])#以平均值做數據
  }
}
for (i in seq_along(grouping)) {
  for (j in seq_along(grouping[[i]])) {
    
    grouping2[[i]][j]<- group(grouping[[i]][j])
    mean_grouping2[i] <- mean(grouping2[[i]])#以平均值做數據
  }
}
final_data <-data%>%
  mutate(level=mean_grouping2)
final_data1 <-final_data%>%
  group_by(level)%>%
  summarise(n=n())%>%
  arrange(desc(n))
final_data1$level <-unlist(final_data1$level )  
ggplot(final_data1)+
  geom_bar(aes(x = level, y = n),
           stat = "identity")+
  labs(title="歌名詞彙使用等級")
  