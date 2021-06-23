
import lyricsgenius
import pandas as pd
import csv
import pandas as pd
from bs4 import BeautifulSoup
import time
import requests
import json
import re
import csv
import string
import random

token = "TTZReCNV_4kBUIEwI2P350OLnvckm68yKdCGgdOVOJaJa4ABud3BCMQVf_nGoKVE"
genius = lyricsgenius.Genius(token)
chart = pd.read_csv("/Users/spacegray0315/Desktop/R&Data_science/billboard/All.csv")
lyrics = []

# 一次爬一千筆歌詞（一個世代每年百大）
for i in range(800, 1800):
    print(i)
    artist_name = chart["Artist"][i]
    song_title = chart["Title"][i]
    try:
        song = genius.search_song(song_title, artist = artist_name)
        lyric = song.lyrics
    except:
        song = "null"
        lyric = "null"
    print(lyric)
    lyrics.append(lyric)
    time.sleep(random.uniform(1, 5))


print(len(lyrics))
# 存到csv檔
fn2 = "all_songs_1980s.csv"
with open(fn2, "w", encoding = "utf-8") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["Position", "year", "Artist", "Title", "Lyrics"])
    
    for i in range(800, 1800):
        writer.writerow([(i-800), str(int(1972+(i / 100))), chart["Artist"][i], chart["Title"][i], lyrics[i-800]])



