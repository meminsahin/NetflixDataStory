# Veri Setini Iceri Aktarilmasi 
netflix <- read.csv("C:/Users/mehmet/Desktop/netflix1.csv")
head(netflix)

# Gerekli Kutuphanlerin Yuklenmesi
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rnaturalearth)
library(readr)

# # Veri setine genel bakis
summary(netflix)

# Eksik degerleri kontrol etme
sapply(netflix, function(x) sum(is.na(x)))

# Veri tiplerini kontrol etme
str(netflix)

# Filmler ve TV Sovlari Arasinda Sayisal Karsilastirma
table(netflix$type)

# Yillara Gore Icerik Ekleme Trendleri
netflix$year_added <- format(netflix$date_added, "%Y")
table(netflix$year_added)

# En Populer Turler ve Kategoriler
netflix$listed_in <- as.factor(netflix$listed_in)
top_genres <- sort(table(netflix$listed_in), decreasing = TRUE)
head(top_genres, 10)

# En Produktif Ulkeler ve Yonetmenler
top_countries <- sort(table(netflix$country), decreasing = TRUE)
head(top_countries, 10)
top_directors <- sort(table(netflix$director), decreasing = TRUE)
head(top_directors, 10)

# Derecelendirmelere Gore Dagilim
table(netflix$rating)

# Filmler ve TV Sovlari Arasindaki Oranin Gorsellestirilmesi
ggplot(netflix, aes(x = type)) + 
  geom_bar(fill = "steelblue") +
  ggtitle("Netflix'te Film ve TV Sov Sayisi")

# Yillara Gore Icerik Ekleme Trendlerinin Gorsellestirilmesi
ggplot(netflix, aes(x = year_added)) +
  geom_line(stat = "count", group = 1, color = "darkred") +
  ggtitle("Yillara Gore Netflix'e Eklenen Icerik Sayisi")

# En Populer Turlerin ve Kategorilerin Gorsellestirilmesi
top_genres_df <- as.data.frame(head(top_genres, 10))
ggplot(top_genres_df, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  ggtitle("Netflix'te En Populer 10 Tur")

# En Produktif Ulkelerin ve Yonetmenlerin Gorsellestirilmesi
top_countries_df <- as.data.frame(head(top_countries, 10))
ggplot(top_countries_df, aes(x = reorder(Var1, Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  ggtitle("Netflix'te En Cok Icerik Ureten 10 Ulke")

# Derecelendirmelere Gore Icerik Dagiliminin Gorsellestirilmesi
ggplot(netflix, aes(x = rating)) + 
  geom_bar(fill = "purple") +
  ggtitle("Netflix'teki Iceriklerin Derecelendirme Dagilimi")

#  Hangi yilda daha fazla Film ve TV Sovu yayinlandi
netflix_years <- netflix %>%
filter(release_year >= 2010) %>%
  count(release_year, type) %>%
  arrange(release_year, type)

head(netflix_years)

ggplot(data = netflix_years, aes(x = release_year, y = n, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Hangi yilda daha fazla Film ve TV Sovu Yayinlandi.",
       x = "Yayin Yili",
       y = "Sayi") +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 20, family = "sans"),
        axis.title.x = element_text(size = 14, family = "sans"),
        axis.title.y = element_text(size = 14, family = "sans"),
        axis.text.x = element_text(size = 12, family = "sans", angle = 90, hjust = 1),
        axis.text.y = element_text(size = 12, family = "sans")) +
  scale_fill_manual(breaks = c("Movie", "TV Show"),
                    values = c("navy blue", "light blue"))


# Filmlerin ve TV Sovlarinin Surelerinin Yillar Icinde Artip Artmadigi

# 'duration' sutunundaki sezon sayilarini sayisal bir degere donusturme
netflix <- netflix %>%
  mutate(duration_num = str_extract(duration, "\\d+")) %>%
  mutate(duration_num = as.numeric(duration_num))

# TV sovvlarini filtreleme ve yillara gore ortalama sezon sayisini hesaplama
duration_tv_shows <- netflix %>%
  filter(type == 'TV Show') %>%
  group_by(release_year) %>%
  summarise(average_seasons = mean(duration_num, na.rm = TRUE))

# Grafik cizimi
ggplot(duration_tv_shows, aes(x = release_year, y = average_seasons)) +
  geom_point(color = 'dark blue') +
  labs(title = 'Yillar Icinde TV Sovlarinin Ortalama Sezon Sayisi',
       x = 'Yil',
       y = 'Ortalama Sezon Sayisi') +
  theme(panel.background = element_blank(),
        plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

# Ulke bazinda gosteri sayisini hesaplama
country_counts <- netflix %>% 
  count(country) %>% 
  rename(show_count = n)

# Dunya ulkelerinin cografi verilerini yukleme
world <- ne_countries(scale = "medium", returnclass = "sf")

# Ulke isimlerini eslestirme ve veri birlestirme
world_data <- left_join(world, country_counts, by = c("name" = "country"))

# Haritayi ve veriyi birlestirme ve gorsellestirme
ggplot(data = world_data) +
  geom_sf(aes(fill = show_count)) +
  scale_fill_viridis_c(option = "plasma", na.value = NA, guide = "colorbar", 
                       breaks = c(10, 100, 500, 1000, 2000, 3000),
                       labels = c("10", "100", "500", "1000", "2000", "3000")) +
  labs(title = "Netflix Gosteri Sayilari Dunya Haritasi Uzerinde",
       subtitle = "Her Ulkedeki Gosteri sayisi",
       fill = "Gosteri\nSayisi") +
  theme_minimal()