
library(dplyr)
library(stringdist)
library(fuzzyjoin)
library(sf)
library(tidyverse)
library(tidyr)
library(jsonlite)
library(tidytext)
library(textdata)

# LOAD DATA
setwd("/Users/carinaclewley/Documents/dissertation/data")
Sys.setenv(SHAPE_RESTORE_SHX = "YES")

# internal politics
voting_data <- read_csv("votes.csv")
contestants_data <- read_csv("contestants.csv")
contestants_data <- read_csv("contestants.csv")
contestants_data <- contestants_data %>%
  mutate(to_country = recode(to_country, "North MacedoniaN.Macedonia" = "North Macedonia"),
         to_country = recode(to_country, "Czechia" = "Czech Republic"))
contestants_data <- contestants_data %>%
  distinct(year, to_country, song, performer, .keep_all = TRUE)
participation <- contestants_data %>%
  group_by(to_country) %>%
  summarise(
    years_participated = n_distinct(year),
    first_year = min(year),
    last_year = max(year),
    .groups = "drop"
  ) %>%
  mutate(
    total_possible = last_year - first_year + 1,
    participation_rate = years_participated / total_possible
  ) %>%
  arrange(desc(participation_rate))


# musical
music_data <- read_csv("every eurovision song ever.csv")
more_music_data <-read_csv("merged_table.csv")
even_more_music_data <- read_csv("Eurovision songs.csv")
lyrics_json <- fromJSON("https://storage.googleapis.com/kagglesdsdata/datasets/883302/11631473/eurovision-lyrics-2025.json?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20260313%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20260313T102336Z&X-Goog-Expires=259200&X-Goog-SignedHeaders=host&X-Goog-Signature=21dd44b16b469d80fd9dfa35d1ff346a5a6e03979fb1f77db7fa4b3f4f9992a8e2748dffc7365f168acc2ed4a7a665dacc9f4a7c54211c31f511c36bb8e77f2f06fd2c82b45142877c228b59e9f0acd15ba0791a41bcb4dd9bcd3612d745836c642923c6cdf335503816060c3e66811e670a4747dc12abf9c6e1e3bce1b43ac573da9120ad38f0e03f7dd912dc8f52ec7f10f1916b415e49b8fc3cd44ef00542f5616ac6ec02a0ee0af13e10bfce37b6e34f24406a5561571f736005c2b058e50081f031bd1871c8b734afa52a7ec366a96bf92c52f56bdb0c33916612799826698a66ebf1144e46d0e7f7d295dcc984087cdcd255898170e07bf738439f5f43", simplifyDataFrame = TRUE, flatten = TRUE)

# CLEAN MUSIC DATA

# normalize and sort word function
normalize <- function(x) {
  x %>%
    tolower() %>%
    gsub("[[:punct:]]", "", .) %>%
    gsub("\\beurovision\\b", "", .) %>%
    gsub("\\b(19|20)\\d{2}\\b", "", .) %>%   # only 19xx or 20xx
    gsub("\\s+", " ", .) %>%
    trimws()
}

sort_words <- function(x) {
  sapply(strsplit(x, " "), function(words) paste(sort(words), collapse = " "))
}

# clean and combine music datasets
music_data <- music_data %>%
  mutate(
    `Album Date` = as.Date(`Album Date`),
    song_key = sort_words(normalize(Song)),
    artist_key = sort_words(normalize(Artist)),
    danceability = Dance,
    energy = Energy,
    valence = Valence,
    tempo = BPM,
    acousticness = Acoustic,
    instrumentalness = Instrumental,
    speechiness = Speech,
    liveness = Live,
    loudness = `Loud (Db)`,
    key = as.character(Key)  # Convert to character
  )

more_music_data <- more_music_data %>%
  mutate(
    song_key = sort_words(normalize(Song)),
    artist_key = sort_words(normalize(Contestant)),
    key = as.character(key),
    danceability = danceability * 100,
    energy = energy * 100,
    valence = valence * 100,
    acousticness = acousticness * 100,
    instrumentalness = instrumentalness * 100,
    speechiness = speechiness * 100,
    liveness = liveness * 100
  )

even_more_music_data <- even_more_music_data %>%
  mutate(
    `Album Date` = as.Date(`Album Date`),
    song_key = sort_words(normalize(Song)),
    artist_key = sort_words(normalize(Artist)),
    danceability = Dance,
    energy = Energy,
    valence = Valence,
    tempo = BPM,
    acousticness = Acoustic,
    instrumentalness = Instrumental,
    speechiness = Speech,
    liveness = Live,
    loudness = `Loud (Db)`,
    key = as.character(Key) 
  )

# combine
all_music <- bind_rows(
  music_data %>% select(song_key, artist_key, `Album Date`, danceability, energy, 
                        valence, tempo, acousticness, instrumentalness, 
                        speechiness, liveness, loudness, key),
  more_music_data %>% select(song_key, artist_key, danceability, energy, 
                             valence, tempo, acousticness, instrumentalness, 
                             speechiness, liveness, loudness, key),
  even_more_music_data %>% select(song_key, artist_key, `Album Date`, danceability, energy, 
                                  valence, tempo, acousticness, instrumentalness, 
                                  speechiness, liveness, loudness, key)
) %>%
  group_by(song_key, artist_key) %>%
  arrange(`Album Date`) %>%
  slice(1) %>%
  ungroup()

# normalized keys to contestants
music_data_clean <- contestants_data %>%
  mutate(
    song_key = sort_words(normalize(song)),
    artist_key = sort_words(normalize(performer))
  ) %>%
  select(song_key, artist_key, year, lyrics, to_country)

all_music <- all_music %>%
  filter(
    !is.na(danceability), !is.na(energy)
  )

# fuzzy join
artist_matched <- stringdist_join(
  music_data_clean,
  all_music,
  by = "artist_key",
  method = "jw",
  max_dist = 0.5,
  mode = "inner",
  distance_col = "artist_dist"
)

matched <- artist_matched %>%
  mutate(song_dist = stringdist(song_key.x, song_key.y, method = "jw")) %>%
  filter(                             # normal case: both reasonably close
    (song_dist < 0.1 & artist_dist <= 0.5) |         # exact song, lenient artist
      (artist_dist < 0.1 & song_dist <= 0.5)            # exact artist, lenient song
  ) %>%
  group_by(song_key.x, artist_key.x) %>%
  arrange(song_dist + artist_dist) %>%               # rank by combined distance
  slice(1) %>%
  ungroup() %>%
  transmute(
    song_key = song_key.x,
    artist_key = artist_key.x,
    matched_song_key = song_key.y,
    matched_artist_key = artist_key.y,
    `Album Date`, danceability, energy, valence, tempo,
    acousticness, instrumentalness, speechiness, liveness, loudness, key
  )

music_data_clean <- music_data_clean %>%
  left_join(matched, by = c("song_key", "artist_key"))

# add music_available flag
music_data_clean <- music_data_clean %>%
  mutate(music_available = !is.na(danceability))

# check coverage
sum(!is.na(music_data_clean$danceability)) / nrow(music_data_clean) * 100

# coverage by year
coverage_by_year <- music_data_clean %>%
  group_by(year) %>%
  summarise(
    total = n(),
    with_music = sum(!is.na(danceability)),
    coverage_pct = (with_music / total) * 100
  )

# plot
ggplot(coverage_by_year, aes(x = year, y = coverage_pct)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Music Feature Coverage Over Time",
       x = "Year",
       y = "Coverage (%)") +
  theme_minimal() +
  ylim(0, 100)

# add languages

music_data_clean <- music_data_clean %>%
  mutate(lyrics = str_replace_all(lyrics, "\\\\n|\n", " "))

# english translations
lyrics_df <- bind_rows(lyrics_json)

lyrics_df <- lyrics_df %>%
  mutate(
    song_key = sort_words(normalize(Song)),
  )

lyrics_df <- lyrics_df %>%
  select(song_key, Year, `Lyrics translation`, Language)

lyrics_matched <- stringdist_join(
  music_data_clean,
  lyrics_df,
  by = "song_key",
  method = "jw",
  max_dist = 0.4,
  mode = "left",
  distance_col = "song_dist"
) %>%
  filter(year == Year | is.na(Year)) %>%
  group_by(song_key.x, year) %>%
  arrange(song_dist) %>%
  slice(1) %>%
  ungroup() %>%
  select(song_key.x, year, `Lyrics translation`, Language) %>%
  rename(
    song_key = song_key.x,
    lyrics_translation = `Lyrics translation`,
    language = Language
  )

# join back to original
music_data_clean <- music_data_clean %>%
  left_join(lyrics_matched, by = c("song_key", "year"))

music_data_clean <- music_data_clean %>%
  mutate(
    language = case_when(
      artist_key == "dschinghis khan" & year == 1979 ~ "German",
      artist_key == "copycat" & year == 2009 ~ "English",
      artist_key == "city groove love" & year == 1995 ~ "English",
      TRUE ~ language
    ),
    lyrics_translation = case_when(
      artist_key == "dschinghis khan" & year == 1979 ~ lyrics_df %>% filter(song_key == "dschinghis khan", Year == "1979") %>% pull(`Lyrics translation`) %>% first(),
      artist_key == "copycat" & year == 2009 ~ "English",
      artist_key == "city groove love" & year == 1995 ~ "English",
      TRUE ~ lyrics_translation
    )
  )


nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

lyrics_to_analyse <- music_data_clean %>%
  mutate(
    text_to_analyse = case_when(
      lyrics_translation == "English" | is.na(lyrics_translation) ~ lyrics,
      TRUE ~ lyrics_translation
    )
  ) %>%
  filter(!is.na(text_to_analyse), nchar(text_to_analyse) > 10) %>%
  select(song_key, artist_key, year, text_to_analyse)

lyrics_tokens <- lyrics_to_analyse %>%
  unnest_tokens(word, text_to_analyse) %>%
  anti_join(stop_words, by = "word")

sentiment_scores <- lyrics_tokens %>%
  left_join(bing, by = "word") %>%
  filter(!is.na(sentiment)) %>%
  count(song_key, year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)

emotion_scores <- lyrics_tokens %>%
  left_join(nrc, by = "word") %>%
  filter(!is.na(sentiment), !sentiment %in% c("positive", "negative")) %>%
  count(song_key, year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

# add total word count per song
word_counts <- lyrics_tokens %>%
  count(song_key, year, name = "total_words")

music_data_clean <- music_data_clean %>%
  left_join(word_counts, by = c("song_key", "year"))

theme_scores <- emotion_scores %>%
  left_join(word_counts, by = c("song_key", "year")) %>%
  pivot_longer(cols = -c(song_key, year, total_words),
               names_to = "emotion", values_to = "n") %>%
  mutate(pct = n / total_words * 100) %>%
  group_by(song_key, year) %>%
  slice_max(pct, n = 1, with_ties = FALSE) %>%
  rename(dominant_theme = emotion) %>%
  select(song_key, year, dominant_theme)

music_data_clean <- music_data_clean %>%
  left_join(sentiment_scores, by = c("song_key", "year")) %>%
  left_join(emotion_scores, by = c("song_key", "year")) %>%
  left_join(theme_scores, by = c("song_key", "year"))

arc_scores <- lyrics_to_analyse %>%
  unnest_tokens(word, text_to_analyse) %>%
  left_join(bing, by = "word") %>%
  filter(!is.na(sentiment)) %>%
  group_by(song_key, year) %>%
  summarise(
    h1 = sum(sentiment[ntile(row_number(), 2) == 1] == "positive") - 
      sum(sentiment[ntile(row_number(), 2) == 1] == "negative"),
    h2 = sum(sentiment[ntile(row_number(), 2) == 2] == "positive") - 
      sum(sentiment[ntile(row_number(), 2) == 2] == "negative"),
    t1 = sum(sentiment[ntile(row_number(), 3) == 1] == "positive") - 
      sum(sentiment[ntile(row_number(), 3) == 1] == "negative"),
    t2 = sum(sentiment[ntile(row_number(), 3) == 2] == "positive") - 
      sum(sentiment[ntile(row_number(), 3) == 2] == "negative"),
    t3 = sum(sentiment[ntile(row_number(), 3) == 3] == "positive") - 
      sum(sentiment[ntile(row_number(), 3) == 3] == "negative"),
    q1 = sum(sentiment[ntile(row_number(), 4) == 1] == "positive") - 
      sum(sentiment[ntile(row_number(), 4) == 1] == "negative"),
    q2 = sum(sentiment[ntile(row_number(), 4) == 2] == "positive") - 
      sum(sentiment[ntile(row_number(), 4) == 2] == "negative"),
    q3 = sum(sentiment[ntile(row_number(), 4) == 3] == "positive") - 
      sum(sentiment[ntile(row_number(), 4) == 3] == "negative"),
    q4 = sum(sentiment[ntile(row_number(), 4) == 4] == "positive") - 
      sum(sentiment[ntile(row_number(), 4) == 4] == "negative"),
    .groups = "drop"
  ) %>%
  mutate(
    # fit score = sum of absolute differences between adjacent sections
    # higher = stronger signal for that arc shape
    score_rise        = h2 - h1,
    score_fall        = h1 - h2,
    score_rise_fall   = (t2 - t1) + (t2 - t3),
    score_fall_rise   = (t1 - t2) + (t3 - t2),
    score_rise_fall_rise = (q2 - q1) + (q2 - q3) + (q4 - q3),
    score_fall_rise_fall = (q1 - q2) + (q3 - q2) + (q3 - q4),
  ) %>%
  rowwise() %>%
  mutate(
    # pick whichever arc has the highest score
    arc = case_when(
      max(score_rise, score_fall, score_rise_fall, score_fall_rise,
          score_rise_fall_rise, score_fall_rise_fall) <= 0 ~ "flat",
      which.max(c(score_rise, score_fall, score_rise_fall, score_fall_rise,
                  score_rise_fall_rise, score_fall_rise_fall)) == 1 ~ "rise",
      which.max(c(score_rise, score_fall, score_rise_fall, score_fall_rise,
                  score_rise_fall_rise, score_fall_rise_fall)) == 2 ~ "fall",
      which.max(c(score_rise, score_fall, score_rise_fall, score_fall_rise,
                  score_rise_fall_rise, score_fall_rise_fall)) == 3 ~ "rise-fall",
      which.max(c(score_rise, score_fall, score_rise_fall, score_fall_rise,
                  score_rise_fall_rise, score_fall_rise_fall)) == 4 ~ "fall-rise",
      which.max(c(score_rise, score_fall, score_rise_fall, score_fall_rise,
                  score_rise_fall_rise, score_fall_rise_fall)) == 5 ~ "rise-fall-rise",
      which.max(c(score_rise, score_fall, score_rise_fall, score_fall_rise,
                  score_rise_fall_rise, score_fall_rise_fall)) == 6 ~ "fall-rise-fall"
    ),
    ending = case_when(
      arc %in% c("rise", "fall-rise", "rise-fall-rise") ~ "happy ending",
      arc %in% c("fall", "rise-fall", "fall-rise-fall")  ~ "sad ending",
      TRUE ~ "neutral"
    )
  ) %>%
  ungroup() %>%
  select(song_key, year, arc, ending, score_rise, score_fall, score_rise_fall, score_fall_rise, score_rise_fall_rise, score_fall_rise_fall, h1, h2, t1, t2, t3, q1, q2, q3, q4)

arc_scores <- arc_scores %>%
  mutate(
    # get the winning score
    winning_score = pmax(score_rise, score_fall, score_rise_fall, score_fall_rise,
                         score_rise_fall_rise, score_fall_rise_fall),
    # max possible signal = total absolute sentiment words in the song
    total_signal = abs(h1) + abs(h2) + abs(t1) + abs(t2) + abs(t3) + 
      abs(q1) + abs(q2) + abs(q3) + abs(q4),
    # normalise to 0-100%
    arc_fit_pct = case_when(
      total_signal == 0 ~ NA_real_,
      TRUE ~ pmin(winning_score / total_signal * 100, 100)  # cap at 100
    )
  ) %>%
  select(song_key, year, arc, ending, arc_fit_pct)

music_data_clean <- music_data_clean %>%
  left_join(arc_scores, by = c("song_key", "year"))

sentiment_over_time <- music_data_clean %>%
  filter(music_available == TRUE) %>%
  group_by(year) %>%
  summarise(avg_sentiment = mean(sentiment_score, na.rm = TRUE), .groups = "drop")

ggplot(sentiment_over_time, aes(x = year, y = avg_sentiment)) +
  geom_line(colour = "#4e79a7", size = 1) +
  geom_point(size = 2, colour = "#4e79a7") +
  geom_smooth(method = "loess", se = TRUE, colour = "grey40", linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", colour = "grey60") +
  labs(title = "Eurovision Song Sentiment Over Time",
       subtitle = "Average positive minus negative word score per year",
       x = NULL, y = "Avg sentiment score") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

audio_over_time <- music_data_clean %>%
  filter(music_available == TRUE) %>%
  group_by(year) %>%
  summarise(
    Danceability = mean(danceability, na.rm = TRUE),
    Energy = mean(energy, na.rm = TRUE),
    Valence = mean(valence, na.rm = TRUE),
    Acousticness = mean(acousticness, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "feature", values_to = "value")

ggplot(audio_over_time, aes(x = year, y = value, colour = feature)) +
  geom_line(size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", size = 0.5) +
  facet_wrap(~feature, scales = "free_y") +
  labs(title = "Audio Features Over Time",
       x = NULL, y = NULL, colour = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

emotions_over_time <- music_data_clean %>%
  filter(music_available == TRUE) %>%
  group_by(year) %>%
  summarise(across(c(joy, sadness, anger, fear, trust, disgust, anticipation, surprise),
                   ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-year, names_to = "emotion", values_to = "value")

ggplot(music_data_clean %>% filter(music_available == TRUE, !is.na(sentiment_score)),
       aes(x = valence, y = sentiment_score)) +
  geom_point(alpha = 0.4, colour = "#4e79a7") +
  geom_smooth(method = "lm", se = TRUE, colour = "grey40") +
  labs(title = "Lyrical Sentiment vs Musical Valence",
       subtitle = "Does happier music have more positive lyrics?",
       x = "Valence (audio)", y = "Sentiment score (lyrics)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggplot(music_data_clean %>% filter(!is.na(dominant_theme)),
       aes(x = fct_infreq(dominant_theme), fill = dominant_theme)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Dominant Emotion Themes Across All Eurovision Songs",
       x = NULL, y = "Number of songs", fill = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

# add total emotion word count per song as column
music_data_clean <- music_data_clean %>%
  mutate(total_emotion_words = rowSums(across(c(joy, anticipation, anger, disgust, 
                                                fear, sadness, surprise, trust)), 
                                       na.rm = TRUE))


# recalculate emotions as percentage of total emotion words
music_data_clean <- music_data_clean %>%
  mutate(across(c(joy, fear, sadness, anger, trust, disgust, anticipation, surprise),
                ~ . / total_emotion_words * 100,
                .names = "{.col}_pct"))


themes_over_time <- music_data_clean %>%
  filter(!is.na(year)) %>%
  group_by(year) %>%
  summarise(across(ends_with("_pct"), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = ends_with("_pct"),
               names_to = "emotion", values_to = "pct",
               names_pattern = "(.*)_pct")

ggplot(themes_over_time, aes(x = year, y = pct, colour = emotion)) +
  geom_line(size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", size = 0.5) +
  facet_wrap(~emotion, ncol = 2, scales = "free_y") +
  labs(title = "Emotional Themes in Eurovision Lyrics Over Time",
       subtitle = "Average % of lyrics matching each emotion category per year",
       x = NULL, y = "% of words", colour = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none")

# arc distribution overall
ggplot(music_data_clean %>% filter(!is.na(arc), arc != "flat"),
       aes(x = fct_infreq(arc), fill = ending)) +
  geom_bar() +
  scale_fill_manual(values = c("happy ending" = "#59a14f", "sad ending" = "#e15759")) +
  labs(title = "Story Arc Distribution Across Eurovision Songs",
       x = NULL, y = "Number of songs", fill = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# arc over time
arc_over_time <- music_data_clean %>%
  filter(!is.na(arc), arc != "flat") %>%
  count(year, ending) %>%
  group_by(year) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(arc_over_time, aes(x = year, y = pct, colour = ending)) +
  geom_line(size = 0.8) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.1) +
  scale_colour_manual(values = c("happy ending" = "#59a14f", "sad ending" = "#e15759")) +
  labs(title = "Happy vs Sad Endings Over Time",
       subtitle = "% of songs with each ending type per year",
       x = NULL, y = "%", colour = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# arc by country (top 20 most participating)
top_countries <- participation %>% 
  slice_max(years_participated, n = 30) %>% 
  pull(to_country)

arc_by_country <- music_data_clean %>%
  filter(!is.na(arc), arc != "flat", to_country %in% top_countries) %>%
  count(to_country, arc) %>%
  group_by(to_country) %>%
  mutate(pct = n / sum(n) * 100)

ggplot(arc_by_country, aes(x = to_country, y = pct, fill = arc)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Story Arc Distribution by Country",
       subtitle = "Top 20 most participating countries",
       x = NULL, y = "%", fill = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

emotions_by_country <- music_data_clean %>%
  filter(to_country %in% top_countries) %>%
  group_by(to_country) %>%
  summarise(across(c(joy, fear, sadness, anger, trust, disgust, anticipation, surprise),
                   ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(cols = c(joy, fear, sadness, anger, trust, disgust, anticipation, surprise),
               names_to = "emotion", values_to = "score") %>%
  group_by(to_country) %>%
  mutate(pct = score / sum(score, na.rm = TRUE) * 100) %>%  # normalise to 100% per country
  ungroup()

ggplot(emotions_by_country, aes(x = to_country, y = pct, fill = emotion)) +
  geom_col(position = "fill") +   # fill makes all bars same length
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Emotional Theme Distribution by Country",
       subtitle = "Top 20 most participating countries",
       x = NULL, y = NULL, fill = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))


write_csv(music_data_clean, "music_data_clean.csv")
