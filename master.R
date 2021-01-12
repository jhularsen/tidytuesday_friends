# Packages ----------------------------------------------------------------
library(tidyverse)

theme_jonas <- function() {
  
  theme_light() %+replace%   
    theme(
      #get rid of grid borders
      panel.border = element_blank(),
      # add white space top, right, bottom, left
      plot.margin = unit(c(1, 1, 1, 1), "cm"), 
      # custom axis title/text/lines
      axis.title = element_text(            
        size = 14),               
      axis.text = element_text(              
        size = 12),   
      # margin pulls text away from axis
      axis.text.x = element_text(           
        margin=margin(5, b = 10)),
      # black lines
      axis.line = element_line(colour = "black", size = rel(1)), 
      # custom plot titles, subtitles, captions
      plot.title = element_text(             
        size = 18, 
        vjust = 4),
      plot.subtitle = element_text(          
        size = 14,
        vjust = 3),
      plot.caption = element_text(           
        size = 10,
        hjust = 0, 
        vjust = 3), 
      # custom legend 
      legend.title = element_text(          
        size = 12), 
      legend.text = element_text(          
        size = 10), 
      #no background on legend
      legend.key = element_blank())
}

theme_set(theme_jonas())

# Explore data ------------------------------------------------------------

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

glimpse(friends_info)

friends_info %>%
  group_by(season) %>%
  summarise(mean_imdb = mean(imdb_rating), 
            mean_views = mean(us_views_millions))

friends_info %>% 
  mutate(episode_num = row_number()) %>%
  ggplot(aes(x = episode_num, y = us_views_millions)) + 
  geom_point(aes(color = as.factor(season)), size = 3) + 
  geom_line() + 
  geom_smooth(se = FALSE) + 
  labs(x = "Episode", y = "US views (millions)", color = "Season", title = "Friends viewership across episodes and seasons")

friends_info %>% 
  mutate(episode_num = row_number()) %>%
  ggplot(aes(x = episode_num, y = imdb_rating)) + 
  geom_point(aes(color = as.factor(season)), size = 3) + 
  geom_line() + 
  geom_smooth(se = FALSE) + 
  labs(x = "Episode", y = "IMDB rating", color = "Season", title = "IMDB rating across episodes and seasons") 

friends_info %>% 
  mutate(episode_num = row_number()) %>%
  ggplot(aes(x = episode_num, y = us_views_millions)) + 
  geom_point(aes(size = imdb_rating, color = as.factor(season))) + 
  geom_line() + 
  geom_smooth(se = FALSE) + 
  labs(x = "Episode", y = "US views (millions)", color = "Season", title = "Friends viewership and ratings across episodes and seasons", 
       size = "IMDB rating") +
  guides(color = guide_legend(override.aes = list(size = 3))) 

friends_info %>% 
  arrange(imdb_rating) %>% 
  mutate(title = paste0(season, ".", episode, ": ", title)) %>%
  mutate(title = fct_reorder(title, imdb_rating)) %>% 
  head(20) %>% 
  ggplot(aes(x = title, y = imdb_rating, color = as.factor(season), size = us_views_millions)) + 
  geom_point() + 
  coord_flip() + 
  labs(x = "Title", y = "IMDB rating", title = "Worst rated episodes of Friends", color = "Season", size = "Viewers (millions)") +
  guides(color = guide_legend(override.aes = list(size = 3)))

friends_info %>% 
  arrange(desc(imdb_rating)) %>% 
  mutate(title = paste0(season, ".", episode, ": ", title)) %>%
  mutate(title = fct_reorder(title, imdb_rating)) %>% 
  head(20) %>% 
  ggplot(aes(x = title, y = imdb_rating, color = as.factor(season), size = us_views_millions)) + 
  geom_point() + 
  coord_flip() + 
  labs(x = "Title", y = "IMDB rating", title = "Best rated episodes of Friends", color = "Season", size = "Viewers (millions)") + 
  guides(color = guide_legend(override.aes = list(size = 3)))

friends_info %>%
  mutate(directed_by = as.factor(directed_by)) %>%
  group_by(directed_by) %>%
  summarise(n = n(), 
            mean_imdb = mean(imdb_rating),
            mean_views = mean(us_views_millions)) %>%
  arrange(desc(n)) %>%
  filter(n > 10) %>%
  ggplot(aes(x = fct_reorder(directed_by, n), y = n, fill = directed_by)) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() + 
  labs(x = "", y = "Count", title = "Top contenders for most directed episodes") + 
  geom_text(aes(label = n), hjust = 0, size = 5)
  
  
  
  
