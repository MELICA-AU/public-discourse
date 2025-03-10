## Article Classification - Danish vs English

#  ------- Load data

# News

news <- readRDS("output_data/articles_ts.rds") 
news <- news %>% 
  select(ID, Date,Title, Attitude_towards_civil_defense, Type, Newspaper, Distributed_in_Aarhus )
glimpse(news)
# ChatGPT-classified data

articles <- read_csv("ChatGPTdata/Classified_Articles_EnglishNLP.csv")


articles <- articles %>% 
  left_join(news, by = "ID")

glimpse(articles)

# ------- Start with time and English perspective & sentiment classification

# Convert the tibble into a temporal tsibble (lose records without date)
a_ts <- as_tsibble(articles, key = ID, index = Date)

# saveRDS(n_ts, "output_data/articles_ts.rds")

# Aggregate publication dates over monthly periods
df_monthly <- a_ts %>%
  #filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type
perspective_monthly <- a_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Perspective_EN) %>% 
  summarise(count = n()) 

sentiment_monthly <- a_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Sentiment_EN) %>% 
  summarise(count = n()) 

bias_monthly <- a_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Bias_EN) %>% 
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type, and 'date' type for ggplot
per_monthly <- perspective_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

sent_monthly <- sentiment_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

bias_monthly <- bias_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

# ---- Initial visualisation - all articles from DK, English content classified by ChatGPT
library(ggplot2)

# An area plot can work if you summarize the article counts over time instead of focusing on individual data points.

ggplot(per_monthly, aes(x = year_month, y = count, fill = Perspective_EN)) +
  geom_area(position = "stack", alpha = 0.6) +  # Stacked area to show cumulative effect
  labs(
    title = "Articles discussing civil-defense per month from all Danish newspapers",
    x = "Year-Month",
    y = "Number of articles"
  ) +
  guides(fill = guide_legend(title = "Article position")) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
ggsave( "figures/Perspective_enDK.png")

ggplot(sent_monthly, aes(x = year_month, y = count, fill = Sentiment_EN)) +
  geom_area(position = "stack", alpha = 0.6) +  # Stacked area to show cumulative effect
  labs(
    title = "Articles discussing civil-defense per month from all Danish newspapers",
    x = "Year-Month",
    y = "Number of articles"
  ) +
  guides(fill = guide_legend(title = "Article position")) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
ggsave( "figures/Sentiment_enDK.png")


ggplot(bias_monthly, aes(x = year_month, y = count, fill = Bias_EN)) +
  geom_area(position = "stack", alpha = 0.6) +  # Stacked area to show cumulative effect
  labs(
    title = "Articles discussing civil-defense per month from all Danish newspapers",
    x = "Year-Month",
    y = "Number of articles"
  ) +
  guides(fill = guide_legend(title = "Article position")) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
ggsave( "figures/Bias_enDK.png")



# ---- CD-relevant articles in Aarhus

# Convert the tibble into a temporal tsibble (lose records without date)
arhus_ts <- as_tsibble(articles %>% filter(Distributed_in_Aarhus == "Yes"), key = ID, index = Date)

# Aggregate publication dates over monthly periods
aa_df_monthly <- arhus_ts %>%
  #filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with Perspective_EN
aa_persp_monthly <- arhus_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Perspective_EN) %>% 
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with Perspective_EN, and 'date' type for ggplot
aa_per_monthly <- aa_persp_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

# Aggregate publication dates over monthly periods with Sentiment_EN
aa_sent_monthly <- arhus_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Sentiment_EN) %>% 
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with Perspective_EN, and 'date' type for ggplot
aa_sen_monthly <- aa_sent_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

# An area plot can work if you summarize the article counts over time instead of focusing on individual data points.

ggplot(aa_per_monthly, aes(x = year_month, y = count, fill = Perspective_EN)) +
  geom_area(position = "stack", alpha = 0.6) +  # Stacked area to show cumulative effect
  labs(
    title = "Articles discussing civil-defense per month in Aarhus",
    x = "Year-Month",
    y = "Number of articles"
  ) +
  guides(fill = guide_legend(title = "Article position")) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
ggsave( "figures/Perspective_enAarhus.png")

ggplot(aa_sen_monthly, aes(x = year_month, y = count, fill = Sentiment_EN)) +
  geom_area(position = "stack", alpha = 0.6) +  # Stacked area to show cumulative effect
  labs(
    title = "Articles discussing civil-defense per month in Aarhus",
    x = "Year-Month",
    y = "Number of articles"
  ) +
  guides(fill = guide_legend(title = "Article position")) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )
ggsave( "figures/Sentiment_enAarhus.png")
