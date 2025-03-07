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

# ------- Start with time and Danish perspective & sentiment classification

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
  group_by(Perspective) %>% 
  summarise(count = n()) 

sentiment_monthly <- a_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Sentiment) %>% 
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type, and 'date' type for ggplot
per_monthly <- perspective_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

sent_monthly <- sentiment_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

# ---- Initial visualisation - all articles from DK, Danish fulltext classified by ChatGPT

autoplot(perspective_monthly, count) +
  labs(
    title = "CD-relevant newspaper articles per month",
    x = "Year-Month",
    y = "Number of articles"
  ) +
  guides(colour = guide_legend(title = "Document type")) +
  theme_minimal() +
  theme(
    legend.position.inside = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),  # Optional: Add a background to the legend for better visibility
    legend.title = element_text(size = 10),  # Adjust legend title size
    legend.text = element_text(size = 8)  # Adjust legend text size
  )# Adjust these values to move the legend

# clean up more of teh Type in original data : too many categories now


library(ggplot2)

# An area plot can work if you summarize the article counts over time instead of focusing on individual data points.

ggplot(per_monthly, aes(x = year_month, y = count, fill = Perspective)) +
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
ggsave( "figures/PerspectiveDK.png")

ggplot(sent_monthly, aes(x = year_month, y = count, fill = Sentiment)) +
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
ggsave( "figures/SentimentDK.png")


# ---- CD-relevant articles in Aarhus

# Convert the tibble into a temporal tsibble (lose records without date)
arhus_ts <- as_tsibble(articles %>% filter(Distributed_in_Aarhus == "Yes"), key = ID, index = Date)

# Aggregate publication dates over monthly periods
aa_df_monthly <- arhus_ts %>%
  #filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type
aa_persp_monthly <- arhus_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Perspective) %>% 
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type, and 'date' type for ggplot
aa_per_monthly <- aa_persp_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))


# Aggregate publication dates over monthly periods with Sentiment_EN
aa_sent_monthly <- arhus_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Sentiment) %>% 
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with Perspective_EN, and 'date' type for ggplot
aa_sen_monthly <- aa_sent_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

# An area plot can work if you summarize the article counts over time instead of focusing on individual data points.

ggplot(aa_per_monthly, aes(x = year_month, y = count, fill = Perspective)) +
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
ggsave( "figures/Perspective_Aarhus.png")

ggplot(aa_sen_monthly, aes(x = year_month, y = count, fill = Sentiment)) +
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
ggsave( "figures/Sentiment_Aarhus.png")