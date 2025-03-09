## Public Discourse
library(tidyverse)
library(googlesheets4)
library(tsibbledata)
library(fable)
library(lubridate)


# Download data
gs4_deauth()
news <- read_sheet("https://docs.google.com/spreadsheets/d/1LgStefx41sNq3oIDijGLlPFoSvOvTnlP7U4lH5MaGc4/edit?gid=0#gid=0",
           range = "Articles")
names(news)

# Classify the outlet by relevance to Aarhus
sort(unique(news$Newspaper))
write_delim(data.frame(Original_Newspaper_Name = sort(unique(news$Newspaper)),
                                                      Distributed_in_Aarhus = relevant), "test.txt")

relevant <- c("No","No","Yes","Yes","Yes","Yes","No","No","Yes","Yes","Yes","No","No","No","Yes","Yes","Yes","Yes","Yes","No","No","No","Yes","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","Yes","Yes","Yes","No","Yes","No","No","No","No","No","No","Yes","No","No","Yes","No","No","No","No","No","No","No","No","No","No","No","No","No","No",
                          "No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","No","Yes")
newspaper <- as_tibble(data.frame(Original_Newspaper_Name = sort(unique(news$Newspaper)),
                        Distributed_in_Aarhus = relevant))   

# Join the relevance to news dataset
news <- news %>% 
  left_join(newspaper, by = c("Newspaper" = "Original_Newspaper_Name"))

glimpse(news)

# # Export each article as a text file: 

# # Define the subdirectory path
# sub_dir <- "output_data/newspapers"
# 
# # Check if the subdirectory exists, and create it if it does not exist
# if (!dir.exists(sub_dir)) {
#   dir.create(sub_dir, recursive = TRUE)
#   message("Directory '", sub_dir, "' created successfully.")
# } else {
#   message("Directory '", sub_dir, "' already exists.")
# }
# # # years <- 1950:1990  # sort the years, so you don't end up looping over each line in the csv
# months <- 1:12  # same for months
# 
# for (y in years){
#   for (i in months){
#     output <- subset(news[,c(1,3,4, 6:10)], Year == y & Month == i)
#     write.table(output, file = paste0("output_data/newspapers/",y,"_",i,"_newsarticles.txt"), sep = "\t")
#   }
# }

# Voyant: https://voyant-tools.org/?corpus=d1349fcba96a6810b0233420b3dfc9b9

# ---Classification of documents by sentiment (Positive Negative)
table(news$Attitude_towards_civil_defense)

news <- news %>%
  mutate(
    Type = case_when(
      str_detect(Attitude_towards_civil_defense, regex("^Negative*", ignore_case = TRUE)) ~ "Negative",
      str_detect(Attitude_towards_civil_defense, regex("^Validate*", ignore_case = TRUE)) ~ "Validating",
        str_detect(Attitude_towards_civil_defense, regex("^Positive*", ignore_case = TRUE)) ~ "Positive",
        str_detect(Attitude_towards_civil_defense, regex("^Mixed*", ignore_case = TRUE)) ~ "Critique",
        str_detect(Attitude_towards_civil_defense, regex("^Neutral", ignore_case = TRUE)) ~ "Neutral",
      str_detect(Attitude_towards_civil_defense, regex("^[Dd]ifferent*", ignore_case = TRUE)) ~ "Critique",
      TRUE ~ "Other"
    )
  ) 
#%>% 
#  select(Title, Newspaper, Type, Attitude_towards_civil_defense ) %>% 
#  print(n =20)

glimpse(news)
# news %>% 
#   mutate(Fulltext = str_replace_all(Fulltext, "\n", " ")) %>% 
#   select(ID, Content, Fulltext) %>% 
#   write_csv("ChatGPTdata/ArticleContent.csv")

# ---# ------- Start with time

news <- news %>%
  select(-Duplicate) %>% 
  rename(Duplicate = DuplicateCHatGPT) %>% 
  mutate(Page_number = as.character(Page_number)) %>% 
  mutate(Date = as_date(Date))

# ------- Start with time

# Convert the tibble into a temporal tsibble (lose records without date)
n_ts <- as_tsibble(news %>% filter(!is.na(Date)), key = ID, index = Date)

# saveRDS(n_ts, "output_data/articles_ts.rds")

# n_ts <- read_rds("output_data/articles_ts.rds")
# Aggregate publication dates over monthly periods
df_monthly <- n_ts %>%
  #filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type
type_monthly <- n_ts %>% 
 # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Type) %>% 
  summarise(count = n()) 

dupl_monthly <- n_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Duplicate, Type) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

# Aggregate publication dates over monthly periods with type, and 'date' type for ggplot
docs_monthly <- type_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

# ---- Initial visualisation test - all documents, typed docs, and a grid

autoplot(type_monthly, count) +
  labs(
    title = "CD-relevant newspaper articles per Month",
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

# An area plot can work if you summarize the article counts over time instead of focusing on individual data points.

ggplot(docs_monthly, aes(x = year_month, y = count, fill = Type)) +
  geom_area(position = "stack", alpha = 0.6) +  # Stacked area to show cumulative effect
  # geom_line(data = dupl_monthly %>% filter(Duplicate == "Y"),  # strange grouping!
  #           aes(y = count, color = "Duplicates"), size = 1, linetype = "dashed") +
  labs(
    title = "Articles discussing civil-defense in Danish newspapers, duplicates highlighted",
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
ggsave( "figures/CDarticlesDK.png")

## Duplicates as geom_line()
ggplot(docs_monthly, aes(x = year_month, y = count, fill = Type) ) +
  geom_area(position = "stack", alpha = 0.6) +  # Stacked area for cumulative effect
  
  # Corrected: Removed fill=Type from geom_line()
 # ggplot()+
  geom_line(data = dupl_monthly %>% filter(Duplicate == "Y"), 
            aes(x = year_month, y = count, 
                fill = "Duplicates",
                color = "Duplicates"), 
            size = 1, 
            linetype = "dashed") +
  
  # Fix legend: ensure Duplicates appears separately as a dashed line
  # scale_fill_viridis_d(name = "Article Type")+
  # scale_fill_manual(name = "Article Type", 
  # values = c("Critique" = "red", "Negative" = "purple", "Neutral" = "skyblue", 
  #            "Other" = "aquamarine", "Positive" = "forestgreen", "Validating" = "hotpink")) +
  scale_color_manual(name = "Legend", values = c("Duplicates" = "black"))+
  
  guides(
    fill = guide_legend(order = 1, title = "Article Type"),
    color = guide_legend(order = 2, title = "Duplicates")
  ) +
  
  labs(
    title = "Articles discussing civil-defense in Danish newspapers, duplicates highlighted",
    x = "Year-Month",
    y = "Number of articles"
  ) +
  guides(fill = guide_legend(title = "Article type")) +
  theme_minimal() +
  theme(
    legend.position = c(0.9, 0.8),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

ggsave( "figures/CDarticlesDKdupl.png")


## Indicate duplicates with a semi-transparent layer

ggplot(docs_monthly, aes(x = year_month, y = count, fill = Type)) +
  geom_area(alpha = 0.7) +  # Main sentiment area plot
  geom_area(data = filter(dupl_monthly, Duplicate == "Y"), 
            aes(fill = "Duplicate"), 
            alpha = 0.4) +  # Overlay duplicates
  labs(title = "Public Discourse Over Time with Duplicates Highlighted",
       x = "Year-Month",
       y = "Number of Articles",
       fill = "Sentiment Type") +
  theme_minimal()





# ---- CD-relevant articles in Aarhus

# Convert the tibble into a temporal tsibble (lose records without date)
an_ts <- as_tsibble(news %>% filter(Distributed_in_Aarhus == "Yes"), key = ID, index = Date)

#saveRDS(n_ts, "output_data/archivedocs20240820_tsbl.rds")

# Aggregate publication dates over monthly periods
aa_df_monthly <- an_ts %>%
  #filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type
aa_type_monthly <- an_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Type) %>% 
  summarise(count = n()) 

# Aggregate publication dates over monthly periods with type, and 'date' type for ggplot
aa_docs_monthly <- aa_type_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))


dupl_a_monthly <- an_ts %>% 
  # filter(Date > "1940-01-01") %>% 
  tsibble::index_by(year_month = ~ yearmonth(.)) %>% # monthly aggregates
  group_by(Duplicate, Type) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))


# An area plot can work if you summarize the article counts over time instead of focusing on individual data points.

ggplot(aa_docs_monthly, aes(x = year_month, y = count, fill = Type)) +
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
  ggsave( "figures/CDarticlesAarhus.png")

  
## Duplicates as geom_line()
ggplot(aa_docs_monthly, aes(x = year_month, y = count, fill = Type) ) +
    geom_area(position = "stack", alpha = 0.6) +  # Stacked area for cumulative effect
    geom_line(data = dupl_a_monthly %>% filter(Duplicate == "Y"), 
              aes(x = year_month, y = count, 
                  fill = "Duplicates",
                  color = "Duplicates"), 
              size = 1, 
              linetype = "dashed") +
    
    # Fix legend: ensure Duplicates appears separately as a dashed line
    # scale_fill_viridis_d(name = "Article Type")+
    # scale_fill_manual(name = "Article Type", 
    # values = c("Critique" = "red", "Negative" = "purple", "Neutral" = "skyblue", 
    #            "Other" = "aquamarine", "Positive" = "forestgreen", "Validating" = "hotpink")) +
    scale_color_manual(name = "Legend", values = c("Duplicates" = "black"))+
    
    guides(
      fill = guide_legend(order = 1, title = "Article Type"),
      color = guide_legend(order = 2, title = "Duplicates")
    ) +
    
    labs(
      title = "Articles discussing civil-defense in Aarhus newspapers, duplicates highlighted",
      x = "Year-Month",
      y = "Number of articles"
    ) +
    guides(fill = guide_legend(title = "Article type")) +
    theme_minimal() +
    theme(
      legend.position = c(0.9, 0.8),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
  
ggsave( "figures/CDarticlesAarhus_dupl.png")
