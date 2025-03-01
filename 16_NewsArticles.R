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
# 
# 
# years <- 1950:1990  # sort the years, so you don't end up looping over each line in the csv
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

news <- news %>%
  mutate(
    Type = case_when(
      str_detect(Attitude_towards_civil_defense, regex("Negative", ignore_case = TRUE)) ~ "Negative",
      str_detect(Attitude_towards_civil_defense, regex("Validates*", ignore_case = TRUE)) ~ "Validating",
        str_detect(Attitude_towards_civil_defense, regex("Positive*", ignore_case = TRUE)) ~ "Positive",
        str_detect(Attitude_towards_civil_defense, regex("Mixed*", ignore_case = TRUE)) ~ "Mixed",
        str_detect(Attitude_towards_civil_defense, regex("Neutral", ignore_case = TRUE)) ~ "Neutral",
      str_detect(Attitude_towards_civil_defense, regex("Different*", ignore_case = TRUE)) ~ "Mixed",
      TRUE ~ "Other"
    )
  )

glimpse(news)
# ------- Start with time
news <- news %>%
  mutate(Date = as_date(Date))

# ------- Start with time

# Convert the tibble into a temporal tsibble (lose 20 records without date)
n_ts <- as_tsibble(news %>% filter(!is.na(Date)), key = ID, index = Date)

#saveRDS(n_ts, "output_data/archivedocs20240820_tsbl.rds")

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

# Aggregate publication dates over monthly periods with type, and 'date' type for ggplot
docs_monthly <- type_monthly %>%
  as_tibble() %>% 
  mutate(year_month = as.Date(year_month))

# ---- Initial visualisation test - all documents, typed docs, and a grid

autoplot(df_monthly, count) +
  labs(
    title = "CD-relevant newspaper articles per Month",
    x = "Year-Month",
    y = "Number of articles"
  ) +
  theme_minimal()

ggsave("figures/articles_undiff.png")


autoplot(publ_monthly, count) +
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