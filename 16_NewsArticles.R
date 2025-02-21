## Public Discourse
library(googlesheets4)

news <- read_sheet("https://docs.google.com/spreadsheets/d/1LgStefx41sNq3oIDijGLlPFoSvOvTnlP7U4lH5MaGc4/edit?gid=0#gid=0",
           range = "Articles")
names(news)
years <- 1950:1990  # sort the years, so you don't end up looping over each line in the csv
months <- 1:12  # same for months
for (y in years){
  for (i in months){
    output <- subset(news[,c(1,3,4, 6:10)], Year == y & Month == i)
    write.table(output, file = paste0("output_data/newspapers/",y,"_",i,"_newsarticles.txt"), sep = "\t")
  }
}

# Voyant: https://voyant-tools.org/?corpus=d1349fcba96a6810b0233420b3dfc9b9