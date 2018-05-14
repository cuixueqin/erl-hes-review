# *****************************************************************************
# * This file reads in a file of Web of Science search results and identifies *
# * distinct articles. It was used to determine which articles to read for    *
# * the review.                                                               *
# *****************************************************************************

library(readr)
library(tidyr)
library(dplyr)

DATA.ALL <- read_csv("./data/Article_Database_04-19-2018.csv")

# First, choose columns that we care about
DATA.ALL %>%
  select(Title, Authors, `Source Title`, `Publication Year`, Volume, Issue, DOI) ->
  DATA.SELECT_COLUMNS

# Next, subset for unique entries
DATA.SELECT_COLUMNS %>%
  distinct() ->
  DATA.DISTINCT

# Write out unique articles
write_csv(DATA.DISTINCT, "./output/distinct_articles.csv")