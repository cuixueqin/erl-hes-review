library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
theme_set(theme_bw())

source("./colors.R")

DATA.ALL <- read_csv("./data/data_categorization.csv")

# **********************************************************************************************************
# Plot distribution of articles for each category
# First, summarize articles by classification (i.e., count total number of articles in each category)
DATA.ALL %>%
  mutate(Number = 1) %>%
  mutate(Classification = if_else(Classification != "Exclude", Type, Classification)) %>%
  group_by(Classification) %>%
  summarize(Number = sum(Number)) ->
  DATA_by_TYPE

DATA_by_TYPE$Classification <- factor(DATA_by_TYPE$Classification, levels = c("Integrated Model", "Linking tool",
                                                                              "Coupling example", "Review",
                                                                              "Commentary", "Exclude"))

# Now, make a pie chart showing these
p <- ggplot() + geom_bar(data=DATA_by_TYPE, aes(factor(1), Number, fill=Classification), stat="identity")
p <- p + coord_polar(theta="y") 
p <- p + ylab("") + xlab("") + classFillScale
p <- p + theme(legend.text = element_text(size = 12), 
               axis.ticks = element_blank(), 
               axis.text.y = element_blank())
print(p)
ggsave("./output/Figure1.png", width=6, height=4)

# **********************************************************************************************************
# Plot distribution of included articles over time
# First, summarize articles by classification and year
#    In this case, we add all of the other included articles together (Integrated Models remain separate)
DATA.ALL %>%
  filter(Classification != "Exclude") %>%
  mutate(Classification = if_else(Classification == "Include", Type, "Other")) %>%
  mutate(Number = 1) %>%
  group_by(Classification, `Publication Year`) %>%
  summarize(Number = sum(Number)) ->
  DATA_by_YEAR

# Now, plot as a bar chart with time on the x-axis
p <- ggplot() + geom_bar(data=DATA_by_YEAR, aes(`Publication Year`, Number, fill=Classification), stat="identity")
p <- p + classFillScale
print(p)
ggsave("./output/Figure2.png", width=8, height=4)


# **********************************************************************************************************
# Plot articles by region
DATA.ALL %>%
  filter(Classification == "Include") %>%
  mutate(Number = 1) %>%
  group_by(Spatial) %>%
  summarize(Number = sum(Number)) ->
  DATA_by_REGION

p <- ggplot() + geom_bar(data=DATA_by_REGION, aes(factor(1), Number, fill=Spatial), stat="identity")
p <- p + coord_polar(theta="y") 
p <- p + ylab("") + xlab("") + spatialFillScale
p <- p + theme(legend.text = element_text(size = 12), 
               axis.ticks = element_blank(), 
               axis.text.y = element_blank())
print(p)
ggsave("./output/Figure3.png", width=4, height=4)

# **********************************************************************************************************
# Print article database as a table.
DATA.ALL %>%
  mutate(Spatial = if_else(Classification == "Include", Spatial, "")) %>%
  mutate(Classification = if_else(Classification == "Exclude", Classification, Type)) %>%
  mutate(Citation = paste0(`Authors (abbr)`, " (", `Publication Year`, ")")) %>%
  select(Citation, DOI, Classification, Spatial) %>%
  arrange(Classification)->
  DATA.TABLE

DATA.TABLE %>%
  filter(Classification != "Exclude") ->
  DATA.TABLE.INC 

DATA.TABLE.INC %>%
  filter(Classification == "Integrated Model") ->
  DATA.TABLE.MOD

DATA.TABLE.INC %>%
  filter(Classification != "Integrated Model") %>%
  select(-Spatial) ->
  DATA.TABLE.OTH

file.name <- paste0("./output/", "article_database_model.png")
png(file.name, width = 640, height = 960, units = "px", pointsize = 12)
grid.table(DATA.TABLE.MOD)
dev.off()

file.name <- paste0("./output/", "article_database_other.png")
png(file.name, width = 640, height = 960, units = "px", pointsize = 12)
grid.table(DATA.TABLE.OTH)
dev.off()
