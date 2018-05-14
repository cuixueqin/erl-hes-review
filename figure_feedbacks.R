library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
source("./helpers.R")
source("./colors.R")

# **************************************************************************************************
# Read RCP-related data
RCP.DATA <- read_csv("./data/rcp_data/RCP_data.csv")
GLM.DATA.AIM <- read_csv("./data/rcp_data/aim_crop.csv")
GLM.DATA.MiniCAM <- read_csv("./data/rcp_data/minicam_crop.csv")
GLM.DATA.MESSAGE <- read_csv("./data/rcp_data/message_crop.csv")
GLM.DATA.IMAGE <- read_csv("./data/rcp_data/image_crop.csv")
TEMP.DATA.RCP26 <- read_csv("./data/rcp_data/rcp26_tas_summary.csv")
TEMP.DATA.RCP45 <- read_csv("./data/rcp_data/rcp45_tas_summary.csv")
TEMP.DATA.RCP60 <- read_csv("./data/rcp_data/rcp60_tas_summary.csv")
TEMP.DATA.RCP85 <- read_csv("./data/rcp_data/rcp85_tas_summary.csv")
TEMP.DATA.HIST <- read_csv("./data/rcp_data/hist_tas_summary.csv")

# **************************************************************************************************
# Tidy RCP data
TEMP.DATA.HIST %>%
  select(year, vtag, meanvalue) %>%
  filter(vtag == "tgav") %>%
  mutate(uniqueJoinField = 1) %>%
  full_join(mutate(tibble(scenario = c("RCP2.6", "RCP4.5", "RCP6.0", "RCP8.5")), uniqueJoinField = 1), by = "uniqueJoinField") %>%
  select(-uniqueJoinField) ->
  TEMP.DATA.HIST

TEMP.DATA.RCP26 %>%
  select(year, scenario, vtag, meanvalue) %>%
  bind_rows(select(TEMP.DATA.RCP45, year, scenario, vtag, meanvalue)) %>%
  bind_rows(select(TEMP.DATA.RCP60, year, scenario, vtag, meanvalue)) %>%
  bind_rows(select(TEMP.DATA.RCP85, year, scenario, vtag, meanvalue)) %>%
  filter(vtag == "tgav", year <= 2100) %>%
  mutate(scenario = sub("rcp26", "RCP2.6", scenario)) %>%
  mutate(scenario = sub("rcp45", "RCP4.5", scenario)) %>%
  mutate(scenario = sub("rcp60", "RCP6.0", scenario)) %>%
  mutate(scenario = sub("rcp85", "RCP8.5", scenario)) %>%
  bind_rows(TEMP.DATA.HIST) %>%
  rename(Year = year, Value = meanvalue, Scenario = scenario) ->
  RCP.TEMP.DATA

RCP.TEMP.DATA %>%
  filter(Year == 1850) %>%
  rename(BaseValue = Value) %>%
  select(-Year) ->
  RCP.BASETEMP.DATA

RCP.TEMP.DATA %>%
  left_join(RCP.BASETEMP.DATA, by=c("Scenario", "vtag")) %>%
  mutate(Value = Value - BaseValue) %>%
  filter(Year >= 2000) ->
  RCP.TEMP.DATA
  
RCP.DATA %>%
  gather(Year, Value, -Region, -Scenario, -Variable, -Unit) %>%
  mutate(Year = as.integer(Year)) %>%
  mutate(Variable = gsub("CO2 emissions - Fossil fuels and Industry", "CO2 Emissions", Variable)) %>%
  mutate(Variable = gsub("Concentration - CO2", "CO2 Concentration", Variable)) %>%
  mutate(Scenario = gsub("AIM - RCP 6.0", "RCP6.0", Scenario)) %>%
  mutate(Scenario = gsub("MESSAGE - RCP 8.5", "RCP8.5", Scenario)) %>%
  mutate(Scenario = gsub("MiniCAM - RCP 4.5", "RCP4.5", Scenario)) %>% 
  mutate(Scenario = gsub("IMAGE - RCP3-PD \\(2.6\\)", "RCP2.6", Scenario)) %>%
  select(-Region) ->
  RCP.DATA

GLM.DATA.AIM %>%
  filter(region == "WORLD") %>%
  rename(Year = year) %>%
  mutate(Value = x/1e6) %>%
  mutate(Scenario = "RCP6.0", Variable = "Cropland", Unit = "thous km2") %>%
  select(Scenario, Variable, Unit, Year, Value) ->
  GLM.DATA.AIM

GLM.DATA.MiniCAM %>%
  filter(region == "WORLD") %>%
  rename(Year = year) %>%
  mutate(Value = x/1e6) %>%
  mutate(Scenario = "RCP4.5", Variable = "Cropland", Unit = "thous km2") %>%
  select(Scenario, Variable, Unit, Year, Value) ->
  GLM.DATA.MiniCAM

GLM.DATA.MESSAGE %>%
  filter(region == "WORLD") %>%
  rename(Year = year) %>%
  mutate(Value = x/1e6) %>%
  mutate(Scenario = "RCP8.5", Variable = "Cropland", Unit = "thous km2") %>%
  select(Scenario, Variable, Unit, Year, Value) ->
  GLM.DATA.MESSAGE

GLM.DATA.IMAGE %>%
  filter(region == "WORLD") %>%
  rename(Year = year) %>%
  mutate(Value = x/1e6) %>%
  mutate(Scenario = "RCP2.6", Variable = "Cropland", Unit = "thous km2") %>%
  select(Scenario, Variable, Unit, Year, Value) ->
  GLM.DATA.IMAGE

tibble::tibble(Scenario = c("RCP2.6", "RCP4.5", "RCP6.0", "RCP8.5"),
               X2000 = c(0,0,0,0),
               X2100 = c(0,0,0,0)) %>%
  gather(Year, Value, -Scenario) %>%
  mutate(Year = as.integer(substr(Year, 2, 5))) ->
  RCP.PROD.DATA

ALL_RCP_DATA <- rbind(RCP.DATA, GLM.DATA.AIM, GLM.DATA.MiniCAM, GLM.DATA.MESSAGE, GLM.DATA.IMAGE)

# **************************************************************************************************
# Read and tidy Feedbacks-related data
FEEDBACKS.DATA <- read_csv("./data/data_feedbacks.csv")

FEEDBACKS.DATA %>%
  gather(Variable, Value, -Study, -Scenario, -Year, -Notes) ->
  FEEDBACKS.DATA

# **************************************************************************************************
# Do necessary manipulations on feedbacks data

# First, interpolate RCP data. This will help later
RCP.DATA %>%
  group_by(Scenario, Variable, Unit) %>%
  expand(Year = full_seq(Year, 1)) %>%
  left_join(RCP.DATA, by=c("Scenario", "Variable", "Unit", "Year")) %>%
  mutate(Value = stats::approx(Year, Value, Year)$y) %>%
  ungroup() ->
  RCP.DATA

# Next, emissions data
FEEDBACKS.DATA %>%
  filter(Variable == "GHG emissions (GtCO2eq/yr)") %>%
  na.omit() %>%
  mutate(Value = as.numeric(Value),
         Year = as.integer(Year), 
         Variable = "CO2 Emissions",
         Unit = "PgC/yr") %>%
  mutate(Value = Value * 1000 * (12/44)) %>% # Convert to PgC/yr
  select(Study, Year, Variable, Value, Unit, Notes) %>%
  mutate(Scenario = "RCP8.5") %>% # YANG IS HISTORIC AND CAN USE ANY RCP DATA
  left_join(RCP.DATA, by=c("Year", "Variable", "Unit", "Scenario")) %>%
  mutate(Value = Value.x + Value.y) %>% # Value.x has change in PgC/y; Value.y has no feedbacks value
  mutate(Scenario = if_else(Study == "Yang et al. (2015)", "Historic", Scenario)) %>% # Change YANG scenario for plotting
  select(-Value.x, -Value.y) -> 
  EMISSIONS_DATA
  
FEEDBACKS.DATA %>%
  filter(Variable == "CO2 emissions (% change from no feedbacks)") %>%
  na.omit() %>%
  mutate(Value = as.numeric(Value),
         Year = as.integer(Year), 
         Variable = "CO2 Emissions",
         Unit = "PgC/yr",
         Scenario = if_else(Scenario == "BAU", "RCP8.5", Scenario)) %>%
  left_join(RCP.DATA, by=c("Year", "Variable", "Unit", "Scenario")) %>%
  mutate(Value = (1 + Value.x) * Value.y) %>% # Value.x has % change; Value.y has no feedbacks value
  select(-Value.x, -Value.y) %>%
  bind_rows(EMISSIONS_DATA) ->
  EMISSIONS_DATA

FEEDBACKS.DATA %>%
  filter(Variable == "C emissions (GtC/yr)") %>%
  na.omit() %>%
  mutate(Value = as.numeric(Value),
         Year = as.integer(Year), 
         Variable = "CO2 Emissions",
         Unit = "PgC/yr") %>%
  select(Study, Year, Variable, Value, Unit, Notes) %>%
  mutate(Scenario = "RCP8.5") %>% # Beckage is BAU
  left_join(RCP.DATA, by=c("Year", "Variable", "Unit", "Scenario")) %>%
  mutate(Value = Value.x + Value.y) %>% # Value.x has change in PgC/y; Value.y has no feedbacks value
  select(-Value.x, -Value.y) %>%
  bind_rows(EMISSIONS_DATA) -> 
  EMISSIONS_DATA


# Next, concentration data
FEEDBACKS.DATA %>%
  filter(Variable == "CO2 Concentration (ppm)") %>%
  na.omit() %>%
  mutate(Value = as.numeric(Value),
         Year = as.integer(Year), 
         Variable = "CO2 Concentration",
         Unit = "ppm") %>%
  select(Study, Year, Variable, Value, Unit, Notes) %>%
  mutate(Scenario = "RCP4.5") %>% # YANG IS HISTORIC AND CAN USE ANY RCP DATA
  left_join(RCP.DATA, by=c("Year", "Variable", "Unit", "Scenario")) %>%
  mutate(Value = Value.x + Value.y) %>% # Value.x has change in PgC/y; Value.y has no feedbacks value
  mutate(Scenario = if_else(Study == "Yang et al. (2015)", "Historic", Scenario)) %>% # Change YANG scenario for plotting
  select(-Value.x, -Value.y) -> 
  CONC_DATA

# Next, cropland data
FEEDBACKS.DATA %>%
  filter(Variable == "Cropland area (%)") %>%
  na.omit() %>%
  mutate(Value = as.numeric(Value),
         Year = as.integer(Year), 
         Variable = "Cropland",
         Unit = "thous km2") %>%
  select(Study, Year, Variable, Value, Unit, Notes) %>%
  mutate(Scenario = "RCP4.5") %>% 
  left_join(GLM.DATA.MiniCAM, by=c("Year", "Variable", "Unit", "Scenario")) %>%
   mutate(Value = (1 + Value.x) * Value.y) %>% # Value.x has % change; Value.y has no feedbacks value
  select(-Value.x, -Value.y) -> 
  CROP_DATA

# Next, temperature data
FEEDBACKS.DATA %>%
  filter(Variable == "Global Mean Temperature (degrees C)") %>%
  na.omit() %>%
  mutate(Value = as.numeric(Value),
         Year = as.integer(Year), 
         Unit = "degree C") %>%
  select(Study, Year, Variable, Value, Unit, Notes) %>%
  mutate(Scenario = "RCP8.5") %>% # YANG IS HISTORIC AND CAN USE ANY RCP DATA; Beckage is a reference -- 2100 temp is ~4.9
  left_join(RCP.TEMP.DATA, by=c("Year", "Scenario")) %>%
  mutate(Value = Value.x + Value.y) %>% # Value.x has change in thous km2; Value.y has no feedbacks value
  mutate(Scenario = if_else(Study == "Yang et al. (2015)", "Historic", Scenario)) %>% # Change YANG scenario for plotting
  mutate(Scenario = if_else(Study == "Beckage et al. (2018)", "Baseline", Scenario)) %>% # Change BECKAGE scenario for plotting
  mutate(Scenario = if_else(Study == "Voldoire et al. (2007)", "A2", Scenario)) %>% # Change VOLDOIRE scenario for plotting
  select(-Value.x, -Value.y) -> 
  TEMP_DATA

# Next, productivity data
FEEDBACKS.DATA %>%
  filter(Variable == "Productivity Change") %>%
  select(-Notes) %>%
  na.omit() %>%
  mutate(Value = as.numeric(Value) * 100, # Convert to %
         Year = as.integer(Year), 
         Unit = "%") %>%
  select(Study, Scenario, Year, Variable, Value, Unit) -> 
  PROD_DATA

# **************************************************************************************************
# Plot RCP information with Feedbacks
# Panel A = CO2 Emissions
RCPa.DAT <- subset(ALL_RCP_DATA, Variable == "CO2 Emissions")

title.a <- expression(paste(CO[2], " Emissions", sep=" "))
pa <- ggplot() + geom_line(data=RCPa.DAT, aes(Year, Value, color=Scenario))
pa <- pa + geom_point(data=EMISSIONS_DATA, aes(Year, Value, color=Scenario, shape=Study), size=3)
pa <- pa + theme(legend.title=element_blank()) + xlab( "Year" ) + ylab(unique(RCPa.DAT$Unit)) 
pa <- pa + ggtitle(title.a) + rcpColorScale + studyShapeScale
pa <- pa + theme( legend.text = element_text(size = 16), 
                  axis.text.x = element_text(size = 16), 
                  axis.text.y = element_text(size = 16),
                  axis.title = element_text(size = 16) )
print(pa)

# Panel B = CO2 Concentration
RCPb.DAT <- subset(ALL_RCP_DATA, Variable == "CO2 Concentration")

title.b <- expression(paste(CO[2], " Concentration", sep=" "))
max_y <- max(RCPb.DAT$Value)
pb <- ggplot() + geom_line(data=RCPb.DAT, aes(Year, Value, color=Scenario))
pb <- pb + geom_point(data=CONC_DATA, aes(Year, Value, color=Scenario, shape=Study), size=3)
pb <- pb + theme(legend.title=element_blank()) + xlab( "Year" ) + ylab(unique(RCPb.DAT$Unit)) 
pb <- pb + ggtitle(title.b) + ylim(0, max_y) + rcpColorScale + studyShapeScale
pb <- pb + theme( legend.text = element_text(size = 16), 
                  axis.text.x = element_text(size = 16), 
                  axis.text.y = element_text(size = 16),
                  axis.title = element_text(size = 16) )
print(pb)

# Panel C = Cropland
RCPc.DAT <- subset(ALL_RCP_DATA, Variable == "Cropland")

title.c <- "Cropland Area"
ylab.c <- expression(paste("million ", km^2, sep=" "))
max_y <- max(RCPc.DAT$Value)
pc <- ggplot() + geom_line(data=RCPc.DAT, aes(Year, Value, color=Scenario))
pc <- pc + geom_point(data=CROP_DATA, aes(Year, Value, color=Scenario, shape=Study), size=3)
pc <- pc + theme(legend.title=element_blank()) + xlab( "Year" ) + ylab(ylab.c) 
pc <- pc + ggtitle(title.c) + ylim(0, max_y) + rcpColorScale + studyShapeScale
pc <- pc + theme( legend.text = element_text(size = 16), 
                  axis.text.x = element_text(size = 16), 
                  axis.text.y = element_text(size = 16),
                  axis.title = element_text(size = 16) )
print(pc)

# Panel D = Temperature
title.d <- "GMT (CMIP5 multi-model mean)"
ylab.d <- "Change in GMT from 1850 (degrees C)"
pd <- ggplot() + geom_line(data=RCP.TEMP.DATA, aes(Year, Value, color=Scenario))
pd <- pd + geom_point(data=TEMP_DATA, aes(Year, Value, color=Scenario, shape=Study), size=3)
pd <- pd + theme(legend.title=element_blank()) + xlab( "Year" ) + ylab(ylab.d) 
pd <- pd + ggtitle(title.d) + rcpColorScale + studyShapeScale
pd <- pd + theme( legend.text = element_text(size = 16), 
                  axis.text.x = element_text(size = 16), 
                  axis.text.y = element_text(size = 16),
                  axis.title = element_text(size = 16) )
print(pd)

# Panel E = Productivity
title.e <- "Land Productivity"
ylab.e <- "Change in Productivity due to Feedbacks (%)"
pe <- ggplot() + geom_line(data=RCP.PROD.DATA, aes(Year, Value, color=Scenario))
pe <- pe + geom_point(data=PROD_DATA, aes(Year, Value, color=Scenario, shape=Study), size=3)
pe <- pe + theme(legend.title=element_blank()) + xlab( "Year" ) + ylab(ylab.e) 
pe <- pe + ggtitle(title.e) + rcpColorScale + studyShapeScale
pe <- pe + theme( legend.text = element_text(size = 16), 
                  axis.text.x = element_text(size = 16), 
                  axis.text.y = element_text(size = 16),
                  axis.title = element_text(size = 16 ) )
print(pe)

png( "./output/Figure5.png", width = 960, height = 960, units = "px", pointsize = 12)
multiplot(pa, pd, pe, pb, pc, cols=2 )
dev.off()
