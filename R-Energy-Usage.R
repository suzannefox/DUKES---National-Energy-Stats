library(tidyverse)
library(janitor)
library(tidyxl)
library(scales)
library(here)

data_source <- 'https://www.gov.uk/government/publications/regional-energy-data-guidance-note'

fn_pcent <- function(numerator, denominator) {
  pcent <- numerator / denominator * 100
  pcent <- round(pcent, 1)
  pcent <- as.character(pcent)
  pcent <- replace_na(pcent, '-')
  pcent <- paste0(pcent,'%')
  pcent <- replace(pcent, pcent == '-%', '-')
}

# -------------------------------------------------------------------------
# STEP 1
# make a df of the list of files/sheets to scrape
# record various parameters, such as the number of lines to skip
# which is different for gas and electricity files
# use the sub-national data

data_dir <- 'data-xlsx'
data_gas <- 'subnational_gas_consumption_statistics_non_weather_corrected_2015-2021.xlsx'
data_ele <- 'subnational_electricity_consumption_statistics_2005-2021.xlsx'

sheets <- seq(2015,2021) %>% as.character()

dfs <- as.data.frame(sheets) %>% 
  mutate(file = data_gas, type='gas', skip = 5) %>% 
  bind_rows(as.data.frame(sheets) %>% 
              mutate(file = data_ele, type = 'elec', skip = 4))

# -------------------------------------------------------------------------
# STEP 2
# extract the data from each file/sheet in the list and create a 
# single tidy dataset with all the measures

for (i in seq_len(nrow(dfs))) {
  
  myfile <- dfs[i, c('file')] %>% as.character()
  mytype <- dfs[i, c('type')] %>% as.character()
  myyear <- dfs[i, c('sheets')] %>% as.character()
  myskip <- dfs[i, c('skip')] %>% as.integer()

  # read the file and sheet
  df_temp <- readxl::read_xlsx(here(data_dir, myfile), 
                               sheet = myyear,
                               skip = myskip) %>% 
    janitor::clean_names()
  
  # add notes col if there isn't one
  if (!"notes" %in% colnames(df_temp)) df_temp <- df_temp %>% mutate(notes = '')
  
  # make data tidy
  df_temp <- df_temp %>% 
    mutate(year = myyear,
           type = mytype) %>% 
    pivot_longer(-c('year','type','code','country_or_region','local_authority','notes'),
                 names_to = 'measure') 
  
  # make a dummy consolidated output df
  if (i == 1) df_energy <- df_temp %>% slice(0)
  df_energy <- df_energy %>% bind_rows(df_temp)
  
  # remove dummy dfs
  if (i == nrow(dfs)) rm(df_temp)
}

# remove the decimal places from the values
df_energy <- df_energy %>% 
  mutate(value = round(value, 0))

# list the available measures
df_energy %>% 
  distinct(type, measure, year) %>% 
  group_by(type, measure) %>% 
  mutate(from = min(year),
         to = max(year)) %>% 
  ungroup() %>% 
  distinct(type, measure, from, to) %>% 
  print(n = Inf) 

df_energy %>% 
  count(local_authority)

# -------------------------------------------------------------------------
# STEP 3.a
# Rise in meters

df_energy %>% filter(year == 2015) %>% count(type, year, measure) %>% print(n = Inf)

df_energy %>% 
  filter(country_or_region == 'England') %>% 
  filter(measure %in% c('number_of_meters_thousands_domestic','number_of_meters_thousands_all_domestic')) %>% 
  ggplot(aes(x=year, y = value, fill = type)) +
  geom_bar(position = 'stack', stat='identity') +
  geom_text(aes(label = value), vjust = -0.5, position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = label_number()) +
  ylab('Number of Meters') +
  labs(title = 'Total Domestic Meters : 2015 - 2021',
       caption = paste0('SOURCE : ',data_source))

df_energy %>% 
  filter(country_or_region == 'England') %>% 
  filter(measure %in% c('number_of_meters_thousands_domestic','number_of_meters_thousands_all_domestic')) %>% 
  group_by(type, year) %>% 
  summarise(total = sum(value)) %>% 
  ungroup() %>% 
  group_by(type) %>% 
  # this years total minus last years, as a percent of last years
  # total and lag(total are vectors)
  mutate(rise = fn_pcent(total - lag(total), lag(total)))
  

# -------------------------------------------------------------------------
# STEP 3.a
# gas/electricity annual fluctuations

df_energy %>% 
  filter(country_or_region == 'England') %>% 
  filter(measure %in% c('total_consumption_g_wh_domestic','total_consumption_g_wh_all_domestic')) %>%
  select(type, year, value) %>% 
  group_by(type) %>% 
  # this years total minus last years, as a percent of last years
  mutate(diff = fn_pcent(value - lag(value), lag(value))) %>% 
  mutate(diff = case_when(year == '2015'~ as.character(value), T ~ diff)) %>% 
  ggplot(aes(x=year, y = value, fill = type)) +
  geom_bar(position = 'stack', stat='identity', alpha = 0.6) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  geom_text(aes(label = diff), vjust = -0.5, position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = label_number()) +
  ylab('Consumption (GWh)') +
  labs(title = 'Total Domestic Consumption (GWh) : 2015 - 2021',
       caption = paste0('SOURCE : ',data_source))

