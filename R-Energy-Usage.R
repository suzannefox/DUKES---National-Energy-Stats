library(tidyverse)
library(janitor)
library(tidyxl)
library(scales)
library(here)

data_source <- 'https://www.gov.uk/government/publications/regional-energy-data-guidance-note'

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
    pivot_longer(-c('year','type','code','country_or_region','local_authority','notes')) 
  
  # make a dummy consolidated output df
  if (i == 1) df_energy <- df_temp %>% slice(0)
  df_energy <- df_energy %>% bind_rows(df_temp)
  
  # remove dummy dfs
  if (i == nrow(dfs)) rm(df_temp)
}

# remove the decimal places from the values
df_energy <- df_energy %>% mutate(value = round(value, 0))

# -------------------------------------------------------------------------
# STEP 3.
# Do some EDA

df_energy %>% filter(year == 2015) %>% count(type, year, name) %>% print(n = Inf)

df_energy %>% 
  filter(country_or_region == 'England') %>% 
  filter(name %in% c('number_of_meters_thousands_domestic','number_of_meters_thousands_all_domestic')) %>% 
  ggplot(aes(x=year, y = value, fill = type)) +
  geom_bar(position = 'stack', stat='identity') +
  geom_text(aes(label = value), vjust = -0.5, position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = label_number()) +
  ylab('Number of Meters') +
  labs(title = 'Total Domestic Meters : 2015 - 2021',
       caption = paste0('SOURCE : ',data_source))

df_energy %>% 
  filter(country_or_region == 'England') %>% 
  filter(name %in% c('total_consumption_g_wh_domestic','total_consumption_g_wh_all_domestic')) %>% 
  ggplot(aes(x=year, y = value, fill = type)) +
  geom_bar(position = 'stack', stat='identity') +
  geom_text(aes(label = value), vjust = -0.5, position = position_stack(vjust = .5)) +
  scale_y_continuous(labels = label_number()) +
  ylab('Consumption (GWh)') +
  labs(title = 'Total Domestic Consumption (GWh) : 2051 - 2021',
       caption = paste0('SOURCE : ',data_source))
