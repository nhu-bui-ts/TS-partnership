library(data.table)
library(lubridate)
library(magrittr)
library(tidyverse)
library(stringr)
library(RPostgreSQL)
library(stringi)
library(janitor)
library(readxl)
library(writexl)
library(dplyr)
library(openxlsx)
library(googlesheets4)
library("Hmisc") #pkg get number of days in a calendar month

rm(list=ls())
#path = '~/vib/rev_sharing/T07-T09_2022_update'
path = '~/vib/rev_sharing/Q4_2022'
options("scipen"=100, "digits"=4)
gs4_auth(token=readRDS('/home/nhubui/googlesheets_token.rds'))
sh = 'https://docs.google.com/spreadsheets/d/1EfE2l6QzE27AHnAjaDo811TnC3etRILMyDOUCQwgmgo/edit#gid=1999513076'

##### READ FILE (LATEST BATCH + HISTORICAL DATA)
duy_op_raw =
  read_xlsx(paste0(path,'/----- OP_data_revenue_VIB_TS_2022.10-12.xlsx'), skip = 1) %>% data.table() %>% clean_names() %>%
  mutate(active = as.Date(as.numeric(active), origin = "1899-12-30"))

duy_op_raw %>% head(5) %>% view()

duy_2in1_raw = read_xlsx(paste0(path,'/----- 2in1_data revenue_VIB_TS_2022.10-12.xlsx'), skip = 2) %>% as.data.table() %>% clean_names() %>%
  mutate(active = as.Date(as.numeric(active), origin = "1899-12-30"))

duy_op_raw = read_xlsx(paste0(path,'/--- data_revenue_VIB_TS_2022.07-09_1.xlsx'), skip = 1) %>% data.table() %>% clean_names()
duy_2in1_raw = read_xlsx(paste0(path,'/--- 2in1_data revenue_VIB_TS_2022.07-09_1.xlsx'), skip = 2) %>% as.data.table() %>% clean_names()

# Combine 2 files
duy_raw = rbind(duy_op_raw %>% mutate(type = 'op'), duy_2in1_raw %>% mutate(type = '2in1'), fill = T) %>% mutate(status = tolower(status)) 

# Standardize cards type
duy = duy_raw %>% mutate(status_final = case_when(status %in% c('active', 'lost', 'pend close', 'cat3', 'cat2', 'risk', 'fraud') | is.na(status) ~ 'active',
                                                  T ~ status)) 

# Read historical data 
duy_op_history =
  rbind(
    read_xlsx('/home/nhubui/vib/rev_sharing/2022-06/-- data_revenue_VIB_TS_2022.01-06.xlsx') %>% data.table() %>% clean_names(),
    read_xlsx('/home/nhubui/vib/rev_sharing/2022-06/-- data_revenue_VIB_TS_2021.xlsx') %>% data.table() %>% clean_names(),
    read_xlsx('/home/nhubui/vib/rev_sharing/2022-06/-- data_revenue_VIB_TS_2020.xlsx') %>% data.table() %>% clean_names(),
    read_xlsx('/home/nhubui/vib/rev_sharing/T07-T09_2022_update/--- data_revenue_VIB_TS_2022.07-09_1.xlsx', skip = 1) %>% data.table() %>% clean_names(),
    fill = T)

duy_2in1_history  = 
  rbind(read_xlsx('/home/nhubui/vib/rev_sharing/2022-06/-- 2in1_data revenue_VIB_TS_2022.06.xlsx', 
                              sheet = 'data') %>% as.data.table() %>% clean_names(),
        read_xlsx('/home/nhubui/vib/rev_sharing/T07-T09_2022_update/--- 2in1_data revenue_VIB_TS_2022.07-09_1.xlsx', skip = 2) %>% as.data.table() %>% clean_names(),
        fill = T)

duy_hist = rbind(duy_op_history %>% mutate(type = 'op'), duy_2in1_history %>% mutate(type = '2in1'), fill = T) %>% mutate(status = tolower(status)) %>%
  mutate(status_final = case_when(
    status %in% c('active', 'lost', 'pend close', 'cat3', 'cat2', 'risk', 'fraud') | is.na(status) ~ 'active',
    T ~ status)) 

# Thuc thu annual fee / eligible annual fee
rbind(duy, duy_hist, fill = T) %>% 
  filter(substr(active,1,4) != '1899' | substr(active_date,1,4) != '1899') %>% distinct(unique_token_new) #116,538 - 246

a = rbind(duy, duy_hist, fill = T) %>% 
  filter(annual_fee_received_9 > 0 | annual_fee_received_1st_year_9 > 0 | annual_fee_received_2nd_year_9 > 0) %>% 
  distinct(unique_token_new) #107,925 (inc reimburse annual fee promotion of VIB)
b = rbind(duy, duy_hist, fill = T) %>% 
  filter(annual_fee_received_9 < 0 | annual_fee_received_1st_year_9 < 0 | annual_fee_received_2nd_year_9 < 0) %>% 
  distinct(unique_token_new) #3,833

a %>% filter(!unique_token_new %in% b$unique_token_new) 

# 2nd year only
rbind(duy, duy_hist, fill = T) %>% 
  filter(substr(active,1,4) != '1899' | substr(active_date,1,4) != '1899') %>% 
  filter(substr(active,1,7) <= '2021-08' | substr(active_date,1,7) <= '2021-08') %>% 
  distinct(unique_token_new) #52,672

rbind(duy, duy_hist, fill = T) %>% 
  filter(substr(active,1,4) != '1899' | substr(active_date,1,4) != '1899') %>% 
  filter(substr(active,1,7) <= '2021-08' | substr(active_date,1,7) <= '2021-08') %>% view()

c = rbind(duy, duy_hist, fill = T) %>% 
  filter(substr(active,1,4) != '1899' | substr(active_date,1,4) != '1899') %>% 
  filter(substr(active,1,7) <= '2021-09' | substr(active_date,1,7) <= '2021-09') %>%
  filter(as.numeric(difftime(as.Date(cycle_report), as.Date(active), units = 'days')) >= 300 | as.numeric(difftime(as.Date(cycle_report), as.Date(active_date), units = 'days')) >= 300) %>% 
  filter(annual_fee_received_9 > 0 | annual_fee_received_2nd_year_9 > 0) %>%
  distinct(unique_token_new) #43,592

c %>% filter(unique_token_new %in% b$unique_token_new)  

##### Rbind 2 batch data + format active date
names(duy)
names(duy_hist)

duy_all = 
rbind(
  duy %>% select(-c(unique_token, scheme, acc_create)) %>% rename(active_date = active) %>% mutate(active_date = as.POSIXct(active_date)), 
  duy_hist %>% select(-c(unique_token, chan_final, scheme, create_date, acc_create, cycle_create)) %>% rename(cycle_create = cycle_create_update), 
  fill = T) #%>% head() %>% view()

# Correct active date format
duy_all[substr(active_date,1,4) == '1899']$active_date = NA

##### Lay port the ACTIVE
duy_active = duy_all %>% filter(status_final != 'close')

##### Flag cycle active cho the 
cycle_active_OP = function(active_date){
  month = strftime(active_date, format='%Y-%m')
  previous_month = strftime(floor_date(active_date, unit = 'month') - 1, format = '%Y-%m')
  next_month = strftime(ceiling_date(active_date, unit = 'month'), format = '%Y-%m')
  cycle = case_when(active_date <= '2020-03-15' & day(active_date) <= 15 ~ paste0(month, '-15'),
                   active_date <= '2020-03-15' & day(active_date) > 15 ~ paste0(next_month, '-15'),
                   active_date > '2020-03-15' & active_date <= '2020-04-25' ~ paste0('2020-04-25'),
                   active_date > '2020-04-25' & active_date <= '2020-07-25' & day(active_date) <= 25 ~ paste0( month, '-25'),
                   active_date > '2020-04-25' & active_date <= '2020-07-25' & day(active_date) > 25 ~ paste0(next_month, '-25'),
                   active_date > '2020-07-25' & day(active_date) < 25 ~ paste0(month, '-25'),
                   active_date > '2020-07-25' & day(active_date) >= 25 ~ paste0(next_month, '-25')
  )
  return (cycle)}

cycle_active_2IN1 = function(active_date){
  month = strftime(active_date, format='%Y-%m')
  previous_month = strftime(floor_date(active_date, unit='month')-1, format='%Y-%m')
  next_month = strftime(ceiling_date(active_date, unit='month'), format= '%Y-%m')
  cycle = case_when(day(active_date) < 10 ~ paste0( month, '-10'),
                   day(active_date) >= 10 ~ paste0(next_month, '-10')
  )
  return (cycle)}

duy_active_cycle = 
rbind(
  duy_active %>% filter(type == 'op') %>%  mutate(cycle_active = cycle_active_OP(active_date)),
  duy_active %>% filter(type == '2in1') %>%  mutate(cycle_active = cycle_active_2IN1(active_date)))

duy_active_cycle %>% filter(is.na(active_date), !is.na(annual_fee_received_9))
duy_active_cycle %>% distinct(status_final, !is.na(annual_fee_received_9))

duy_active_cycle %>% get_dupes(cycle_report, unique_token_new) %>% view() #the phu 

##### Flag DOB 
mob = function(m1, m2){
  y1 = as.numeric(substr(m1, 1,4))
  m1 =  as.numeric(substr(m1, 6,7))
  
  y2 =  as.numeric(substr(m2, 1,4))
  m2 =  as.numeric(substr(m2, 6,7))
  
  diff = (y2-y1)*12 + (m2-m1)
  
  return(diff)
}

duy_mob = duy_active_cycle %>% mutate(mob = mob(cycle_active, cycle_report))
duy_mob %>% filter(is.na(active_date), as.numeric(annual_fee_received_9) > 0) %>% distinct(unique_token_new) #207 cards -> accept

# Check thu xem ky thu phi annual dang hoat dong nhu the nao
duy_mob %>% filter(cycle_create < '2021-08-01') %>% arrange(desc(unique_token_new), cycle_report) %>% head(10000) %>% view()
duy_mob %>% filter(mob == 0) %>% distinct(type, status_final) #new, active
#duy_mob %>% filter(type == 'op', substr(cycle_create,9,10) == '15')


#### Check metrics 
# So luong cards in force theo aging
duy_mob %>% 
  filter(mob > 0) %>% 
  group_by(cylce_report = substr(cycle_report,1,7), active_mob =  mob) %>% 
  summarise(n = n()) %>% 
  range_write(ss = sh, sheet = 'Cards in force', range = 'A1000')

duy_mob %>%  filter(is.na(mob) | mob < 0) %>% view()

# So luong the thu duoc phi thuong nien tai tung aging
duy_mob %>% 
  filter(mob > 0,
         as.numeric(annual_fee_received_9) > 0 | as.numeric(annual_fee_received_1st_year_9) > 0 | as.numeric(annual_fee_received_2nd_year_9) > 0 | as.numeric(annual_fee_received_3rd_year_9) > 0) %>% #distinct(status_final)
  group_by(cylce_report = substr(cycle_report,1,7), active_mob = mob) %>% 
  summarise(n = n()) %>% view()
  range_write(ss = sh, sheet = 'Annual fee', range = 'A1000')

duy_mob %>% 
  filter(mob > 0,
         as.numeric(annual_fee_received_9) > 0 | as.numeric(annual_fee_received_1st_year_9) > 0 | as.numeric(annual_fee_received_2nd_year_9) > 0 | as.numeric(annual_fee_received_3rd_year_9) > 0)  %>% 
  get_dupes(unique_token_new, cycle_report)#0 
 
#### Quan sat so luong 
duy_mob %>% count(annual_fee_received_9) %>% view() #OP 
duy_mob %>% count(annual_fee_received_1st_year_9) %>% view() #2in1 
duy_mob %>% count(annual_fee_received_2nd_year_9) %>% view() #2in1 
duy_mob %>% count(annual_fee_received_3rd_year_9) %>% view() #2in1 gan nhu chua du thoi gian thu phi, chi co 1 the

duy_mob %>% filter(annual_fee_received_9 == '499000') %>% view()

# Xem viec hoan phi thuong nien cho KH dien ra tu cycle nao 
duy_mob %>% filter(as.numeric(annual_fee_received_9) < 0 | as.numeric(annual_fee_received_1st_year_9) < 0 | as.numeric(annual_fee_received_2nd_year_9) < 0 | as.numeric(annual_fee_received_3rd_year_9) < 0) %>% 
  distinct(cycle_report) #T08, T09

# Quan sat tap duoc waving fee
duy_mob %>% 
  filter(mob > 0,
         as.numeric(annual_fee_received_9) < 0 | as.numeric(annual_fee_received_1st_year_9) < 0 | as.numeric(annual_fee_received_2nd_year_9) < 0 | as.numeric(annual_fee_received_3rd_year_9) < 0) %>% #distinct(status_final)
  group_by(cylce_report = substr(cycle_report,1,7), active_mob = mob) %>% 
  summarise(n = n()) %>% view()
  range_write(ss = sh, sheet = 'Annual fee', range = 'F1000')

# Ty le tra du phi thuong nien
duy_mob %>% 
  filter(mob > 0,
         as.numeric(annual_fee_received_9) %in% c(399000, 499000) | as.numeric(annual_fee_received_1st_year_9) == 599000 | as.numeric(annual_fee_received_2nd_year_9) == 599000 | as.numeric(annual_fee_received_3rd_year_9) == 599000) %>% #distinct(status_final)
  group_by(cylce_report = substr(cycle_report,1,7), active_mob = mob) %>% 
  summarise(n = n()) %>% view()
  range_write(ss = sh, sheet = 'Annual fee', range = 'K1000')


hist(duy_mob$annual_fee_received_9)




