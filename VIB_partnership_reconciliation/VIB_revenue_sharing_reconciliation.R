## IMPORT LIBS & INPUT THE NEEDED ONE 
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
path = '~/vib/rev_sharing/Q2_2023'
options("scipen"=100, "digits"=4)

sh = 'https://docs.google.com/spreadsheets/d/1zqFK3-ohYvEeHwoLmetLMmWbMODzUFRqqjOULKrsZO8/edit#gid=235681388' #upgrade cards
options(gargle_oauth_email = "nhu.bui@trustingsocial.com")
gs4_auth(cache = ".secrets", email = 'nhu.bui@trustingsocial.com')
s = 'statistics'

# Read historical data
# op
duy_op_history =
  rbind(
    read_xlsx('/home/nhubui/vib/rev_sharing/2022-06/-- data_revenue_VIB_TS_2022.01-06.xlsx') %>% data.table() %>% clean_names(),
    read_xlsx('/home/nhubui/vib/rev_sharing/2022-06/-- data_revenue_VIB_TS_2021.xlsx') %>% data.table() %>% clean_names(),
    read_xlsx('/home/nhubui/vib/rev_sharing/2022-06/-- data_revenue_VIB_TS_2020.xlsx') %>% data.table() %>% clean_names(),
    read_xlsx('/home/nhubui/vib/rev_sharing/T07-T09_2022_update/--- data_revenue_VIB_TS_2022.07-09_1.xlsx', skip = 1) %>% data.table() %>% clean_names() %>% 
      select(-channel) %>%
      rename(active_date = active, channel = chan_final, cycle_create_update = cycle_create), #data gui lai version final 
    # read_xlsx('/home/nhubui/vib/rev_sharing/Q4_2022/----- OP_data_revenue_VIB_TS_2022.10-12.xlsx', skip = 1) %>% data.table() %>% clean_names() %>%
    #   mutate(active = as.Date(as.numeric(active), origin = "1899-12-30")) %>% mutate(active = as.POSIXct(active)) %>%
    #   rename(active_date = active, cycle_create_update = cycle_create),
    fill = T) # data Q3/2022 update lai theo channel dung nen dung lam ref de check channel data Q4/2022

# 2in1
duy_2in1_history = rbind(read_xlsx('/home/nhubui/vib/rev_sharing/2022-06/-- 2in1_data revenue_VIB_TS_2022.06.xlsx', sheet = 'data') %>% as.data.table() %>% clean_names(),
                         read_xlsx('/home/nhubui/vib/rev_sharing/T07-T09_2022_update/--- 2in1_data revenue_VIB_TS_2022.07-09_1.xlsx', skip = 2) %>% as.data.table() %>% clean_names() %>% 
                           select(-channel) %>% rename(channel = chan_final, create_date = acc_create, cycle_create_update = cycle_create), 
                         # read_xlsx('/home/nhubui/vib/rev_sharing/Q4_2022/----- 2in1_data revenue_VIB_TS_2022.10-12.xlsx', skip = 2) %>% as.data.table() %>% clean_names() %>%
                         #   mutate(active = as.Date(as.numeric(active), origin = "1899-12-30")) %>% 
                         #   mutate(active = as.POSIXct(active)) %>%
                         #   rename(create_date = acc_create, active_date = active, cycle_create_update = cycle_create),
                         fill = T)

# combine historical data:
duy_hist = 
  rbind(duy_op_history %>% mutate(product = 'op'),
        duy_2in1_history %>% mutate(product = '2in1'),
        fill = T) 

# read latest data

 # Q2-2023
q2_2023 = read_xlsx('/home/nhubui/vib/rev_sharing/Q2_2023/----- data_revenue_VIB_TS_2023.03-06.xlsx', skip = 1) %>% data.table() %>% clean_names()
all = q2_2023

# Q4-2022
q4_2022 = 
  rbind(
   read_xlsx('/home/nhubui/vib/rev_sharing/2022-10_2023-03/----- OP_data_revenue_VIB_TS_2022.10-12_update_1.xlsx', skip = 1) %>% data.table() %>% clean_names() %>% mutate(product = 'op')
  ,read_xlsx('/home/nhubui/vib/rev_sharing/2022-10_2023-03/----- 2in1_data revenue_VIB_TS_2022.10-12_update_1.xlsx', skip = 2) %>% data.table() %>% clean_names() %>% select(-acc_create) %>% mutate(product = '2in1')
  ,fill = T ) %>%
  data.table() %>% clean_names() %>%
  select(-unique_token) %>%
  rename(
     active_date = active
    ,cycle_create_update = cycle_create
    #,issued_date = acc_create
  )
  
q1_2023 = 
  read_xlsx('/home/nhubui/vib/rev_sharing/2022-10_2023-03/----- data_revenue_VIB_TS_2023.01-03_update_1.xlsx', skip = 1) %>% data.table() %>% clean_names() %>% 
  select(-unique_token) %>% 
  rename(
     active_date = active
    ,cycle_create_update = cycle_create
    ,annual_fee_received_1st_year_9 = annual_fee_received_1st_9
    ,annual_fee_received_2nd_year_9 = annual_fee_received_2nd_9
    ,annual_fee_received_3rd_year_9 = annual_fee_received_3rd_9
  )

# merge all the data
duy_all = rbind(q1_2023, q2_2023, q4_2022, duy_hist, fill = T) %>% mutate(status = tolower(status))

# Standardize the reports
all = rbind(q4_2022, q1_2023) %>% mutate(status = tolower(status))      
duy = q2_2023 %>% mutate(status = tolower(status)

##### CHECK DETAILED SHARING DATA

# CHECK ABNORMAL POINTS

# Cards not stored in latest data
duy_hist %>% filter(!unique_token_new %in% duy$unique_token_new, status == 'active') %>% distinct(unique_token_new) -> not_inc #121
duy_hist %>% 
  filter(!unique_token_new %in% duy$unique_token_new, status == 'active') %>% 
  group_by(unique_token_new) %>% 
  summarise(n = n(), max = max(cycle_report)) %>%
  filter(n > 2, substr(max,1,7) > '2022-01') -> not_inc_true

# Standardize CARD STATUS
duy = duy %>% mutate(status = tolower(status)) # same with df 'all'

distinct(duy_raw, channel)

duy_hist = duy_hist %>% mutate(status = tolower(status)) 

distinct(duy_raw, status)

duy = duy_raw %>% mutate(status_final = case_when(
  status %in% c('active', 'lost', 'pend close', 'cat3', 'cat2', 'risk', 'fraud') | is.na(status) ~ 'active',
  T ~ status)) 

distinct(duy, status_final)

# ANNUAL FEE 
# annual fee < 0 -> VIB reimburse annual fee for customers -> seperate by product + issued_cycle
duy %>% filter(annual_fee_received_1st_year_9 < 0 | annual_fee_received_2nd_year_9 < 0 | annual_fee_received_3rd_year_9 < 0) %>% distinct(unique_token_new) -> annual_waving

  
duy %>% filter(annual_fee_received_1st_year_9 < 0 | annual_fee_received_2nd_year_9 < 0 | annual_fee_received_3rd_year_9 < 0)  %>%
  group_by(cylce_create = substr(cycle_create_update,1,7), product) %>%
  summarise(n_cards = n(), 
            annual_fee = sum(annual_fee_received_1st_year_9, annual_fee_received_2nd_year_9, annual_fee_received_3rd_year_9, na.rm = T),
            avg_annual_fee = sum(annual_fee_received_1st_year_9, annual_fee_received_2nd_year_9, annual_fee_received_3rd_year_9, na.rm = T)/n()
           ) %>% ungroup() %>%
  adorn_totals() %>% 
  range_write(
    ss = sh, 
    sheet = 'Sheet29',
    range = 'a3')

duy %>% 
  filter(unique_token_new %in% annual_waving$unique_token_new) %>%
  arrange(desc(interest_income_8_4_5_6_7)) %>% 
  distinct(unique_token_new, .keep_all = T) %>% 
  group_by(cylce_create = substr(cycle_create_update,1,7), product) %>%
  summarise(n_cards = sum(interest_income_8_4_5_6_7 > 0)) %>% ungroup() %>%
  adorn_totals() %>% 
  range_write(
    ss = sh, 
    sheet = s,
    range = 'o47')

duy %>% filter(annual_fee_received_1st_year_9 < 0 | annual_fee_received_2nd_year_9 < 0 | annual_fee_received_3rd_year_9 < 0) %>% get_dupes(unique_token_new) 
# DUPLICATE REIMBURSED ANNUAL FEE OFF 2IN1 CARDS IN CONSECTIVE CYCLE (OCT-NOV/2022)  #499k 
duy %>% filter(annual_fee_received_1st_year_9 < 0 | annual_fee_received_2nd_year_9 < 0 | annual_fee_received_3rd_year_9 < 0) %>% get_dupes(unique_token_new) %>% distinct(unique_token_new) -> annual_waving_double

write_xlsx(annual_waving_double, paste0(path,'/waving_double_annual_fee.xlsx'))

# Check historical annual fee amount of those waived annual fee as mentioned above
duy_hist %>% filter(unique_token_new %in% annual_waving_double$unique_token_new) %>% arrange(unique_token_new) #toan bo la the OP -> tuc la chuyen tu the OP sang the 2in1
duy_hist %>% filter(unique_token_new %in% annual_waving_double$unique_token_new) %>% arrange(desc(annual_fee_received_9)) %>% distinct(unique_token_new, .keep_all = T) #the OP issued thang 

duy_hist %>% filter(unique_token_new %in% annual_waving_double$unique_token_new) %>% arrange(unique_token_new, cycle_report) %>% view() #dung la toan bo deu da dong phi thuong nien nhung chi 1 lan duy nhat 

# Check spending of waived annual fee cards
rbind(duy %>% 
        select(unique_token_new, cycle_create_update, annual_fee_received_9, annual_fee_received_1st_year_9, annual_fee_received_2nd_year_9, annual_fee_received_3rd_year_9, authorised_transaction_amount_14, average_balance_in_statement_period_16, sale_off_us_amount_11_1),
      duy_hist %>%
        select(unique_token_new, cycle_create_update, annual_fee_received_9, annual_fee_received_1st_year_9, annual_fee_received_2nd_year_9, annual_fee_received_3rd_year_9, authorised_transaction_amount_14, average_balance_in_statement_period_16, sale_off_us_amount_11_1)) %>%
  filter(unique_token_new %in% annual_waving$unique_token_new) %>% 
  group_by(unique_token_new, cycle_create_update) %>% 
  summarise(
    annual_fee = sum(annual_fee_received_9, na.rm = T),
    #avg_balance = sum(average_balance_in_statement_period_16, na.rm = T),
    sale_off_us = sum(sale_off_us_amount_11_1, na.rm = T), 
    authorise_amt = sum(authorised_transaction_amount_14, na.rm = T)) %>%
  range_write(
    ss = sh, 
    sheet = 'Sheet23',
    range = 'a1')

# Check whether a customer is waived annual fee 2 or more times
annual_waving_Q3.2022 = read_xlsx('~/vib/rev_sharing/T07-T09_2022/negative_annual_fee.xlsx') %>% data.table() %>% clean_names()

annual_waving %>% filter(unique_token_new %in% waving_annual_Q3.2022$unique_token_new) #0 

# DUP RECORDS
duy %>% get_dupes() %>% group_by(status_final) %>% summarise(n()) # should not have 'active'

# status_final   `n()`
# 1 close        20501
# 2 wo              33

# CHANGE SCHEME
# change scheme
change_scheme = 
  duy %>% distinct(unique_token_new, scheme) %>% group_by(unique_token_new) %>%
  summarise(count = n()) %>% filter(count > 1)

# Change product -> the whether the number make sense & cards info are consistent, most of these cases due to change statement cycle & combine 2 types of card
change_product = 
  duy %>% distinct(unique_token_new, product) %>% group_by(unique_token_new) %>%
  summarise(count = n()) %>% filter(count > 1) 

# cycle Q4_2022 there are 1,3k cards changing product (OP - 2in1) -> result of combine cards product
write_xlsx(change_product %>% distinct(unique_token_new), paste0(path,'/cards_change_product.xlsx'))

# Same product but change scheme
# Q1_2023: good
change_scheme_true = 
  duy %>% filter(unique_token_new %in% change_scheme$unique_token_new & !unique_token_new %in% change_product$unique_token_new & status_final == 'active') %>%
  distinct(unique_token_new, product) %>%  group_by(unique_token_new) %>%
  summarise(count = n()) %>% filter(count == 1) #check lai data update thi da dung

duy %>% filter(unique_token_new %in% change_scheme_true$unique_token_new) %>% distinct(product, channel, cycle_report, scheme, cycle_create_update)
#    chan_final scheme cycle_create cycle_report
# 1: ORGANIC_TS  15/85   2021-02-25   2022-09-25
# 2:      TELCO  15/85   2021-02-25   2022-09-25
# 3:       ZALO  15/85   2021-02-25   2022-09-25
# 4: TRUE_MONEY  15/85   2021-02-25   2022-09-25
# 5:      TELCO  30/70   2021-02-25   2022-08-25
# 6: ORGANIC_TS  30/70   2021-02-25   2022-08-25
# 7:       ZALO  30/70   2021-02-25   2022-08-25
# 8: TRUE_MONEY  30/70   2021-02-25   2022-08-25
# 9:      TELCO  30/70   2021-02-25   2022-07-25
# 10: ORGANIC_TS  30/70   2021-02-25   2022-07-25
# 11:       ZALO  30/70   2021-02-25   2022-07-25
# 12: TRUE_MONEY  30/70   2021-02-25   2022-07-25

rbind(duy_raw, duy_hist, fill = T) %>% filter(unique_token_new %in% change_scheme_true$unique_token_new) %>% view() 

duy_raw %>% filter(unique_token_new %in% change_scheme_true$unique_token_new) %>% distinct(unique_token_new) %>% write_xlsx('~/vib/rev_sharing/T07-T09_2022_update/change_scheme.xlsx')

# NEGATIVE SALE OFF-US AMOUNT
duy_raw %>% filter(sale_off_us_amount_11_1 < 0) %>% view()
neg_sale_off_us = duy_raw %>% filter(sale_off_us_amount_11_1 < 0) 

rbind(duy_raw, duy_hist, fill = T) %>% filter(unique_token_new %in% neg_sale_off_us$unique_token_new) %>% 
  group_by(unique_token_new) %>% 
  summarise(
    sale_off_us = sum(sale_off_us_amount_11_1, na.rm = T)
  ) %>% ungroup() %>% filter(sale_off_us < 0) -> check_sale_off_us

rbind(duy_raw, duy_hist, fill = T) %>% filter(unique_token_new %in% check_sale_off_us$unique_token_new) %>% view() #3 the OP bi ghi nhan < 0 ma trc do k co ghi nhan giao dich gi

duy_hist %>% filter(sale_off_us_amount_11_1 < 0) %>% view()

rbind(duy_raw, duy_hist, fill = T) %>% filter(unique_token_new == '5505343')  %>% view()

# OVERLIMIT FEE
# abnormal value
overlimit_abn = duy %>% filter(overlimit_fee_received_10 > 1000000 & status == 'active')

duy_all = rbind(duy %>% select(-unique_token) %>% mutate(active_date = as.POSIXct(active_date)), 
                duy_hist %>% select(-unique_token), 
                fill = T) 

duy_all %>%
  filter(unique_token_new %in% overlimit_abn$unique_token_new) %>% 
  group_by(unique_token_new) %>% 
  summarise(
    overlimit_fee = sum(overlimit_fee_received_10, na.rm = T),
    avg_balance = sum(average_balance_in_statement_period_16, na.rm = T),
    sale_off_us = sum(sale_off_us_amount_11_1, na.rm = T), 
    authorise_amt = sum(authorised_transaction_amount_14, na.rm = T)
  ) %>% view()

duy_hist %>% filter(unique_token_new %in% overlimit_abn$unique_token_new) %>%  arrange(desc(cycle_report)) %>% distinct(unique_token_new, .keep_all = T) %>% #count(status)
  filter(status == 'active') %>% view()


# Check COF 
# all is the df combined from reconciled reports
all %>% 
  mutate(d = monthDays(as.Date(cycle_report, '%Y-%m-01') - 30)) %>% 
  mutate(rate = cost_of_fund_17_16_interest_365_x_number_of_days_interest_is_refered_in_sheet_tham_so * 365 / (d * average_balance_in_statement_period_16)) %>% 
  count(billing_cycle = substr(cycle_report,1,7), product, rate) #10% 

change_scheme = 
  all %>% distinct(unique_token_new, scheme) %>% group_by(unique_token_new) %>%
  summarise(count = n()) %>% filter(count > 1)

all %>%
#rbind(base_of_truth, all, fill = T) %>% 
  filter(unique_token_new %in% change_scheme$unique_token_new, status != 'close') %>% arrange(unique_token_new, cycle_report) %>% 
  range_write(
    ss = 'https://docs.google.com/spreadsheets/d/17a1fK20EiG5fEEU7cW8xqgKDU7P3WoU3ozMBSQ4ehSw/edit#gid=1177765',
    sheet = 'Sheet38',
    range = 'a1',
    reformat = F
  )

all %>%
  filter(unique_token_new %in% change_scheme$unique_token_new) %>% 
  distinct(unique_token_new, channel) %>% 
  count(channel) %>% view()

rbind(base_of_truth, all, fill = T) %>% filter(unique_token_new %in% change_scheme$unique_token_new, status != 'close') %>%  count(channel)

duy_hist %>% filter(unique_token_new %in% change_scheme$unique_token_new) %>% arrange(unique_token_new, cycle_report) %>% view()

# Check interchange fee rate & processing fee rate 
all %>% mutate(fee = interchange_fee_12_1_11_1_interchange_fee_ratio_refered_in_sheet_tham_so/sale_off_us_amount_11_1) %>% count(fee) #ok
all %>% mutate(fee = processing_fee_15_14_processing_fee_ratio_refered_in_sheet_tham_so/authorised_transaction_amount_14) %>% count(fee) #ok 

# Check overlimit fee 
all %>% filter(overlimit_fee_received_10 > 1000000, status == 'active') %>% arrange(unique_token_new, cycle_report) %>% 
  range_write(
    ss = 'https://docs.google.com/spreadsheets/d/17a1fK20EiG5fEEU7cW8xqgKDU7P3WoU3ozMBSQ4ehSw/edit#gid=1177765',
    sheet = 'Sheet39',
    range = 'a1',
    reformat = F
  )

# Overlimit fee: wo + cat2/3 cards with high overlimit fee, if customers make payment enough to cover all the interest & fee, we can be shared the high amount
all %>% filter(overlimit_fee_received_10 > 1000000, status == 'active') %>% 
  get_dupes(unique_token_new) 

base_of_truth %>% filter(overlimit_fee_received_10 > 1000000, status == 'active') %>% distinct(unique_token_new) # 59 cards -> double -> 120 cards / 2 billing cycles

#SYSTEM MAINTAINANCE ABNORMAL
system_fee_weird = duy_raw %>% filter(!system_maintainance_fee_18 %in% c(-5000,0)) #%>% distinct(unique_token_new) #176 cases
system_fee_weird %>% count(type, cycle_create, cycle_report) #toan bo la the 2in1 -> cycle month T09/2022


# Wrap up final number

rev_cal = 
all %>% 
  mutate(scheme_ts = case_when(unique_token_new %in% change_scheme$unique_token_new ~ 0.3,
                               unique_token_new %in% check_scheme$unique_token_new ~ 0.3,
                               scheme == '30/70' ~ 0.3,
                               scheme == '15/85' ~ 0.15)) 
rev_cal = 
  all %>% 
  mutate(scheme_ts = case_when(scheme == '30/70' ~ 0.3,
                               scheme == '15/85' ~ 0.15)) 


# check number before share
rev_cal = 
  all %>% 
  mutate(scheme_ts = 1) 

rev_ts = 
  rev_cal %>% 
  filter(product == 'op') %>%
  mutate(
         annual_fee_1 = annual_fee_received_1st_9 * scheme_ts, 
         annual_fee_2 = annual_fee_received_2nd_9 * scheme_ts,
         annual_fee_3 = annual_fee_received_3rd_9 * scheme_ts) %>%
  group_by(cycle_report) %>% 
  summarise(cards_inforce = n_distinct(unique_token_new),
            interest_income = sum(interest_income_8_4_5_6_7* scheme_ts),
            annual_fee = sum(annual_fee_1, na.rm = T) + sum(annual_fee_2, na.rm = T) + sum(annual_fee_3, na.rm = T),
            overlimit_fee =  sum(overlimit_fee_received_10 * scheme_ts, na.rm = T), 
            inter_change_fee =  sum(interchange_fee_12_1_11_1_interchange_fee_ratio_refered_in_sheet_tham_so * scheme_ts),
            fee_from_h2h = sum(fee_from_h2h_transaction_12_2_11_2_1_1_percent * scheme_ts),
            processing_fee =  sum(processing_fee_15_14_processing_fee_ratio_refered_in_sheet_tham_so * scheme_ts),
            cost_of_fund =  sum(cost_of_fund_17_16_interest_365_x_number_of_days_interest_is_refered_in_sheet_tham_so * scheme_ts),
            maintaince_fee = sum(system_maintainance_fee_18 * scheme_ts), 
            write_off_fee  =  sum(provision_cost_for_write_off_19 * scheme_ts),
            scope_2 = sum(cost_organic_ts_non_viettel),
            telco_cost = sum(telco_cost_20, na.rm = T),
            anr = sum(average_balance_in_statement_period_16, na.rm=T))

row_name =  colnames(rev_ts) 
rev_ts = rev_ts %>% t() %>% data.table()  
names(rev_ts)= as_vector(rev_ts[1,]) 
rev_ts = rev_ts[2:nrow(rev_ts),]
rownames(rev_ts) = row_name[2:length(row_name)]
rev_ts = rownames_to_column(rev_ts)

# 2IN1 
rev_ts = 
  rev_cal %>% 
  filter(product == '2in1') %>%
  mutate(annual_fee_1 = case_when(channel == 'GRAB' ~ annual_fee_received_1st_9, TRUE ~ annual_fee_received_1st_9 * scheme_ts),
         annual_fee_2 = case_when(channel == 'GRAB' ~ 0, TRUE ~ annual_fee_received_2nd_9 * scheme_ts),
         annual_fee_3 = annual_fee_received_3rd_9 * scheme_ts) %>%
  group_by(cycle_report) %>% 
  summarise(cards_inforce = n_distinct(unique_token_new),
            interest_income = sum(interest_income_8_4_5_6_7* scheme_ts),
            annual_fee = sum(annual_fee_1, na.rm = T) + sum(annual_fee_2, na.rm = T) + sum(annual_fee_3, na.rm = T),
            overlimit_fee =  sum(overlimit_fee_received_10 * scheme_ts, na.rm = T), 
            inter_change_fee =  sum(interchange_fee_12_1_11_1_interchange_fee_ratio_refered_in_sheet_tham_so * scheme_ts),
            fee_from_h2h = sum(fee_from_h2h_transaction_12_2_11_2_1_1_percent * scheme_ts),
            processing_fee =  sum(processing_fee_15_14_processing_fee_ratio_refered_in_sheet_tham_so * scheme_ts),
            cost_of_fund =  sum(cost_of_fund_17_16_interest_365_x_number_of_days_interest_is_refered_in_sheet_tham_so * scheme_ts),
            maintaince_fee = sum(system_maintainance_fee_18 * scheme_ts), 
            write_off_fee  =  sum(provision_cost_for_write_off_19 * scheme_ts),
            scope_2 = sum(cost_organic_ts_non_viettel),
            telco_cost = sum(telco_cost_20),
            total_anr = sum(average_balance_in_statement_period_16,na.rm=T))

# lech so the bi change scheme
vib = read_xlsx('~/vib/rev_sharing/lech_.xlsx') %>% data.table() %>% clean_names()
anti_join(vib, change_scheme, by = 'unique_token_new')

vib %>% filter(unique_token_new %in% change_scheme$unique_token_new)
vib %>% distinct(unique_token_new)

# check lai phan annual fee cua the 2IN1
# so tong: dung; GRAB: dung; tung group organic: dung.
rev_cal %>%
  filter(product == '2in1') %>%
  mutate(
    group = case_when(channel == 'ORGANIC_VIB' ~ 'org_vib', 
                      channel == 'GRAB' ~ 'grab',
                      T ~ 'org_ts'
                      )
  ) %>% 
  group_by(billing_cycle = substr(cycle_report,1,7), group) %>%
  summarise(ANF1 = sum(annual_fee_received_1st_year_9, na.rm = T),
            ANF2 = sum(annual_fee_received_2nd_year_9, na.rm = T),
            ANF3 = sum(annual_fee_received_3rd_year_9, na.rm = T)
) %>% view()

rev_cal %>%
  filter(product == '2in1') %>%
  mutate(
    group = case_when(channel == 'ORGANIC_VIB' ~ 'org_vib', 
                      channel == 'GRAB' ~ 'grab',
                      T ~ 'org_ts'
    )
  ) %>% #count(group, scheme_ts)  #org_ts ma scheme ghi nhan la 15%
  filter(group == 'org_ts', scheme_ts == 0.15) %>% distinct(unique_token_new, .keep_all = T) -> check_scheme

check_scheme %>% count(substr(cycle_create_update,1,7))

change_scheme %>% filter(unique_token_new %in% check_scheme$unique_token_new)

check_scheme %>% arrange(desc(annual_fee_received_1st_year_9)) %>% head()

# tong hop rev before share
# OP
rev_ts = 
  rev_cal %>% 
  filter(product == 'op') %>%
  group_by(cycle_report) %>% 
  summarise(n_cards_inforce = n_distinct(unique_token_new),
            interest_income = sum(interest_income_8_4_5_6_7),
            annual_fee =  sum(annual_fee_received_9, na.rm=T) + sum(annual_fee_received_1st_year_9, na.rm =T) + sum(annual_fee_received_2nd_year_9, na.rm=T) + sum(annual_fee_received_3rd_year_9, na.rm=T),
            overlimit_fee =  sum(overlimit_fee_received_10 ), 
            inter_change_fee =  sum(interchange_fee_12_1_11_1_interchange_fee_ratio_refered_in_sheet_tham_so ),
            fee_from_h2h = sum(fee_from_h2h_transaction_12_2_11_2_1_1_percent ),
            processing_fee =  sum(processing_fee_15_14_processing_fee_ratio_refered_in_sheet_tham_so ),
            cost_of_fund =  sum(cost_of_fund_17_16_interest_365_x_number_of_days_interest_is_refered_in_sheet_tham_so ),
            maintaince_fee = sum(system_maintainance_fee_18 ), 
            write_off_fee  =  sum(provision_cost_for_write_off_19 ),
            scope_2 = sum(cost_organic_ts_non_viettel),
            telco_cost = sum(telco_cost_20))

# 2in1
rev_ts = 
  rev_cal %>% 
  filter(product == '2in1') %>%
  group_by(cycle_report) %>% 
  summarise(n_cards_inforce = n_distinct(unique_token_new),
            interest_income = sum(interest_income_8_4_5_6_7),
            annual_fee =  sum(annual_fee_received_9, na.rm=T) + sum(annual_fee_received_1st_year_9, na.rm =T) + sum(annual_fee_received_2nd_year_9, na.rm=T) + sum(annual_fee_received_3rd_year_9, na.rm=T),
            overlimit_fee =  sum(overlimit_fee_received_10 ), 
            inter_change_fee =  sum(interchange_fee_12_1_11_1_interchange_fee_ratio_refered_in_sheet_tham_so ),
            fee_from_h2h = sum(fee_from_h2h_transaction_12_2_11_2_1_1_percent ),
            processing_fee =  sum(processing_fee_15_14_processing_fee_ratio_refered_in_sheet_tham_so ),
            cost_of_fund =  sum(cost_of_fund_17_16_interest_365_x_number_of_days_interest_is_refered_in_sheet_tham_so ),
            maintaince_fee = sum(system_maintainance_fee_18 ), 
            write_off_fee  =  sum(provision_cost_for_write_off_19 ),
            scope_2 = sum(cost_organic_ts_non_viettel),
            telco_cost = sum(telco_cost_20))