
# 0. Context: deep dive why the interest income hike in recent months, consider 2 big scenarios: 
#             (1) the higher no. customers having status high/very high IR
#             (2) more paid interest 

# Inputs of file: 
options(gargle_oauth_email = "nhu.bui@trustingsocial.com")
gs4_auth(cache = ".secrets", email = 'nhu.bui@trustingsocial.com')
file = 'https://docs.google.com/spreadsheets/d/17a1fK20EiG5fEEU7cW8xqgKDU7P3WoU3ozMBSQ4ehSw/edit#gid=1792775471' #22Q4 & 23Q1
file = 'https://docs.google.com/spreadsheets/d/1lwIdA1jlqmMOH2r9fU3Ch7J2W-vQhY8hu9GhqxYj7xQ/edit#gid=1792775471' #23Q2
sh = '5. Interest income analysis'

# Combine all data to df 'duy_all' 

#### Starting with the No (1) calculate the IR of 

# 1. Interest rate by each cards cross cycle

# (1) Separate into many dfs: oct-2022 to Mar-2023
month = '2022-06'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> jun_2022

month = '2022-07'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> jul_2022

month = '2022-08'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> aug_2022

month = '2022-09'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> sep_2022

month = '2022-10'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> oct_2022

month = '2022-11'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> nov_2022

month = '2022-12'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> dec_2022

month = '2023-01'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> jan_2023

month = '2023-02'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> feb_2023

month = '2023-03'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> mar_2023

month = '2023-04'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> apr_2023

month = '2023-05'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> may_2023

month = '2023-06'
duy_all %>% filter(substr(cycle_report,1,7) == month) -> jun_2023

# (2) Mapping ANR of the previous billing cycle to the current Interest Income
jul_2022_mapping = 
  jul_2022 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    jun_2022 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

aug_2022_mapping = 
  aug_2022 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    jul_2022 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

sep_2022_mapping = 
  sep_2022 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    aug_2022 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

oct_2022_mapping = 
oct_2022 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    sep_2022 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

nov_2022_mapping = 
  rbind(
    nov_2022 %>% filter(product == 'op') %>% 
      select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
      arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
      filter(interest_income > 0) %>%
      left_join(
        sep_2022 %>% filter(product == 'op') %>% 
          arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
          select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
                 previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
      )
    ,
   nov_2022 %>% 
     filter(product == '2in1') %>%
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    oct_2022 %>% filter(product == '2in1') %>%
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )
  )

dec_2022_mapping = 
  dec_2022 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    nov_2022 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

jan_2023_mapping = 
  jan_2023 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    dec_2022 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

feb_2023_mapping = 
  feb_2023 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    jan_2023 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

mar_2023_mapping = 
  mar_2023 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    feb_2023 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

apr_2023_mapping = 
  apr_2023 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    mar_2023 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

may_2023_mapping = 
  may_2023 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    apr_2023 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

jun_2023_mapping = 
  jun_2023 %>% 
  select(cycle_report, unique_token_new, product, interest_income = interest_income_8_4_5_6_7) %>% 
  arrange(desc(interest_income)) %>% distinct(unique_token_new, .keep_all = T) %>%
  filter(interest_income > 0) %>%
  left_join(
    may_2023 %>% 
      arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, .keep_all = T) %>%
      select(unique_token_new, previous_anr = average_balance_in_statement_period_16,
             previous_interest_income = interest_income_8_4_5_6_7), by = 'unique_token_new'
  )

# (3) Combine all of them & calculate the rate
 final_cal = rbind(jul_2022_mapping, aug_2022_mapping, sep_2022_mapping, oct_2022_mapping,
                   nov_2022_mapping, dec_2022_mapping, jan_2023_mapping, feb_2023_mapping, mar_2023_mapping,
                   apr_2023_mapping, may_2023_mapping, jun_2023_mapping, fill = T) %>% 
                   mutate(int = interest_income * 12 / previous_anr)

  head(final_cal)                     
  final_cal %>% get_dupes(cycle_report, unique_token_new) #0 good
  
  hist(final_cal$int, breaks = 2)
  final_cal %>% distinct(int) %>% view()
  final_cal %>% filter(int < 0) %>% view()
  final_cal %>% filter(is.na(int)) %>% view()
  
# duy_all %>% filter(interest_income_8_4_5_6_7 < 0, status == 'active') %>% view() #small rate (eg: close card)
  
  duy_all %>% filter(unique_token_new == '3656096') %>% arrange(unique_token_new, cycle_report) %>% view()
  
# (4) Flag interest every month with 5 statuses of interest (very low -> very high)
  final_cal_flag = final_cal %>% 
                   mutate(group = case_when( int < 0 ~ 'exclude',
                                             int > 0 & int <= 0.295 ~ '1. Very low',
                                             int > 0.295 & int <= 0.345 ~ '2. Low',
                                             int > 0.345 & int <= 0.355 ~ '3. Medium',
                                             int > 0.355 & int <= 0.385 ~ '4. High',
                                             int > 0.385 & int <= 0.395 ~ '5. Very high',
                                           # int > 0.395 & int <= 0.369 ~ 'Blended',
                                             (int > 0.395 & int < 0.5925) & previous_interest_income > 0 ~ '6. Overdue',
                                             int > 0.395 & previous_interest_income == 0 ~ '7. 1st time Revolvers',
                                             int > 0.5925 ~ 'abnormal',
                                             T ~ 'check'))
  
  final_cal_flag %>% count(group) 
  final_cal_flag %>% filter(group == 'abnormal') %>% view() #nhung cases ma lai thu duoc ky nay la tu ANR cua nhieu ky truoc do 
  
  final_cal_flag %>% group_by(billing_month = substr(cycle_report,1,7), group) %>% 
    summarise(n = n()) %>% 
    range_write(
      ss = file, sheet = sh, range = 'a254', reformat = F, col_names = F
    )
  
  # tao them 1 nhom moi co Interest 150% max interest, va co interest income of previous billing month == 0 thanh '7. 1st time Revolvers'
  final_cal_flag %>% group_by(billing_month = substr(cycle_report,1,7), group) %>% 
    summarise(n = n()) %>% 
    range_write(
      ss = file, sheet = sh, range = 'bb200', reformat = F
    )
  
  # med monthly nominal int by billing month
  final_cal_flag %>% 
    #filter(!group %in% c('abnormal', 'check')) %>%
    group_by(billing_month = substr(cycle_report,1,7)) %>% 
    summarise(n = median(int/12, na.rm = T)) %>% view()
  
  final_cal_flag %>% 
    filter(!group %in% c('abnormal', 'check')) %>%
    group_by(billing_month = substr(cycle_report,1,7)) %>% 
    summarise(n = median(int/12, na.rm = T)) %>% view()
  
  # %paid interest 
  duy_all %>%
    arrange(desc(average_balance_in_statement_period_16)) %>% 
    distinct(cycle_report, unique_token_new, .keep_all = T) %>% 
    filter(!status %in% c('Close', 'close')) %>%
    group_by(billing_month = substr(cycle_report,1,7)) %>% 
    summarise(cards_inforce = n_distinct(unique_token_new)) %>% data.table() %>% 
    left_join(
      duy_all %>% arrange(desc(interest_income_8_4_5_6_7)) %>% distinct(unique_token_new, cycle_report, .keep_all = T) %>% 
        filter(interest_income_8_4_5_6_7 > 0) %>% 
        group_by(billing_month = substr(cycle_report,1,7)) %>% 
        summarise(n_paid_int = n()) %>% data.table(),
      by = 'billing_month') %>% 
    data.table() %>%
    view()
  
  
  # %paid interest exc 'abnormal' and 'check'
  duy_all %>% 
    group_by(billing_month = substr(cycle_report,1,7)) %>% 
    summarise(cards_inforce = n_distinct(unique_token_new)) %>% data.table() %>% 
    left_join(
      duy_all %>% arrange(desc(interest_income_8_4_5_6_7)) %>% distinct(unique_token_new, cycle_report, .keep_all = T) %>% 
        filter(interest_income_8_4_5_6_7 > 0, !unique_token_new %in% final_cal_flag[group %in% c('abnormal', 'check'),]$unique_token_new) %>% 
        group_by(billing_month = substr(cycle_report,1,7)) %>% 
        summarise(n_paid_int = n()) %>% data.table(),
      by = 'billing_month') %>% 
    data.table() %>%
    view()
  
  
  
#### (2.1) Calculate the ANR
  sample = 
  duy_all %>% 
    filter(substr(cycle_report,1,7) >= '2022-01') %>% 
    mutate(user_group = case_when(interest_income_8_4_5_6_7 > 0 ~ 'Revolvers', T ~ 'Transactors'))

  # sum ANR of all port
  sample %>% 
    group_by(user_group, billing_month = substr(cycle_report,1,7)) %>%
    summarise(total_ANR = sum(average_balance_in_statement_period_16)/10^6) %>% 
    range_write(
      ss = file, sheet = sh, range = 'f200', reformat = F
    )

  # AVG ANR of 'activated port'
  sample %>% distinct(status)    
  sample = sample %>% mutate(status = tolower(status))
  
  sample %>% 
    arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, cycle_report, .keep_all = T) %>% 
    filter(!status %in% c('close', 'wo')) %>% 
    group_by(user_group, billing_month = substr(cycle_report,1,7)) %>% 
    summarise(
       total_anr = sum(average_balance_in_statement_period_16)/10^6
      ,n_cards = n_distinct(unique_token_new)
      ) %>% 
    range_write(
      ss = file, sheet = sh, range = 'k200', reformat = F
    )

#### (2.2) Calculate the Spending    
  # sum ANR of all port
  sample %>% 
    group_by(user_group, billing_month = substr(cycle_report,1,7)) %>%
    summarise(total_spending = sum(authorised_transaction_amount_14)/10^6) %>% 
    range_write(
      ss = file, sheet = sh, range = 'q200', reformat = F
    )
  
  # AVG spending of 'activated port'
  sample %>% distinct(status)    
  sample %>% 
    arrange(desc(authorised_transaction_amount_14)) %>% distinct(unique_token_new, cycle_report, .keep_all = T) %>% 
    filter(!status %in% c('close', 'wo')) %>% 
    group_by(user_group, billing_month = substr(cycle_report,1,7)) %>% 
    summarise(
      total_spending = sum(authorised_transaction_amount_14)/10^6
      ,n_cards = n()
    ) %>% 
    range_write(
      ss = file, sheet = sh, range = 'v200', reformat = F
    )
  
#### (2.3) Calculate the COF theo nhom T va R 
   
  # sum COF of all port
  sample %>% 
    group_by(user_group, billing_month = substr(cycle_report,1,7)) %>%
    summarise(total_COF = sum(-cost_of_fund_17_16_interest_365_x_number_of_days_interest_is_refered_in_sheet_tham_so)/10^6) %>% 
    range_write(
      ss = file, sheet = sh, range = 'ap200', reformat = F
    )
  
  # tach ANR/spending theo tung segment
  sample_mapping =
    sample %>% 
    left_join(
      final_cal_flag %>% select(unique_token_new, cycle_report, group), 
      by = c('unique_token_new' = 'unique_token_new', 'cycle_report' = 'cycle_report')
    )
  
  sample_mapping %>% 
    arrange(desc(average_balance_in_statement_period_16)) %>% distinct(unique_token_new, cycle_report, .keep_all = T) %>% 
    filter(!status %in% c('close', 'wo')) %>% 
    group_by(user_group, group, billing_month = substr(cycle_report,1,7)) %>% 
    summarise(
      total_anr = sum(average_balance_in_statement_period_16)/10^6
      ,n_cards = n()
    ) %>% 
    range_write(
      ss = file, sheet = sh, range = 'ab246', reformat = F, col_names = F
    )
  
  sample_mapping %>% 
    arrange(desc(authorised_transaction_amount_14)) %>% distinct(unique_token_new, cycle_report, .keep_all = T) %>% 
    filter(!status %in% c('close', 'wo')) %>% 
    group_by(user_group, group, billing_month = substr(cycle_report,1,7)) %>% 
    summarise(
      total_spending = sum(authorised_transaction_amount_14)/10^6
      ,n_cards = n()
    ) %>% 
    range_write(
      ss = file, sheet = sh, range = 'ai246', reformat = F, col_names = F
    )
  
  #### Check them histogram cua port theo tung ky bill
  final_cal_flag %>% 
    filter(substr(cycle_report,1,7) == '2022-11', !group %in% c('check', 'abnormal')) -> check_hist
  
  hist(check_hist$int, breaks = 20, xlim = c(0,0.5925), ylim = c(0,20000),
       xlab = 'IR', main = paste0('Histogram of IR'))
  
  #### check abnormal case 
  final_cal_flag %>% filter(group == 'abnormal') -> abnormal
  duy_all %>% filter(unique_token_new %in% abnormal$unique_token_new) %>% arrange(unique_token_new, cycle_report) %>% view() 
  abnormal %>% group_by(unique_token_new) %>% summarise(n = n()) %>% data.table() %>% count(n) %>% view()

  # a. Tach tap abnormal nay ra thanh 1 tap co thay doi status ( >2 ngoai tru new & active) de check xem pattern cua chung
  duy_all %>% 
    filter(unique_token_new %in% abnormal$unique_token_new) %>% 
    group_by(unique_token_new, status) %>% 
    summarise(n=n()) %>% data.table() %>% filter(n > 2, !status %in% c('new', 'active', 'close')) -> change_status

  distinct(change_status, unique_token_new) #3.9k cards
  
  duy_all %>% filter(unique_token_new %in% change_status$unique_token_new) %>% 
    arrange(desc(interest_income_8_4_5_6_7)) %>% distinct(unique_token_new, cycle_report, .keep_all = T) %>%
    arrange(unique_token_new, cycle_report) %>% view()
    range_write(
      ss = file, sheet = 'Sheet36', range = 'a100', reformat = F
    )
  
  # b. Con lai se la tap gom nhung the khong roll out sang status cat2, cat3 ma hau het la active
  
  
  
  
  # sample
    
    
    
    
  ### ANALYSIS STATUS 'CAT2' ROLL BACK TO ACTIVE
    
  # so luong the reactive lai tu trang thai cat2
  duy_all %>%
    filter(status == 'cat2') %>% 
    distinct(unique_token_new) -> cat2 #12.3k 
  
  duy_all %>% 
    filter(status == 'cat2') %>%
    arrange(desc(cycle_report), desc(interest_income_8_4_5_6_7)) %>% 
    distinct(unique_token_new, .keep_all = T) -> max_cat2

  duy_all %>% 
    filter(status == 'active') %>%
    arrange(desc(cycle_report), desc(interest_income_8_4_5_6_7)) %>% 
    distinct(unique_token_new, .keep_all = T) -> max_active

  mapping = 
    max_cat2 %>%
    rename(max_cat2_cycle = cycle_report) %>%
    left_join(max_active %>% select(unique_token_new, max_active_cycle = cycle_report), by = 'unique_token_new')

  mapping %>% 
    filter(max_active_cycle > max_cat2_cycle) %>% distinct(unique_token_new, .keep_all = T) -> reactive # ~ 9.7%
 
  # sau bao nhieu ky cat2 toi da thi the se active tro lai
  duy_all %>% 
    filter(unique_token_new %in% reactive$unique_token_new) %>%
    filter(status == 'cat2') %>%
    get_dupes(unique_token_new) %>% data.table() %>%
    distinct(unique_token_new, .keep_all = T) -> n_repeat_cat2 # ~ 54.3% trong do la the bi co > 2 ky bi cat2
  
  n_repeat_cat2 %>% count(dupe_count) %>% adorn_totals() %>% view()
    
  # dupe_count   n
  # 2 272
  # 3 112
  # 4  70
  # 5  39
  # 6  34
  # 7  37
  # 8  30
  # 9  23
  # 10  14
  # 11   6
  # 12   3
  # 13   4
  # 14   1
  # 15   1
  # 16   2
  # 17   1
  # Total 649
  
  #toi da co the len toi 15 ky
  
  duy_all %>% filter(unique_token_new %in% n_repeat_cat2[which(dupe_count == 3),]$unique_token_new) %>% arrange(unique_token_new, cycle_report) %>% view()
  
  
  ### lam tuong tu vs status risk
  # so luong the reactive lai tu trang thai risk
  duy_all %>%
    filter(status == 'risk') %>% 
    distinct(unique_token_new) -> risk #7.5k 
  
  duy_all %>% 
    filter(status == 'risk') %>%
    arrange(desc(cycle_report), desc(interest_income_8_4_5_6_7)) %>% 
    distinct(unique_token_new, .keep_all = T) -> max_risk
  
  duy_all %>% 
    filter(status == 'active') %>%
    arrange(desc(cycle_report), desc(interest_income_8_4_5_6_7)) %>% 
    distinct(unique_token_new, .keep_all = T) -> max_active
  
  mapping = 
    max_risk %>%
    rename(max_risk_cycle = cycle_report) %>%
    left_join(max_active %>% select(unique_token_new, max_active_cycle = cycle_report), by = 'unique_token_new')
  
  mapping %>% 
    filter(max_active_cycle > max_risk_cycle) %>% distinct(unique_token_new, .keep_all = T) -> reactive # ~ 8.9%
  
  # sau bao nhieu ky risk toi da thi the se active tro lai
  duy_all %>% 
    filter(unique_token_new %in% reactive$unique_token_new) %>%
    filter(status == 'risk') %>%
    get_dupes(unique_token_new) %>% data.table() %>%
    distinct(unique_token_new, .keep_all = T) -> n_repeat_risk # ~ 54.3% trong do la the bi co > 2 ky bi risk
  
  n_repeat_risk %>% count(dupe_count) %>% adorn_totals() %>% view()
  
  # dupe_count  n
  # 2 56
  # 3  4
  # 4  1
  # Total 61
  
  duy_all %>% filter(unique_token_new %in% n_repeat_risk[which(dupe_count == 2),]$unique_token_new) %>% arrange(unique_token_new, cycle_report) %>% view()
 # co the bi lan lon giua cat 2 roll back ve risk 
    
  
  #### SO THE CLOSE IN Q4-2023 THI STATUS LA GI TRONG Q4-2022
  sh = '4. Attrition rate analysis'
  
  # flag quarter
  duy_all = 
    duy_all %>% 
    mutate(quarter = paste0(substr(cycle_report,1,4),quarters(cycle_report))) 
  
  # loai nhung case chuyen san pham ra
  change_product = 
    duy_all %>% distinct(unique_token_new, product) %>% group_by(unique_token_new) %>%
    summarise(count = n()) %>% filter(count > 1) 
  
  # the close Q1-2023
  close_card_2023q1 =
    duy_all %>% 
    filter(status == 'close') %>% 
    arrange(cycle_report) %>% 
    distinct(unique_token_new, .keep_all = T) %>% 
    filter(!unique_token_new %in% change_product$unique_token_new) %>%
    filter(quarter == '2023Q1')
  
  close_card_2023t1 =
    duy_all %>% 
    filter(status == 'close') %>% 
    arrange(cycle_report) %>% 
    distinct(unique_token_new, .keep_all = T) %>% 
    filter(!unique_token_new %in% change_product$unique_token_new) %>%
    filter(substr(cycle_report,1,7) == '2023-01')
  
  # xem trang thai cua the 'close' Q1-2023 trong Q4-2022 la gi
  duy_all %>%
  filter(!unique_token_new %in% change_product$unique_token_new, unique_token_new %in% close_card_2023q1$unique_token_new, quarter == '2022Q4') %>%
    arrange(desc(cycle_report)) %>%
    distinct(unique_token_new, status, .keep_all = T) -> status_2022q4
  
  duy_all %>%
    filter(!unique_token_new %in% change_product$unique_token_new, unique_token_new %in% close_card_2023t1$unique_token_new, substr(cycle_report,1,7) == '2022-12') %>%
    arrange(desc(interest_income_8_4_5_6_7)) %>%
    distinct(unique_token_new, status, .keep_all = T) -> status_2022t12

  status_2022q4 %>% 
    group_by(status) %>% 
    summarise(t10.2022 = sum(substr(cycle_report,1,7) == '2022-10'),
              t11.2022 = sum(substr(cycle_report,1,7) == '2022-11'),
              t12.2022 = sum(substr(cycle_report,1,7) == '2022-12')) %>% 
    range_write(
      ss = file, sheet = sh, range = 'z112', reformat = F
    )
  
  status_2022t12 %>% 
    group_by(status) %>% 
    summarise(t10.2022 = sum(substr(cycle_report,1,7) == '2022-10'),
              t11.2022 = sum(substr(cycle_report,1,7) == '2022-11'),
              t12.2022 = sum(substr(cycle_report,1,7) == '2022-12')) %>% adorn_totals() %>% view()

  #so the close trong T01-2023 ma trang thai o T12-2022 van la 'active' -> thi thay the close nhung van co phat sinh chi tieu
  close_card_2023t1 %>% filter(authorised_transaction_amount_14 > 0) %>%
    filter(unique_token_new %in% status_2022t12[which(status == 'active'),]$unique_token_new) %>% distinct(unique_token_new)
  
  close_card_2023t1 %>% filter(authorised_transaction_amount_14 == 0) -> close_need_check
  
  status_2022t12 %>% filter(unique_token_new %in% close_need_check$unique_token_new) %>% count(status) %>% view()
  
  duy_all %>% filter(unique_token_new %in% close_need_check$unique_token_new) %>% arrange(unique_token_new, cycle_report) %>% view()
  
  # -- check them the close ma co trang thai van phat sinh spending
  duy_all %>% 
    filter(!unique_token_new %in% change_product$unique_token_new, status == 'close', authorised_transaction_amount_14 > 0) %>% #view()
    count(substr(cycle_report,1,7)) %>% adorn_totals()
  
  #so the close trong T01-2023 ma trang thai o T12-2022 van la 'new' -> the da lau khong spending
  duy_all %>% filter(unique_token_new %in% status_2022t12[which(status == 'new'),]$unique_token_new) %>%
    group_by(unique_token_new) %>% 
    summarise(spending = sum(authorised_transaction_amount_14, na.rm = T)) %>% 
    data.table() %>% filter(spending == 0) -> not_spending
  
  duy_all %>% filter(unique_token_new %in% not_spending$unique_token_new) %>% distinct(unique_token_new, .keep_all = T) %>%
    count(substr(cycle_create_update,1,7))
  
  #so the close trong T01-2023 ma trang thai o T12-2022 la 'pend close' 
  duy_all %>% filter(unique_token_new %in% status_2022t12[which(status == 'pend close'),]$unique_token_new) %>%
    arrange(unique_token_new, cycle_report) %>% view() # co the the truoc do gap trang thai dinh risk, sau do pend close roi moi close
  
  duy_all %>% filter(unique_token_new %in% status_2022t12[which(status == 'pend close'),]$unique_token_new, substr(cycle_report,1,7) == '2022-09') %>% count(status)
  
  duy_all %>% 
    arrange(desc(interest_income_8_4_5_6_7)) %>%
    distinct(unique_token_new, cycle_report, .keep_all = T) %>% 
    group_by(quarter, billing_month = substr(cycle_report,1,7), status) %>% 
    summarise(n_cards = n()) %>% 
    range_write(
      ss = file, sheet = sh, range = 'z1000', reformat = F
    )
  
