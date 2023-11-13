# change data frame 
duy_all = rbind(all, hist_all, duy_hist, fill=T)

change_product = 
  duy_all %>% distinct(unique_token_new, product) %>% group_by(unique_token_new) %>%
  summarise(count = n()) %>% filter(count > 1)

duy_all = rbind(duy_raw %>% select(unique_token_new, status, cycle_report, product, cycle_create_update, telco, channel), 
                duy_hist %>%select(unique_token_new, status, cycle_report, product, cycle_create_update, telco, channel))

duy_all %>% 
  filter(status == 'close') %>% 
  arrange(cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) %>% 
  filter(!unique_token_new %in% change_product$unique_token_new) -> close_card

duy_all %>% 
  filter(status == 'close') %>% 
  arrange(cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) %>% 
  filter(!unique_token_new %in% change_product$unique_token_new) %>%
  distinct(unique_token_new, cycle_report, .keep_all = T) %>%
  group_by(billing_cycle = substr(cycle_report,1,7)) %>% 
  summarise(
    op = sum(product == 'op'),
    op_2in1 = sum(product == '2in1')
  ) -> df 
  
row_name =  colnames(df) 
df = df %>% t() %>% data.table()  
names(df)= as_vector(df[1,]) 
df = df[2:nrow(df),]
rownames(df) = row_name[2:length(row_name)]
df = rownames_to_column(df)

close_card %>% filter(substr(cycle_report,1,7) == '2023-04') %>% 
  #view() 
  #count(product, substr(cycle_create,1,7))
  count(channel)

close_card %>% filter(substr(cycle_report,1,7) == '2023-04') -> close_card_t4

view(close_card_t4)

# check tap the active t4 
all %>% filter(status == 'active', substr(cycle_report,1,7) == '2023-04') %>% arrange(desc(interest_income_8_4_5_6_7)) %>% 
  distinct(unique_token_new, .keep_all = T) -> active_card_t4

close_card_t4 %>% filter(unique_token_new %in% active_card_t4$unique_token_new) %>% count(product) 

duy_all %>% 
  filter(status == 'cat2') %>% 
  arrange(cycle_report) %>%
  distinct(unique_token_new, .keep_all = T) %>% 
  group_by(billing_cycle = substr(cycle_report,1,7)) %>% 
  summarise(
    op = sum(product == 'op'),
    op_2in1 = sum(product == '2in1')
  ) -> df 

duy_all %>% 
  filter(status == 'cat3') %>% 
  arrange(cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) %>% 
  group_by(billing_cycle = substr(cycle_report,1,7)) %>% 
  summarise(
    op = sum(product == 'op'),
    op_2in1 = sum(product == '2in1')
  ) -> df 

duy_all %>% 
  filter(status == 'risk') %>% 
  arrange(cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) %>% 
  group_by(billing_cycle = substr(cycle_report,1,7)) %>% 
  summarise(
    op = sum(product == 'op'),
    op_2in1 = sum(product == '2in1')
  ) -> df 


duy_all %>% 
  filter(status == 'close') %>% 
  arrange(cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) %>% 
  filter(substr(cycle_report,1,7) == '2022-11') %>%
  count(substr(cycle_create_update,1,7)) %>% view()

duy_all %>% 
  filter(status == 'close') %>% 
  arrange(cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) %>% 
  filter(substr(cycle_report,1,7) == '2022-11') %>%
  count(telco, channel) %>% view()


duy_all %>% 
  distinct(unique_token_new, cycle_report, .keep_all = T) %>%
  filter(substr(cycle_report,1,7) == '2022-11') %>% 
  count(telco, channel) %>% view()

duy %>% filter(status_final == 'close', substr(cycle_report,1,7) == '2022-11') %>% distinct(unique_token_new) -> close_cycle_Nov_2022

duy %>% filter(unique_token_new %in% close_cycle_Nov_2022$unique_token_new) %>% 
  group_by(channel, issued_month = substr(cycle_create_update_update,1,7)) %>%
  summarise(n_cards = n_distinct(unique_token_new),
            avg_annual_fee = sum(annual_fee_received_9, annual_fee_received_1st_year_9, annual_fee_received_2nd_year_9, annual_fee_received_3rd_year_9, na.rm = T)/n_distinct(unique_token_new),
            avg_overlimit_fee = sum(overlimit_fee_received_10, na.rm = T)/n_distinct(unique_token_new),
            avg_sale_off_us_amt = sum(sale_off_us_amount_11_1, na.rm = T)/n_distinct(unique_token_new),
            avg_authorised_transaction_amt = sum(authorised_transaction_amount_14, na.rm = T)/n_distinct(unique_token_new)) %>% 
  range_write(ss = sh, sheet = 'Sheet23', range = 'h1')

duy %>% filter(unique_token_new %in% close_cycle_Nov_2022$unique_token_new, substr(cycle_create_update_update,1,7) == '2022-11') 

duy_hist %>% filter(!unique_token_new %in% duy$unique_token_new)

# check attrition rate 
head(duy)
duy_all %>% 
  filter(substr(cycle_create_update,1,7) >= '2021-07') %>% 
  arrange(cycle_report) %>% 
  select(unique_token_new, channel, scheme, telco, cycle_create_update) %>% 
  distinct(unique_token_new, .keep_all = T) -> port

duy_all %>% filter(status == 'new') %>% arrange(cycle_report) %>% 
  select(unique_token_new, new_cycle = cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) -> new

duy_all %>% filter(status == 'active') %>% arrange(cycle_report) %>%
  select(unique_token_new, active_cycle = cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) -> active

duy_all %>% filter(status == 'risk') %>% arrange(cycle_report) %>%
  select(unique_token_new, risk_cycle = cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) -> risk

duy_all %>% filter(status == 'cat2') %>% arrange(cycle_report) %>% 
  select(unique_token_new, cat2_cycle = cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) -> cat2

duy_all %>% filter(status == 'cat3') %>% arrange(cycle_report) %>% 
  select(unique_token_new, cat3_cycle = cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) -> cat3

duy_all %>% filter(status == 'wo') %>% arrange(cycle_report) %>% 
  select(unique_token_new, wo_cycle = cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) -> wo

duy_all %>% filter(status == 'close') %>% arrange(cycle_report) %>% 
  select(unique_token_new, close_cycle = cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) -> close

duy_all %>% filter(status == 'lost') %>% arrange(cycle_report) %>% 
  select(unique_token_new, lost_cycle = cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) -> lost 

duy_all %>% filter(status == 'fraud') %>% arrange(cycle_report) %>% 
  select(unique_token_new, fraud_cycle = cycle_report) %>%
  distinct(unique_token_new, .keep_all = T) -> fraud

duy_all %>% filter(status == 'pend_close') %>% arrange(cycle_report) %>% 
  select(unique_token_new, pend_close_cycle = cycle_report) %>%
  distinct(unique_token_new, .keep_all = T) -> pend_close #0

#xem thu pattern ve flag status
duy_all %>% 
  filter(unique_token_new %in% fraud$unique_token_new) %>%
  arrange(unique_token_new, cycle_report) %>% view()

port_mapping = 
  port %>% 
  left_join(new, by = 'unique_token_new') %>% 
  left_join(active, by = 'unique_token_new') %>% 
  left_join(risk, by = 'unique_token_new') %>% 
  left_join(cat2, by = 'unique_token_new') %>% 
  left_join(cat3, by = 'unique_token_new') %>% 
  left_join(wo, by = 'unique_token_new') %>% 
  left_join(close, by = 'unique_token_new') 

head(port_mapping)

port_mapping %>% 
  filter(!is.na(close_cycle)) %>% 
  count(!is.na(new_cycle), !is.na(active_cycle), !is.na(risk_cycle), !is.na(cat2_cycle), !is.na(cat3_cycle), !is.na(wo_cycle)) %>% 
  view()

port_mapping %>% 
  filter(!is.na(close_cycle),
         is.na(new_cycle), !is.na(active_cycle), is.na(risk_cycle), is.na(cat2_cycle), is.na(cat3_cycle), is.na(wo_cycle)) -> a_c

# khi thẻ close thì mỗi token tại 1 kỳ bill sẽ ghi nhận 2 rows -> check logic close


duy_all %>% filter(unique_token_new %in% a_c$unique_token_new) %>% 
  arrange(unique_token_new, cycle_report) %>% view()

port_mapping %>% 
  filter(!is.na(close_cycle),
         !is.na(active_cycle),
         !is.na(cat2_cycle) | !is.na(cat3_cycle) | !is.na(wo_cycle)) -> obs

obs %>% 
  mutate(delta_1 = interval(risk_cycle, close_cycle) %/% months(1),
         delta_2 = interval(cat2_cycle, close_cycle) %/% months(1),
         delta_3 = interval(cat3_cycle, close_cycle) %/% months(1),
         delta_4 = interval(wo_cycle, close_cycle) %/% months(1)) -> obs_flag

obs_flag %>% count(delta_1) %>% arrange(desc(n))
obs_flag %>% filter(delta_2 > 0) %>% count(delta_2) %>% arrange(desc(n))
obs_flag %>% filter(delta_3 > 0) %>% count(delta_3) %>% arrange(desc(n))
obs_flag %>% filter(delta_4 < 0) %>% count(delta_4) %>% arrange(desc(n))

duy_all %>% filter(status == 'active') %>% arrange(desc(cycle_report)) %>%
  select(unique_token_new, max_active_cycle = cycle_report) %>% 
  distinct(unique_token_new, .keep_all = T) -> active


cat2 %>% left_join(active, by = 'unique_token_new')  %>% filter(substr(max_active_cycle,1,7) > substr(cat2_cycle,1,7)) %>% 
  mutate(delta = interval(cat2_cycle, max_active_cycle) %/% months(1)) %>% count(delta) %>% arrange(desc(n)) %>% view()

