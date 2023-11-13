### read file to env 
cic = read_xlsx('~/vib/rev_sharing/DOI SOAT CHI PHI CIC FROM OCT_22.xlsx', sheet = 2) %>% clean_names()

head(cic)

cic %>% count(thang_ds) # tu Q4-2022 den gio 

dj_con <- RMariaDB::dbConnect(drv=RMariaDB::MariaDB(),
                              host = 'dop-production-mysql-replica.ckzhnzkexzci.ap-southeast-1.rds.amazonaws.com',
                              dbname = "digital_journey",
                              user = "vn_da_dj_ro",
                              password = "AcToRtIolyMEMiCE",
                              port = '3306') 

vib_application <- dbGetQuery(dj_con, paste0("
select 
           a.id as application_id
         , lender_config_id
         , date(lead_created_at) as lead_created_date
         , state
         , lead_unique_token
         , lead_product_code
         , lead_telco_code
         , lead_source
         , JSON_VALUE(lead_collected_data, '$.request_params.utm_campaign') utm_campaign
         , JSON_VALUE(lead_collected_data, '$.request_params.utm_medium') utm_medium
         , JSON_VALUE(lead_collected_data, '$.request_params.utm_source') utm_source
         , name
         , s.check_eligible_offer
         , s.status
from applications a 
left join (select name, id from lender_configs) l on a.lender_config_id = l.id
left join (select application_id, status, 1 as check_eligible_offer from application_services where service_name = 'get_eligible_offer') s on a.id = s.application_id
where lead_client_code = 'vib_score_card'
AND date(lead_created_at) >= '2022-09-01' ;
")) 

head(vib_application)

# mapping voi file goc 
cic_mapping = 
  cic %>%
  left_join(vib_application, by = c('lead_unique_token'))

view(head(cic_mapping))

cic_mapping %>% count(lead_source)

cic_mapping[, 2:5][is.na(cic_mapping[, 2:5])] <- 0

cic_final = 
cic_mapping %>% 
  mutate(scheme = case_when(lead_source %in% c('sms', 'op_sms') ~ 0.3, T ~ 0.15),
         price_r14 = 15000,
         price_s11 = 30000) %>% #ban dau a H bao 39k/rq sau do update lai 30k/re vao ngay 18/07
  select(-c(10:22)) %>%
  group_by(lead_unique_token) %>%
  mutate(rq_r14 = sum(dop_r14 + acl_r14, na.rm = T),
         rq_s11 = sum(dop_s11a + acl_s11a, na.rm = T))  %>%
  mutate(total_cost = sum(rq_r14 * price_r14 + rq_s11 * price_s11)) %>% 
  mutate(shared_cost = total_cost * scheme) %>%
  ungroup()

  cic_final %>% 
    group_by(thang_ds, scheme) %>%
    summarise(n_leads = n_distinct(lead_unique_token),
              n_r14_rq = sum(rq_r14),
              n_s11_rq = sum(rq_s11), 
              total_cost = sum(total_cost),
              shared_cost = sum(shared_cost)) %>% 
    range_write(ss = 'https://docs.google.com/spreadsheets/d/1E1NhuswTIPo8JczqKn-FlsUnKk4rEmpZJhNv7nQGcjA/edit#gid=0',
                sheet = 'data',
                range = 'a10',
                reformat = F)
