library(tidyverse)
library(REDCapR)

# Загрузка данных ---------------------------------------------------------
uri     <- "http://redcap.fccho-moscow.ru/api/"
token   <- ""
data    <- REDCapR::redcap_read(redcap_uri=uri, token=token, raw_or_label = "label")$data

data_del1 <- data %>%
  filter(redcap_event_name == 'ХТ 1') %>%
  select(c(record_id,inc_age:inc_sign)) %>%
  filter(if_any(inc_age:inc_sign, ~ . == 'Нет'))

data_del2 <- data %>%
  filter(redcap_event_name == 'ХТ 1') %>%
  select(c(record_id,exc_apsych:exc_gcs)) %>%
  filter(if_any(exc_apsych:exc_gcs, ~ . == 'Да'))

data_del3 <- data %>%
  filter(redcap_event_name == 'ХТ 2') %>%
  select(c(record_id,ct_2_is)) %>%
  filter(if_any(ct_2_is, ~ . == 'Нет'))

data_del <- c(data_del1$record_id,data_del2$record_id,data_del3$record_id)
data <- data[!(data$record_id %in% data_del),] # убираем не прошедших на исследование

base_data <- data %>%
  filter(redcap_event_name == "ХТ 1") %>%
  select(record_id, patient_name:ddcd_696bee_complete)
base_data <- base_data[colnames(base_data) %in% c('record_id','patient_birthdate',
                                               'patient_sex','patient_diag',
                                               'patient_diag_oth','ct_pre_is',
                                               'ct_pre_hasit','ct_plan_pt_is',
                                               'rand_arm')] 
base_data <- left_join(base_data,recommend <- filter(data,redcap_event_name == 'Оценка')[,c('record_id','recommend')])
# Получили таблицу для подсчёта основных частот


ct1 <- data %>%
  filter(str_detect(redcap_event_name, "ХТ 1 День \\d$")) %>%
  select(record_id, redcap_event_name, ct_dt:ddcd_2a49_complete)

ct1_p <- data %>%
  filter(str_detect(redcap_event_name, "ХТ 1 День \\dP$"),
         str_detect(redcap_event_name, "ХТ 1 День 5P", negate = T)) %>%
  select(record_id, redcap_event_name, ct_dt:ddcd_2a49_complete)

ct2 <- data %>%
  filter(str_detect(redcap_event_name, "ХТ 2 День \\d$")) %>%
  select(record_id, redcap_event_name, ct_dt:ddcd_2a49_complete)

ct2_p <- data %>%
  filter(str_detect(redcap_event_name, "ХТ 2 День \\dP$"),
         str_detect(redcap_event_name, "ХТ 2 День 5P", negate = T)) %>%
  select(record_id, redcap_event_name, ct_dt:ddcd_2a49_complete)

gr <- data_del1 <- data %>%
  filter(redcap_event_name == 'ХТ 1') %>%
  select(record_id, rand_arm)

# нежелательные явления
calculate <- . %>%
  select(record_id, ct_ae_common:ct_ae_pns) %>%
  pivot_longer(-record_id) %>%
  group_by(record_id) %>%
  summarise(ct_ae = any(value != "Grade 0"))

ct_ae_1 <- bind_rows(ct1, ct1_p) %>% calculate()
ct_ae_2 <- bind_rows(ct2, ct2_p) %>% calculate()
colnames(ct_ae_1) <- c('record_id','ct_ae_1')
colnames(ct_ae_2) <- c('record_id','ct_ae_2')

calculate <- . %>%
  select(record_id, ct_ae_common:ct_ae_pns) %>%
  pivot_longer(-record_id) %>%
  group_by(record_id) %>%
  summarise(ct_ae = any(str_starts(value, "Grade 3")|str_starts(value, "Grade 4")))

ct_ae_34_1 <- bind_rows(ct1, ct1_p) %>% calculate()
ct_ae_34_2 <- bind_rows(ct2, ct2_p) %>% calculate()
colnames(ct_ae_34_1) <- c('record_id','ct_ae_34_1')
colnames(ct_ae_34_2) <- c('record_id','ct_ae_34_2')


# не было рвоты и/или применения терапии
calculate <- . %>%
  group_by(record_id) %>%
  summarise(ct_vomit = any(ct_vomit != "Нет"),
            ct_aemet_is = any(ct_aemet_is != "Нет"),
            bad = ct_vomit | ct_aemet_is,
            good = !bad) %>%
            select(record_id,good)

ct1_24 <- calculate(ct1) # терапия и первые 24 часа
ct2_24 <- calculate(ct2)

ct1_p_120 <- calculate(ct1_p) # 120 часов после терапии
ct2_p_120 <- calculate(ct2_p)


ct1_120 <- bind_rows(ct1, ct1_p) %>% calculate()  # терапия и 120 часов после терапии
ct2_120 <- bind_rows(ct2, ct2_p) %>% calculate()


# Доля пациентов, у которых не было
# тошноты (оценка по PeNAT) и/или
# рвоты и/или
# использования дополнительных противорвотных препаратов
# в период проведения цикла химиотерапии и
# в течение 120 часов после его окончания

calculate <- . %>%
  group_by(record_id) %>%
  summarise(ct_nausea = any(ct_nausea != "1 - отсутствие тошноты"),
            ct_vomit = any(ct_vomit != "Нет"),
            ct_aemet_is = any(ct_aemet_is != "Нет"),
            bad = ct_nausea | ct_vomit | ct_aemet_is,
            good = !bad) %>%
  select(record_id,good)

ct1_t <- bind_rows(ct1, ct1_p) %>% calculate()
ct2_t <- bind_rows(ct2, ct2_p) %>% calculate() 

base_data <- left_join(base_data,data.frame(record_id = ct1_24$record_id,ct1_24 = ct1_24$good,ct2_24 = ct2_24$good,ct1_p_120 = ct1_p_120$good,ct2_p_120 = ct2_p_120$good,
                                            ct1_120 = ct1_120$good,ct2_120 = ct2_120$good,ct1_t = ct1_t$good,ct2_t = ct2_t$good))

# промежуточная таблица со всеми данными

ct2 <- data %>%
  filter(str_detect(redcap_event_name, "ХТ 2 День \\d$")) %>%
  select(record_id, redcap_event_name) %>%
  add_column(n = 1) %>%
  group_by(record_id) %>%
  summarise(n_day_ct_2 = sum(n))


ct1 <- data %>%
  filter(str_detect(redcap_event_name, "ХТ 1 День \\d$")) %>%
  select(record_id, redcap_event_name) %>%
  add_column(n = 1) %>%
  group_by(record_id) %>%
  summarise(n_day_ct1 = sum(n)) %>%
  left_join(ct2)

base_data <- base_data %>% left_join(ct1) #вместе с количеством дней каждого курса терапии
base_data <- base_data %>% left_join(ct_ae_1)
base_data <- base_data %>% left_join(ct_ae_2)
base_data <- base_data %>% left_join(ct_ae_34_1)
base_data <- base_data %>% left_join(ct_ae_34_2)
colnames(base_data) <- c("record_id","Дата рождения","Пол","Диагноз","Другой диагноз","Получал химиотерапию ранее?",
                         "Ранее получал курсы ПОДОБНОЙ химиотерапии?","Высокие дозы цисплатина или высокие дозы карбоплатина",
                         "Ветвь рандомизации","Для последующего цикла пациенту рекомендован режим","Цикл 1, наличие рвоты/применения терапии во время ХТ",
                         "Цикл 2, наличие рвоты/применения терапии во время ХТ","Цикл 1, наличие рвоты/применения терапии следующие 120 часов после ХТ",
                         "Цикл 2, наличие рвоты/применения терапии следующие 120 часов после ХТ",
                         "Цикл 1, наличие рвоты/применения терапии во время ХТ и следующие 120 часов после ХТ","Цикл 2, наличие рвоты/применения терапии во время ХТ следующие 120 часов после ХТ",
                         "Цикл 1, наличие тошноты/рвоты/применения терапии во время ХТ и следующие 120 часов после ХТ","Цикл 2, наличие тошноты/рвоты/применения терапии во время ХТ и следующие 120 часов после ХТ",
                         "Количество дней 1 цикла ХТ","Количество дней 2 цикла ХТ",
                         "Наличие нежелательных явлений 1 цикла ХТ","Наличие нежелательных явлений 2 цикла ХТ",
                         "Наличие нежелательных явлений 3-4 степени 1 цикла ХТ","Наличие нежелательных явлений 3-4 степени 2 цикла ХТ")
write_csv(base_data,'base_data.csv')
