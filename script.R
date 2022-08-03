library(tidyverse)
library(REDCapR)


# Загрузка данных ---------------------------------------------------------
uri     <- "http://redcap.fccho-moscow.ru/api/"
token   <- ""
data    <- REDCapR::redcap_read(redcap_uri=uri, token=token, raw_or_label = "label")$data


# Подготовка таблиц ---------------------------------------------------------
data_excluded <- data %>%
  filter(redcap_event_name == 'ХТ 1') %>%
  filter(if_any(inc_age:inc_sign, ~ . == 'Нет') | if_any(exc_apsych:exc_gcs, ~ . == 'Да'))

data_wo_ct2 <- data %>%
  filter(redcap_event_name == 'ХТ 2') %>%
  filter(ct_2_is == "Нет")

data <- data %>%
  anti_join(data_excluded, "record_id") %>%
  anti_join(data_wo_ct2, "record_id")

data_recommend <- data %>%
  filter(redcap_event_name == 'Оценка') %>%
  select(record_id, recommend, rating)

base_data <- data %>%
  filter(redcap_event_name == "ХТ 1") %>%
  select(record_id,
         patient_birthdate, patient_sex, patient_diag, patient_diag_oth,
         ct_pre_is, ct_pre_hasit, ct_plan_pt_is,
         rand_arm) %>%
  left_join(data_recommend, 'record_id')

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


# Наличие осложнений ---------------------------------------------------------
calculate <- . %>%
  select(record_id, ct_ae_common:ct_ae_pns) %>%
  pivot_longer(-record_id) %>%
  group_by(record_id) %>%
  summarise(ct_ae = any(value != "Grade 0"))

# наличие/остутствие осложнений
ct_ae_1 <- bind_rows(ct1, ct1_p) %>% calculate() %>% rename(ct_ae_1 = ct_ae)
ct_ae_2 <- bind_rows(ct2, ct2_p) %>% calculate() %>% rename(ct_ae_2 = ct_ae)

calculate <- . %>%
  select(record_id, ct_ae_common:ct_ae_pns) %>%
  pivot_longer(-record_id) %>%
  group_by(record_id) %>%
  summarise(ct_ae = any(str_starts(value, "Grade 3")|str_starts(value, "Grade 4")))

# наличие/остутствие осложнений 3-4 степени
ct_ae_34_1 <- bind_rows(ct1, ct1_p) %>% calculate() %>% rename(ct_ae_34_1 = ct_ae)
ct_ae_34_2 <- bind_rows(ct2, ct2_p) %>% calculate() %>% rename(ct_ae_34_2 = ct_ae)


# эпизоды рвоты и/или использования дополнительных противорвотных препаратов (терапия спасения) 
# в период проведения химиотерапии и после его окончания (24 часа) ----------------------------
calculate <- . %>%
  group_by(record_id) %>%
  summarise(ct_vomit = any(ct_vomit != "Нет"),
            ct_aemet_is = any(ct_aemet_is != "Нет"),
            bad = ct_vomit | ct_aemet_is)%>%
  select(record_id,ct_vomit,ct_aemet_is,bad)

ct1_24 <- calculate(ct1) 
ct2_24 <- calculate(ct2)


# эпизоды рвоты и/или использования дополнительных противорвотных препаратов (терапия спасения) 
# после окончания первого цикла химиотерапии (25-120 часов) ----------------------------------
ct1_p_120 <- calculate(ct1_p)
ct2_p_120 <- calculate(ct2_p)


# эпизоды рвоты и/или использования дополнительных противорвотных препаратов (терапия спасения) 
# в период проведения первого цикла химиотерапии и после его окончания (25-120 часов) --------
ct1_120 <- bind_rows(ct1, ct1_p) %>% calculate() 
ct2_120 <- bind_rows(ct2, ct2_p) %>% calculate()


# эпизоды тошноты (оценка по шкале PeNAT) и/или рвоты и/или использования дополнительных противорвотных препаратов --------
# (терапия спасения) в период проведения первого цикла химиотерапии и 120 часов после его окончания 
calculate <- . %>%
  group_by(record_id) %>%
  summarise(ct_nausea = any(ct_nausea != "1 - отсутствие тошноты"),
            ct_vomit = any(ct_vomit != "Нет"),
            ct_aemet_is = any(ct_aemet_is != "Нет"),
            bad = ct_nausea | ct_vomit | ct_aemet_is) %>%
  select(record_id,ct_nausea,bad)

ct1_t <- bind_rows(ct1, ct1_p) %>% calculate()
ct2_t <- bind_rows(ct2, ct2_p) %>% calculate() 


# Количество n-дневных режимов химиотерапии ---------------------------------------------------------
ct1_n <- count(ct1, record_id, name = "n_day_ct1")
ct2_n <- count(ct2, record_id, name = "n_day_ct2")


# Формируем итоговую таблицу ---------------------------------------------------------
final <- base_data %>%
  left_join(select(ct1_24,    record_id, ct1_24    = bad, ct1_24_vomit = ct_vomit,  ct1_24_aemet = ct_aemet_is), 'record_id') %>%
  left_join(select(ct2_24,    record_id, ct2_24    = bad, ct2_24_vomit = ct_vomit,  ct2_24_aemet = ct_aemet_is), 'record_id') %>%
  left_join(select(ct1_p_120, record_id, ct1_p_120 = bad, ct1_p_120_vomit = ct_vomit,  ct1_p_120_aemet = ct_aemet_is), 'record_id') %>%
  left_join(select(ct2_p_120, record_id, ct2_p_120 = bad, ct2_p_120_vomit = ct_vomit,  ct2_p_120_aemet = ct_aemet_is), 'record_id') %>%
  left_join(select(ct1_120,   record_id, ct1_120   = bad, ct1_120_vomit = ct_vomit,  ct1_120_aemet = ct_aemet_is), 'record_id') %>%
  left_join(select(ct2_120,   record_id, ct2_120   = bad, ct2_120_vomit = ct_vomit,  ct2_120_aemet = ct_aemet_is), 'record_id') %>%
  left_join(select(ct1_t,     record_id, ct1_t     = bad, ct1_t_nausea = ct_nausea), 'record_id') %>%
  left_join(select(ct2_t,     record_id, ct2_t     = bad, ct2_t_nausea = ct_nausea), 'record_id') %>%
  left_join(ct1_n, 'record_id') %>%
  left_join(ct2_n, 'record_id') %>%
  left_join(ct_ae_1, 'record_id') %>%
  left_join(ct_ae_2, 'record_id') %>%
  left_join(ct_ae_34_1, 'record_id') %>%
  left_join(ct_ae_34_2, 'record_id')

final <- final %>%
  mutate_if(is.logical, as.numeric)

labels <- c(
  record_id = "record_id",
  patient_birthdate = "Дата рождения",
  patient_sex = "Пол",
  patient_diag = "Диагноз",
  patient_diag_oth = "Другой диагноз",
  ct_pre_is = "Получал химиотерапию ранее?",
  ct_pre_hasit = "Ранее получал курсы ПОДОБНОЙ химиотерапии?",
  ct_plan_pt_is = "Высокие дозы цисплатина или высокие дозы карбоплатина",
  rand_arm = "Ветвь рандомизации",
  recommend = "Для последующего цикла пациенту рекомендован режим",
  rating = "Предпочтительный режим противорвотной профилактики",
  ct1_24 = "Цикл 1, наличие рвоты/применения терапии во время ХТ",
  ct1_24_vomit = "Цикл 1, наличие рвоты во время ХТ",
  ct1_24_aemet = "Цикл 1, наличие применения терапии во время ХТ",
  ct2_24 = "Цикл 2, наличие рвоты/применения терапии во время ХТ",
  ct2_24_vomit = "Цикл 2, наличие рвоты во время ХТ",
  ct2_24_aemet = "Цикл 2, наличие применения терапии во время ХТ",
  ct1_p_120 = "Цикл 1, наличие рвоты/применения терапии следующие 120 часов после ХТ",
  ct1_p_120_vomit = "Цикл 1, наличие рвоты следующие 120 часов после ХТ",
  ct1_p_120_aemet = "Цикл 1, наличие применения терапии следующие 120 часов после ХТ",
  ct2_p_120 = "Цикл 2, наличие рвоты/применения терапии следующие 120 часов после ХТ",
  ct2_p_120_vomit = "Цикл 2, наличие рвоты следующие 120 часов после ХТ",
  ct2_p_120_aemet = "Цикл 2, наличие применения терапии следующие 120 часов после ХТ",
  ct1_120 = "Цикл 1, наличие рвоты/применения терапии во время ХТ и следующие 120 часов после ХТ",
  ct1_120_vomit = "Цикл 1, наличие рвоты во время ХТ и следующие 120 часов после ХТ",
  ct1_120_aemet = "Цикл 1, наличие применения терапии во время ХТ и следующие 120 часов после ХТ",
  ct2_120 = "Цикл 2, наличие рвоты/применения терапии во время ХТ следующие 120 часов после ХТ",
  ct2_120_vomit = "Цикл 2, наличие рвоты во время ХТ следующие 120 часов после ХТ",
  ct2_120_aemet = "Цикл 2, наличие применения терапии во время ХТ следующие 120 часов после ХТ",
  ct1_t = "Цикл 1, наличие тошноты/рвоты/применения терапии во время ХТ и следующие 120 часов после ХТ",
  ct1_t_nausea = "Цикл 1, наличие тошноты во время ХТ и следующие 120 часов после ХТ",
  ct2_t = "Цикл 2, наличие тошноты/рвоты/применения терапии во время ХТ и следующие 120 часов после ХТ",
  ct2_t_nausea = "Цикл 2, наличие тошноты во время ХТ и следующие 120 часов после ХТ",
  n_day_ct1 = "Количество дней 1 цикла ХТ",
  n_day_ct2 = "Количество дней 2 цикла ХТ",
  ct_ae_1 = "Наличие нежелательных явлений 1 цикла ХТ",
  ct_ae_2 = "Наличие нежелательных явлений 2 цикла ХТ",
  ct_ae_34_1 = "Наличие нежелательных явлений 3-4 степени 1 цикла ХТ",
  ct_ae_34_2 = "Наличие нежелательных явлений 3-4 степени 2 цикла ХТ"
)

final <- final %>% rename_with(~ labels[.])

openxlsx::write.xlsx(final, "base_data.xlsx")
