
library(krvotes)
library(tidyverse)

`총선_df` <- congress_2018 %>% 
  filter(str_detect(`precinct`, "분당")) %>% 
  unnest(data_clean) %>% 
  group_by(`읍면동명`) %>% 
  summarise(`진보` = sum(`더불어민주당 김병관`, `더불어민주당 김병욱`, na.rm=TRUE),
            `보수` = sum(`새누리당 권혁세`, `새누리당 전하진`, na.rm=TRUE),
            `그 외` = sum(`국민의당 염오봉`, `국민의당 윤은숙`, `무소속 임태희`, na.rm=TRUE)) %>% 
  mutate(`선거` = "2016총선")

`분당_df` <- bind_rows(`총선_df`) %>% 
  filter(!str_detect(`읍면동명`, "잘못")) %>% 
  select(`선거`, everything())


`분당_df` %>% 
  gather(`정당`, `득표수`, -`선거`, -`읍면동명`) %>% 
  mutate(`정당` = factor(`정당`, levels = c("진보", "보수", "그 외"))) %>% 
  group_by(`선거`, `정당`) %>% 
  summarise(`득표수` = sum(`득표수`)) %>% 
  spread(`정당`, `득표수`) %>% 
  DT::datatable()
