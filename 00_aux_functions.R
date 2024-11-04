require(tidyverse)

get_cdc_groups <- function() {
  cdc_groups <- tribble(
    ~grupo, ~cids,
    "Influenza e Pneumonia", "J09|J1[2-8]",
    "Respiratórias Crônicas Inferiores", "J4[0-7]",
    "Outras doenças respiratórias", "J0[0-6]|J2[0-9]|J3[0-9]|J6[0-9]|J70|J8[0-6]|J9[0-6]|J9[7-9]|R092|U04",
    "Doenças hipertensivas", "I1[0-5]",
    "IAM", "I2[0-5]",
    "Parada Cardíaca", "I50",
    "AVC", "I6[0-9]",
    "Outras doenças do Aparelho Circulatório", "I0[0-9]|I2[6-9]|I3[0-9]|I4[0-9]|I51|I52|I7[0-9]|I8[0-9]|I9[0-9]",
    "Neoplasias", "C[0-8]|C9[0-7]",
    "Alzheimer e demência", "G3[0-1]|F01|F03",
    "Diabetes", "E1[0-4]",
    "Falência Renal", "N1[7-9]",
    "Sepse", "A4[0-1]",
    "Trato urinário", "N39",
    "Causas indeterminadas", "R"
  )
  
  cdc_groups <- cdc_groups %>% 
    add_row(
      tibble(
        grupo = "Causas selecionadas",
        cids = paste(cdc_groups$cids, collapse = "|")
      )
    ) %>% 
    add_row(
      tibble(
        grupo = "Causas naturais",
        cids = "^[^TVWXY]*$"
      )
    ) %>% 
    mutate(group_id = seq_len(nrow(.))) %>% 
    relocate(group_id, .before = everything())
  
  cdc_groups
}

translate_cdc_groups <- function(label) {
  case_when(
    label == "Influenza e Pneumonia" ~ "Influenza and pneumonia",
    label == "Respiratórias Crônicas Inferiores" ~ "Chronic lower respiratory diseases",
    label == "Outras doenças respiratórias" ~ "Other diseases of the respiratory system",
    label == "Doenças hipertensivas" ~ "Hypertensive diseases",
    label == "IAM" ~ "Ischemic heart disease",
    label == "Parada Cardíaca" ~ "Heart failure",
    label == "AVC" ~ "Cerebrovascular diseases",
    label == "Outras doenças do Aparelho Circulatório" ~ "Other disease of the circulatory system",
    label == "Neoplasias" ~ "Malignant neoplasms",
    label == "Alzheimer e demência" ~ "Alzheimer and dementia",
    label == "Diabetes" ~ "Diabetes",
    label == "Falência Renal" ~ "Renal failure",
    label == "Sepse" ~ "Sepsis",
    label == "Trato urinário" ~ "Urinary tract infections",
    label == "Causas indeterminadas" ~ "Undetermined deaths",
    label == "Causas selecionadas" ~ "Selected causes",
    label == "Causas naturais" ~ "Natural causes"
  )
}