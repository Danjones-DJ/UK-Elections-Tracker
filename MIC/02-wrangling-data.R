# 00: Load packages and Data ----------------------------------------------
pacman::p_load(rvest, tidyverse, stringr, purrr, tibble, xml2, janitor)



# 01: Load all data -------------------------------------------------------
allFiles = tibble(file = list.files("datasets", pattern = "\\.xlsx$", full.names = TRUE)) %>%
  mutate(date = str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}") %>% as.Date()) %>%
  arrange(desc(date))

allFiles


# 02: Create list of dataframes -------------------------------------------

allDataFrames = allFiles %>%
  mutate(data = map2(file, date, ~ {
    
    sheets = excel_sheets(.x)
    
    target_sheet = sheets[
      str_detect(sheets, regex("votingintention", ignore_case = TRUE))
    ][1]
    
    read_excel(.x, sheet = target_sheet, skip=4) %>%
      clean_names() %>%
      mutate(date = .y)
  }))


# 03: Create helper function ----------------------------------------------
getTimeSeries = function(df) {
  
  names(df)[1] = "votingintention"
  
  survey_count = df %>%
    filter(votingintention == "Unweighted N") %>%
    pull("all") %>% 
    as.numeric()
  
  df.v2 = df %>% 
    mutate(survey_count = survey_count) %>%
    select(votingintention, "all", survey_count, date) %>%
    filter(!votingintention %in% c("Weighted N", "Unweighted N", "Weight")) %>%
    filter(!is.na(votingintention)) %>%
    mutate(
      all = as.numeric(all),
      raw_count = round(all * survey_count, 0)
    )
  
  remove_counts = df.v2 %>% 
    filter(votingintention %in% c(
      "Don't know",
      "Adjustments:",
      "Another party/Independent candidate",
      "Plaid Cymru",
      "Scottish National Party (SNP)"
    )) %>% 
    pull(raw_count) %>%
    sum(na.rm = TRUE)
  
  survey_count_new = survey_count - remove_counts
  
  DF = df.v2 %>%
    filter(!votingintention %in% c(
      "Don't know",
      "Adjustments:",
      "Another party/Independent candidate",
      "Plaid Cymru",
      "Scottish National Party (SNP)"
    )) %>%
    mutate(
      survey_count_new = survey_count_new,
      true_proportion = (raw_count / survey_count_new) * 100
    ) %>%
    select(date, votingintention, true_proportion, survey_count_new)
  
  return(DF)
}


# 04: Create final data ---------------------------------------------------
MIC_LONG = map_dfr(allDataFrames$data, getTimeSeries)
MIC_WIDE = MIC_LONG %>%pivot_wider(id_cols = date, names_from = votingintention, values_from = true_proportion)

# 05: Save data -----------------------------------------------------------
saveRDS(MIC_LONG, "datasets/LONG-MostInCommon-voting-intention.rds")
saveRDS(MIC_WIDE, "datasets/WIDE-MostInCommon-voting-intention.rds")







