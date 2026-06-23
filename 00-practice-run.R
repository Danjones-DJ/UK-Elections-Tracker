pacman::p_load(readxl, tidyverse)
options(scipen=999)


# 01: Load all data -------------------------------------------------------

df1 <- read_excel("datasets/MIC/votingintention18june.xlsx", sheet = "votingintention (raw)", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "18-June-2026")
df2 <- read_excel("datasets/MIC/voting-intention-june-10.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "10-June-2026")
df3 <- read_excel("datasets/MIC/voting-intention-june-3.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "3-June-2026")
df4 <- read_excel("datasets/MIC/voting-intention-may-27.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "27-May-2026")
df5 <- read_excel("datasets/MIC/voting-intention-may-20.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "20-May-2026")
df6 <- read_excel("datasets/MIC/voting-intention-may-13.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "13-May-2026")
df7 <- read_excel("datasets/MIC/voting-intention-may-6.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "6-May-2026")
df8 <- read_excel("datasets/MIC/voting-intention-april-29.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "29-April-2026")
df9 <- read_excel("datasets/MIC/voting-intention-april-22.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "22-April-2026")
df10 <- read_excel("datasets/MIC/voting-intention-april-15.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "15-April-2026")
df11 <- read_excel("datasets/MIC/voting-intention-april-8.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "8-April-2026")
df11 <- read_excel("datasets/MIC/voting-intention-april-1.xlsx", sheet = "votingintention", skip = 4) %>% janitor::clean_names() %>% mutate(surveydate = "8-April-2026")


# 02: Helper function for creating time series ----------------------------

getTimeSeries = function(df) {
  # Get survey count
  survey_count = df %>%
    filter(votingintention == "Unweighted N") %>%
    pull(all) %>% as.numeric()
  
  # Store survey count
  df$survey_count = survey_count
  
  # Clean NAs and select variables of interest (intention, all, date, count)
  df.v2 = df %>% 
    select(votingintention, all, survey_count, surveydate) %>%
    filter(!votingintention %in% c("Weighted N", "Unweighted N", "Weight")) %>%
    filter(!is.na(votingintention)) %>%
    mutate(
      all = as.numeric(all),
      raw_count = round(all * survey_count, 0)
    )
  
  # Get DK count and remove from total N
  dk_count = df.v2 %>% 
    filter(votingintention == "Don't know") %>% 
    pull(raw_count)
  
  # New count
  survey_count_new = survey_count - dk_count
  
  # Relevel
  df.v2$survey_count_new = survey_count_new
  
  # Final stage
  DF = df.v2 %>%
    filter(votingintention != "Don't know") %>%
    mutate(
      true_proportion = (raw_count / survey_count_new) * 100
    ) %>%
    select(votingintention, surveydate, true_proportion, survey_count_new)
  
  return(DF)
}

# Test
getTimeSeries(df1)
getTimeSeries(df2)


# 03: Map all to create time series ---------------------------------------

dfs <- list(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11)

MIC = purrr::map_dfr(dfs, getTimeSeries)



MIC <- MIC %>%
  mutate(surveydate = lubridate::dmy(surveydate))

MIC %>%
  ggplot(aes(x = surveydate, y = true_proportion, colour = votingintention, group = votingintention)) +
  geom_line()
