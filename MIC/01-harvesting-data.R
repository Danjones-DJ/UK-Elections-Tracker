# 00: Load packages and URLs ----------------------------------------------
pacman::p_load(rvest, tidyverse, stringr, purrr, tibble, xml2)

# Load URL 
url = "https://www.moreincommon.org.uk/polling-tables/?_polling_tables_type=voting-intention"
page = read_html(url)

# 01: Extract download links --------------------------------------------------

excel_nodes = page %>%
  html_elements("a[href$='.xlsx']")

links = tibble(
  href = html_attr(excel_nodes, "href"),
  link_text = html_text2(excel_nodes),
  nearby_text = excel_nodes %>%
    xml_parent() %>%
    xml_parent() %>%
    html_text2()
)

# 02: Filter to only keep data of interest ------------------------------------
# Data of interest is GB adults, excluding Northern Ireland

validPattern = regex(
  "XLS Voting Intention & Trackers Voting intention|XLS Voting Intention Voting intention|XLS Voting intention and trackers \\(12 -15 June\\) Voting intention",
  ignore_case = TRUE
)

filtered_links = links %>%
  select(href, nearby_text) %>%
  mutate(
    nearby_text = str_squish(nearby_text),
    end_week = str_squish(str_sub(nearby_text, -7, -2))
  ) %>%
  filter(str_detect(nearby_text, validPattern)) %>%
  mutate(recency_rank = row_number())  # 1 = most recent data

# 03: Parse day/month from end_week -------------------------------------------

currentYear = 2026

filtered_links.v2 = filtered_links %>%
  mutate(
    end_day = as.numeric(str_sub(end_week, 1, 2)),
    end_month = as.numeric(match(str_sub(end_week, -3, -1), month.abb)),
    end_year = if_else(recency_rank == 1, currentYear, NA_real_)
  )

# 04: Resolve full dates by walking backward through time --------------------
# Each row's year is inferred relative to the PREVIOUS (already-resolved) row,
# instead of a day/month-gap lookup table. Fixes the Dec->Jan wraparound bug,
# leap-year errors, and the NA cascade.

filtered_links.v3 = filtered_links.v2

for (i in 2:nrow(filtered_links.v3)) {
  prev_day   = filtered_links.v3$end_day[i - 1]
  prev_month = filtered_links.v3$end_month[i - 1]
  prev_year  = filtered_links.v3$end_year[i - 1]
  
  this_day   = filtered_links.v3$end_day[i]
  this_month = filtered_links.v3$end_month[i]
  
  prev_date = make_date(prev_year, prev_month, prev_day)
  
  candidate_year = prev_year
  candidate_date = make_date(candidate_year, this_month, this_day)
  
  steps = 0
  while (candidate_date > prev_date && steps < 5) {
    candidate_year = candidate_year - 1
    candidate_date = make_date(candidate_year, this_month, this_day)
    steps = steps + 1
  }
  
  filtered_links.v3$end_year[i] = candidate_year
}

filtered_links.v3 = filtered_links.v3 %>%
  mutate(endDate = make_date(end_year, end_month, end_day))

# 05: Build final filenames, disambiguating any real collisions --------------
# If two different files resolve to the same endDate (a genuine source-data
# inconsistency, e.g. two filenames both labelled with the same fieldwork
# week), keep BOTH instead of letting one silently overwrite the other.

FINAL_LINKS = filtered_links.v3 %>%
  select(href, nearby_text, endDate) %>%
  mutate(
    is_dupe = endDate %in% endDate[duplicated(endDate)],
    file_slug = tools::file_path_sans_ext(basename(href)),
    out_name = if_else(
      is_dupe,
      paste0("MoreInCommon-voting-intention", endDate, "_", file_slug, ".xlsx"),
      paste0("MoreInCommon-voting-intention", endDate, ".xlsx")
    )
  )

# 06: Sanity checks before downloading ----------------------------------------

cat("Rows:", nrow(FINAL_LINKS), "\n")
cat("NA dates:", sum(is.na(FINAL_LINKS$endDate)), "\n")
cat("Duplicate dates:", sum(FINAL_LINKS$is_dupe), "\n")
cat("Unique output filenames:", n_distinct(FINAL_LINKS$out_name), "\n")

if (sum(is.na(FINAL_LINKS$endDate)) > 0) {
  warning("Some endDates are still NA -- check these rows before downloading:")
  print(FINAL_LINKS %>% filter(is.na(endDate)))
}

if (sum(FINAL_LINKS$is_dupe) > 0) {
  warning("Some endDates collide -- both files will be KEPT with disambiguated names. Verify these manually:")
  print(FINAL_LINKS %>% filter(is_dupe) %>% select(href, nearby_text, endDate, out_name))
}

# 07: Download -----------------------------------------------------------------

dir.create("datasets", showWarnings = FALSE)

FINAL_LINKS %>%
  select(href, out_name) %>%
  pwalk(function(href, out_name) {
    download.file(
      url = href,
      destfile = file.path("datasets", out_name),
      mode = "wb"
    )
  })

cat("Files written:", length(list.files("datasets")), "vs rows:", nrow(FINAL_LINKS), "\n")
print("DONE")