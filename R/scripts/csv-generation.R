library(dplyr)
library(stringr)
library(rvest)
library(tidygeocoder)
library(sf)
library(lubridate)


# Defining Crime Collecting Functions -------------------------------------

get_crime_df <- function(
    start_date, 
    end_date = today() - 1
) {
  # generate link
  # we have to do this silly stuff because the website only accepts
  # double digit months and days
  strt_month <- format.Date(start_date, "%m")
  strt_day <- format.Date(start_date, "%d")
  end_month <- format.Date(end_date, "%m")
  end_day <- format.Date(end_date, "%d")
  
  link <- str_glue("https://incidentreports.uchicago.edu/",
                   "incidentReportArchive.php?",
                   "startDate={strt_month}%2F{strt_day}%2F{year(start_date)}&",
                   "endDate={end_month}%2F{end_day}%2F{year(end_date)}")
  
  # find total # of pages
  crime_site <- read_html(link)
  xpath = paste0('//*[contains(concat( " ", @class, " " ),', 
                 'concat( " ", "page-count", " " ))]')
  
  num_pages <- html_element(crime_site, xpath = xpath) %>% 
    html_text() %>% 
    str_remove("1 / ") %>% 
    as.integer()
  
  # create df from every table
  offset_sequence = seq(from = 0, to = num_pages)
  link_vec = rep(c(0), num_pages)
  
  for (offset in offset_sequence) {
    link_vec[offset] = str_glue(link, "&offset={(offset - 1)* 5}")
  }
  
  crime_list <- rep(list(data.frame()), num_pages)
  
  for (i in 1:num_pages) {
    crime_list[[i]] <- read_html(link_vec[i]) %>% 
      html_element("table") %>%
      html_table()
  }
  
  crime_df <- bind_rows(crime_list)
  
  if (nrow(crime_df) == 0) {
    if (exists("con")) {
      dbDisconnect(con, shutdown = TRUE)
    }
    stop("No crimes to upload.")
  }
  
  return(crime_df)
}


# Defining Filtering Functions --------------------------------------------

filter_crime_df <- function(crime_df) {
  filtered_df <- crime_df %>% 
    filter(str_detect(Incident, "Theft|Robbery|Battery|Assault")) %>% 
    mutate(
      Location = str_replace_all(Location, " \\(.*", "")
    )
  
  return(filtered_df)
}

geocode_crime_df <- function(crime_df) {
  new_locs = c()
  for (loc in crime_df$Location) {
    if (str_detect(loc, "between")) {
      if (str_detect(loc, "^\\d\\d")) {
        number <- str_extract(loc, "^\\d\\d")
        street <- str_match(loc, "(?:\\w* ){2}(\\w*) ")[,2]
        address <- str_glue("{number}00 S. {street}")
      } else {
        number <- str_extract(loc, '\\d\\d')
        street <- str_match(loc, '(\\w*) between')[,2]
        address <- str_glue("{number} E. {street}")
      }
      new_locs <- append(new_locs, address)
    } else {
      new_locs <- append(new_locs, loc)
    }
  }
  crime_df$Location <- new_locs
  
  geocoded_df <- crime_df %>% 
    mutate(
      Location = str_glue("{Location}, Chicago, IL"),
      report_date = str_extract(Reported, "\\d{1,2}/\\d{1,2}/\\d{2}")
    ) %>% 
    geocode(Location, method = "census", lat = lat , long = long) %>% 
    na.omit()
  return(geocoded_df)
}


# Creating CSV ------------------------------------------------------------

if (file.exists("data/23-24_crime.csv")) {
  
  old_csv <- read.csv("data/23-24_crime.csv")
  last_date = as_date(mdy_hm(tail(old_csv, n = 1)$Reported))
  
  if (last_date != today() - 1 && last_date != today()) {
    new_df <- get_crime_df(last_date) %>% 
      filter_crime_df() %>% 
      geocode_crime_df()
    
    crime_df <- bind_rows(old_csv, new_df)
    
    write.csv(crime_df, "data/23-24_crime.csv")
  }
  
} else {
  crime_df <- get_crime_df(mdy("09/26/2023")) %>% 
    filter_crime_df() %>% 
    geocode_crime_df()
  
  write.csv(crime_df, "data/23-24_crime.csv")
}
