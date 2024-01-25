library(tidyverse)
library(rvest)
library(tidygeocoder)
library(sf)
library(here)
library(duckdb)
library(DBI)


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


# Creating Database -------------------------------------------------------

if (file.exists(here("data/duckdb/crime.db"))) {
  
  con <- DBI::dbConnect(duckdb(), dbdir = here("data/duckdb/crime.db"))
  
  new_date <- dbGetQuery(
    con,
    "SELECT MAX (reported_date) AS 'Max Date' FROM crimes"
  )[1,] + 1
  
  crime_df <- get_crime_df(new_date) %>% 
    filter_crime_df() %>% 
    geocode_crime_df()
  
  dbExecute(con, "LOAD 'spatial';")
  dbWriteTable(con, "crimes_temp", crime_df)
  
  dbExecute(con, "CREATE OR REPLACE TABLE crimes_temp AS SELECT *,
      ST_asWKB(ST_Point(lat, long)) as geometry, FROM crimes_temp;")
  
  dbExecute(
    con,
    "ALTER TABLE crimes_temp ADD reported_date DATE null;
     UPDATE crimes_temp SET reported_date=strptime(report_date, '%m/%d/%y');
     ALTER TABLE crimes_temp DROP report_date"
  )
  
  dbExecute(con, "INSERT INTO crimes SELECT * FROM crimes_temp;")
  dbRemoveTable(con, "crimes_temp")
  
} else {
  
  con <- DBI::dbConnect(duckdb(), dbdir = here("data/duckdb/crime.db"))
  
  status <- DBI::dbExecute(con, "INSTALL 'spatial';")
  status <- DBI::dbExecute(con, "LOAD 'spatial';")
  
  crime_df <- get_crime_df(mdy("01/01/2020")) %>% 
    filter_crime_df() %>% 
    geocode_crime_df()
  
  dbWriteTable(con, "crimes", crime_df)
  
  dbExecute(con, "CREATE OR REPLACE TABLE crimes AS SELECT *,
      ST_asWKB(ST_Point(long, lat)) as geometry, FROM crimes;")
  
  dbExecute(
    con,
    "ALTER TABLE crimes ADD reported_date DATE null;
     UPDATE crimes SET reported_date=strptime(report_date, '%m/%d/%y');
     ALTER TABLE crimes DROP report_date"
  )
}

sql <- "SELECT * FROM crimes WHERE reported_date > '2023-09-26'"
sy_crime_sf <- st_read(con, query = sql)
write_csv(sy_crime_sf, here("data/crime.csv"))
dbDisconnect(con, shutdown = TRUE)






