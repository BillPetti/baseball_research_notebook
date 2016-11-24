#### Bill Petti
#### billpetti.github.io
#### Research Notebook
#### Acquiring Baseball Stats from the NCAA with R
#### Originally coded November 2016

# load required packages

if(!require(baseballr)) {
  install_github("BillPetti/baseballr")
  require(baseballr)
} # functions for baseball analysis

require(rvest) # for data scraping
require(xml2) # for data scraping
require(dplyr) # for data manipulation

# function to scrape conference_ids

conference_table <- function(year, div) {
  url <- paste0("http://stats.ncaa.org/team/inst_team_list?academic_year=", year, "&conf_id=-1&division=", div, "&sport_code=MBA")
  read <- read_html(url)
  links <- html_nodes(read, "a") %>%
    html_attr("href")
  link_names <- html_nodes(read, "a") %>%
    html_text()
  table <- as.data.frame(cbind(link_names, links))
  names(table) <- c("conference", "links")
  table$conference <- as.character(table$conference)
  links_conferences <- table %>%
    filter(grepl("changeConference", links))
  conference_ids <- sub("\\).*", "", sub(".*\\(", "", links_conferences$links))
  conference_ids <- as.data.frame(conference_ids)
  names(conference_ids) <- "conference_id"
  table <- cbind(links_conferences, conference_ids)
  table <- table %>%
    mutate(year = year, division = div, conference_id = as.numeric(as.character(conference_id))) %>%
    select(year, division, everything(), -links) %>% .[-1,]
  table
}

# loop table

year <- c(2013, 2014, 2015, 2016)
division <- c(1,2,3)
div_yr <- expand.grid(year, division)

# loop over values to create conference lookup table

conference_code_lu <- div_yr %>% 
  group_by(Var1, Var2) %>%
  do(conference_table(.$Var1, .$Var2))

# team function

teams <- function(year, conference, div) {
  url <- paste0("http://stats.ncaa.org/team/inst_team_list?academic_year=", year, "&conf_id=", conference, "&division=", div, "&sport_code=MBA")
  read <- read_html(url)
  links <- html_nodes(read, "a") %>%
    html_attr("href")
  link_names <- html_nodes(read, "a") %>%
    html_text()
  table <- as.data.frame(cbind(link_names, links))
  table$links <- as.character(table$links)
  table$link_names <- as.character(table$link_names)
  table <- table %>%
    filter(grepl("team", links)) %>%
    filter(!grepl("inst_team", links)) %>%
    filter(!grepl("schedule", links))
  table$links <- gsub("/team/", "", table$links)
  table$links <- sub("/.*", "", table$links)
  table$year <- year
  table$division <- div
  table$conference_id <- conference
  names(table) <- c("school", "school_id", "year", "division", "conference_id")
  table
}

# loop over values in the conference_code_lu table 

master_ncaa_team_lu <- conference_code_lu %>% 
  group_by(year, division, conference, conference_id) %>% 
  do(teams(.$year, .$conference_id, .$division)) %>%
  ungroup() %>%
  select(school, conference, everything()) %>%
  mutate(school_id = as.numeric(school_id)) %>%
  arrange(school)

# example uses of the baseballr::ncaa_scrape function
# 736 is the school_id for Vanderbilt

ncaa_scrape(736, 2015, "batting") 

ncaa_scrape(736, 2015, "pitching")

# create looping table

year <- c(2014, 2015, 2016)
school_id <- 736
division <- 1
v_table <- expand.grid(school_id, year)

# loop over values to acquire last three years of pitching data for Vanderbilt

v_table %>%
  group_by(Var1, Var2) %>%
  do(ncaa_scrape(.$Var1, .$Var2, "pitching"))

# create looping table for Vanderbilt and Florida for the past three years

year <- c(2014, 2015, 2016)
school_id <- c(235, 736)
v_table <- expand.grid(school_id, year)

# loop over values to acquire last three years of pitching data for Vanderbilt and Florida

v_table %>%
  group_by(Var1, Var2) %>%
  do(ncaa_scrape(.$Var1, .$Var2, "pitching"))

