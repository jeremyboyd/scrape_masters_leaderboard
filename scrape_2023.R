# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Scrape Masters leaderboard data every 60 seconds and write to a
# Google sheet.

# Packages
library(tidyverse)
library(RSelenium)          # Selenium server
library(rvest)              # Web scraping
library(googlesheets4)
library(httpuv)
library(lubridate)

# Interactive authorization puts token in secrets. This only has to be done
# once when the script is first run.
# gs4_auth(email = "kenyonboyd@gmail.com", cache = ".secrets")
# gs4_deauth()

# Scrape this page
url <- "https://www.masters.com/en_US/scores/index.html"
# url <- "https://2022.masters.com/en_US/scores/index.html"

# Authorize access to Google sheets
gs4_auth(email = "kenyonboyd@gmail.com",
         cache = ".secrets")

# Start Docker
system("open --background -a Docker", wait = TRUE)
Sys.sleep(40)
message("Finished starting Docker.")

# Start Selenium container on Docker
system("docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.1",
       wait = TRUE)
message("Finished starting Selenium server.")

# Connect to server
remDr <- remoteDriver(port = 4445L)
Sys.sleep(5)
remDr$open()
Sys.sleep(5)
message("Finished connecting to server.")

# Navigate to URL and wait for page to load
remDr$navigate(url)
Sys.sleep(5)
message(paste0("Finished navigating to ", url, "."))

# The leaderboard page has a drop-down menu that's used to control which type of
# leaderboard is displayed: Traditional (which is what we want), and Over/Under.
# If Over/Under is currently being displayed, switch to Traditional.
# Get current drop-down setting
webElems <- remDr$findElements(
    using = "xpath",
    # value = '//*[contains(concat( " ", @class, " " ), concat( " ", "center_cell", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "navigation_down_arrow", " " ))]'
    value = '//*[contains(concat( " ", @class, " " ), concat( " ", "center_cell", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "navigation_down_arrow", " " ))]')
resHeaders <- unlist(lapply(webElems, function(x) { x$getElementText() }))

# If drop-down is currently set to Over/Under, switch to Traditional
if(resHeaders == "Over/Under") {
    
    # Click drop-down
    target <- webElems[[which(resHeaders == "Over/Under")]]
    target$clickElement()
    
    # Click to select traditional table
    webElems <- remDr$findElements(
        using = "xpath",
        value = '//*[contains(concat( " ", @class, " " ), concat( " ", "option", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]')
    resHeaders <- unlist(lapply(webElems, function(x) { x$getElementText() }))
    target <- webElems[[which(resHeaders == "Traditional")]]
    target$clickElement() }

# Scrape leaderboard every 60 seconds and write to Google sheet
while (TRUE) {
    
    # User message
    message(paste("Initiating leaderboard refresh at", Sys.time()))
    
    # Refresh page
    remDr$refresh()
    Sys.sleep(6)
    message("Page refresh complete.")
    
    # Leaderboard doesn't seem to be structured as a table. Instead, read in all
    # data elements as a character string.
    leader_char <- remDr$getPageSource() %>%
        .[[1]] %>%
        read_html() %>%
        html_elements(".data") %>%
        html_text2()
    
    # Convert character string to a table by first finding the indices of player
    # names. Player names are considered to be at least two alpha characters,
    # but not MC (missed cut), WD (withdrawn), or GMT (Greenwich Mean Time,
    # giving tee times for players who haven't started yet). Requiring at least
    # two characters allows us to miss T (used for ties, like tied for third,
    # "T3") and F (finished).
    name_idx <- which(str_detect(leader_char, "^[A-Za-z]{2}") &
                          !str_detect(leader_char, "^(MC|WD|GMT)"))
    
    # Based on where player names occur, we can compute the indices for the
    # start and end of each row in the table.
    start <- name_idx - 1
    end <- start + 9
    
    # Iterate over start & end indices to extract rows and combine into a table
    leader_tab <- map2(start, end, function(s, e) {
        as_tibble(leader_char[s:e]) }) %>%
        list_cbind() %>%
        t() %>%
        as_tibble()
    
    # Add column names
    colnames(leader_tab) <- c("place", "player", "total_under", "thru", "today_under", "R1", "R2", "R3", "R4", "total_score")
    
    # If the place is either MC or WD then only keep place, player, R1, R2, R3,
    # and total score.
    mc_wd <- leader_tab %>%
        filter(place %in% c("MC", "WD")) %>%
        select(place, player, R1 = thru, R2 = today_under, R3 = R2,
               total_score = R3)
    
    # Players who haven't started have a start time with "GMT" in the thru col
    not_started <- leader_tab %>%
        filter(!place %in% c("MC", "WD"),
               str_detect(thru, "GMT")) %>%
        select(place:total_under, R1 = today_under, R2 = R1, R3 = R2, R4 = R3,
               total_score = R4) %>%
        mutate(across(R1:total_score, ~ if_else(.x == "", NA_character_, .x)))
    
    # If the place isn't MC or WD and thru doesn't have "GMT" then player has
    # started today's round.
    started <- leader_tab %>%
        filter(!place %in% c("MC", "WD", ""),
               !str_detect(thru, "GMT"))
    
    # Final leaderboard
    leaderboard <- bind_rows(
        started,
        not_started,
        mc_wd) %>%
        
        # Convert to numeric
        mutate(
            across(c("total_under", "today_under"),
                   ~ if_else(.x == "E", "0", .x)),
            across(
                c("total_under", "today_under", R1:total_score),
                ~ as.integer(.x)),
            
            # Add a datetime stamp. Google sheets converts all datetimes to UTC,
            # so subtract six hours to show mountain time.
            last_updated = Sys.time() - hours(6),
            
            # Need these columns to properly sort the leaderboard
            place2 = as.integer(str_extract(place, "[0-9]+")),
            thru2 = if_else(
                thru == "F"
                ,99
                ,as.integer(str_extract(thru, "[0-9]+")))) %>%
        arrange(place2, desc(thru2)) %>%
        select(-place2, -thru2)
    
    # Write to Google sheet. If there's an error, print message & continue.
    tryCatch(
        write_sheet(
            data = leaderboard,
            ss = "1-Mq_xMxERqTPUnSerpig5NU9oDVj4a09KFH1WSSedBw",
            sheet = "leaderboard"),
        error = function(e) {
            message("Error in write_sheet()")
            print(e) })
    
    # Pause 60 seconds before running loop again
    message("Waiting for next loop...")
    Sys.sleep(60)
}
