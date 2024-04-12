# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Scrape Masters leaderboard data every 60 seconds and write to a
# Google sheet.

# This package could provide a solution that uses chromote to read live pages
# and also allows for interactivity:
# https://cran.r-project.org/web/packages/selenider/index.html

# Packages
library(tidyverse)
library(rvest)
library(googlesheets4)

# Interactive authorization puts token in secrets. This only has to be done
# once when the script is first run.
# gs4_auth(email = "kenyonboyd@gmail.com", cache = ".secrets")
# gs4_deauth()

# Scrape this page
url <- "https://www.masters.com/en_US/scores/index.html"

# Write results to google sheet with this url
sheet_url <- "https://docs.google.com/spreadsheets/d/1bf5BZ46053tmu45wDafrFUM3nx2aD97VNnEPjn8sy4o/edit#gid=0"

# Authorize access to Google sheets
gs4_auth(email = "kenyonboyd@gmail.com",
         cache = ".secrets")

# Get live page
page_live <- rvest::read_html_live(url)

# Check out live page & manually switch leaderboard to traditional view if
# necessary.
page_live$view()

# Scrape leaderboard every 60 seconds and write to Google sheet
while (TRUE) {
    
    # User message
    message("Initiating leaderboard refresh...")
    
    # Leaderboard doesn't seem to be structured as a table. Instead, read in all
    # data elements as a character string from traditional leaderboard. If error
    # crops up then script should continue with old version of leader_char
    tryCatch(
        test <- page_live$html_elements(".data") |>
            html_text2(),
        error = function(e) {
            message("Error in html_elements()")
            print(e) })
    
    # Only assign test to leader_char if it has content. If not, use the stored
    # version of leader_char to complete the current loop,
    if(length(test) > 0) { leader_char <- test }
    
    # Convert character string to a table by first finding the indices of player
    # names. Player names are considered to be at least two alpha characters,
    # but not MC (missed cut), WD (withdrawn), or GMT (Greenwich Mean Time,
    # giving tee times for players who haven't started yet). Requiring at least
    # two characters allows us to miss T (used for ties, like tied for third,
    # "T3") and F (finished).
    name_idx <- which(str_detect(leader_char, "^[A-zÀ-ꭓ]{2}") &
                          !str_detect(leader_char, "^(MC|WD|GMT)"))
    
    # Table with showing where each player's data start and end. The end index
    # isn't always accurate, but gets cleaned up later.
    player_idx <- tibble(start = name_idx - 1) |>
        mutate(end = start + 9)
    
    # Iterate over start & end indices to extract each player's data & combine
    # into a list.
    leader_list <- map2(player_idx$start, player_idx$end, function(s, e) {
        as_tibble(leader_char[s:e]) })
    
    # Iterate over list, transpose data into rows, & rbind
    leader_table <- map(leader_list, function(player) {
        t(player[[1]]) |>
            as_tibble() }) |>
        list_rbind()
    
    # Add column names
    colnames(leader_table) <- c("place", "player", "total_under", "thru", "today_under", "R1", "R2", "R3", "R4", "total_score")
    
    # If the place is either MC or WD then only keep place, player, R1-R4, &
    # total score.
    mc_wd <- leader_table %>%
        filter(place %in% c("MC", "WD")) |>
        select(place, player, R1:R4, total_score)
    
    # Players who haven't started have a start time with "GMT" in the thru col.
    # All of their columns after thru need to be shifted right.
    not_started <- leader_table %>%
        filter(!place %in% c("MC", "WD"),
               str_detect(thru, "MDT")) |>
        select(place:thru, R1 = today_under, R2 = R1, R3 = R2, R4 = R3,
               total_score = R4)
        
    # If the place isn't MC or WD and thru doesn't have "GMT" then player has
    # started today's round.
    started <- leader_table %>%
        filter(!place %in% c("MC", "WD", ""),
               !str_detect(thru, "MDT"))
    
    # Final leaderboard
    leaderboard <- bind_rows(
        started,
        not_started,
        mc_wd) %>%
        
        # Add a datetime stamp. Google sheets defaults times to GMT, so subtract
        # 6 to get to mountain.
        mutate(
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
            ss = sheet_url,
            sheet = "leaderboard"),
        error = function(e) {
            message("Error in write_sheet()")
            print(e) })
    
    # Pause 60 seconds before running loop again
    message("Waiting for next loop...")
    Sys.sleep(60)
}
