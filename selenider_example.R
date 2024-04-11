# Check out slenider package for scraping. Is able to open a headed browser session, but doesn't seem to be good at finding page elements


library(tidyverse)
library(selenider)
library(rvest)


# Start session. Headless by default
# session <- selenider_session(session = "chromote")

# Can do a headed session like this, which means that chrome will open and show page
# Might need to open this on a different port because already using port in other session. Can't set port in chromote options...
session <- selenider_session(
    session = "chromote",
    options = chromote_options(headless = FALSE))



# Got to r project
open_url("https://www.r-project.org/")

# Finds most recent mastadon post on page and follows URL
s(".mt-timeline") |>
    find_element("article") |>
    elem_attr("data-location") |>
    open_url()

# For followed url, get text of post
s(".columns-area") |>
    find_element(".status__content") |>
    read_html() |>
    html_text2()






# Select all links 
ss("a")


ss("a") |>
    find_element("li:nth-child(4) a")



menu_items <- s("#rStudioHeader") |>
    find_element("#menu") |>
    find_elements(".menuItem")


# Go to masters leaderboard
open_url("https://www.masters.com/en_US/scores/index.html")

# Won't select heading level 1. Makes me think that selenider can't do this live.
ss("h1")


