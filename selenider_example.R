# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Explore the use of selenider package to scrape webpages.

# Packages
library(tidyverse)
library(selenider)
library(rvest)

# Open a headed session. With a headed session you can watch in Chrome as the
# code below operates the browser.
session <- selenider_session(
    session = "chromote",
    options = chromote_options(headless = FALSE))

# Won't open this page
open_url("https://www.masters.com/en_US/scores/index.html")

# But will open this one
open_url("https://www.r-project.org/")

# Finds most recent mastodon post on page and follows URL
s(".mt-timeline") |>
    find_element("article") |>
    elem_attr("data-location") |>
    open_url()

# For followed url, get text of post
s(".columns-area") |>
    find_element(".status__content") |>
    read_html() |>
    html_text2()

# Go back to R project
open_url("https://www.r-project.org/")
