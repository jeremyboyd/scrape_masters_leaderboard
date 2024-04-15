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
# open_url("https://www.masters.com/en_US/scores/index.html")

# Navigate to the R project website, find the link to the CRAN mirror list, check that the link is correct, and click the link element.
open_url("https://www.r-project.org/")
s(".row") |>
    find_element("div") |>
    find_elements("a") |>
    elem_find(has_text("CRAN")) |>
    
    # Checks to make sure that the element links to cran
    elem_expect(attr_contains("href", "cran.r-project.org")) |>
    elem_click()

# Now that we’re in the mirror list page, let’s find the link to every CRAN mirror in the UK.
s("dl") |>
    find_elements("dt") |>
    elem_find(has_text("UK")) |>
    find_element(xpath = "./following-sibling::dd") |>
    find_elements("tr") |>
    elem_expect(has_at_least(1)) |>
    as.list() |>
    lapply(
        \(x) x |>
            find_element("a") |>
            elem_attr("href"))

# Redo the above to use the least code and build a table with UK university
# names and links.
s("dl") |>
    find_elements("dt") |>
    elem_find(has_text("UK")) |>
    find_element(xpath = "./following-sibling::dd") |>
    find_elements("tr") |>
    elem_expect(has_at_least(1)) |>
    as.list() |>
    lapply(
        \(x) x |>
            find_element("a") |>
            elem_attr("href"))

# Get CRAN mirror links in UK
links <- ss("dt") |>
    elem_find(has_text("UK")) |>
    find_element(xpath = "./following-sibling::dd") |>
    find_elements("tr") |>
    as.list() |>
    map(\(x) x |>
            find_element("a") |>
            elem_attr("href") |>
            as_tibble()) |>
    list_rbind()

# This code extracts UK links and institution names in the same table
s("dd:nth-child(82) table") |>
    read_html() |>
    html_table() |>
    magrittr::extract2(1) |>
    select(name = X2, url = X1)


# Go to tidyverse page
open_url("https://www.tidyverse.org/")

# Extract title of page
s("#rStudioHeader") |>
    find_element("div") |>
    find_element(".productName") |>
    read_html() |>
    html_text2()

# Extract menu link names from header
link_names <- s("#rStudioHeader") |>
    find_elements(".menuItem") |>
    as.list() |>
    map(\(x) x |>
            read_html() |>
            html_text2() |>
            as_tibble()) |>
    list_rbind() |>
    rename(name = value)

# Extract menu links header
links <- s("#rStudioHeader") |>
    find_elements(".menuItem") |>
    as.list() |>
    map(\(x) x |>
            elem_attr("href") |>
            as_tibble()) |>
            list_rbind() |>
    rename(url = value)

# Combine into table
bind_cols(link_names, links)

    





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
