# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)

# set url ----------------------------------------------------------------------

first_url <- "https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=0"

# read first page --------------------------------------------------------------

page1 <- read_html(first_url)

# scrape titles ----------------------------------------------------------------

titles <- page1 %>%
  html_nodes(".iteminfo") %>%
  html_node("h3 a") %>%
  html_text() %>%
  str_squish()

# scrape links -----------------------------------------------------------------

links <- page1 %>%
  html_nodes(".iteminfo") %>%
  html_node("h3 a") %>%
  html_attr("href") %>%
  str_replace(".", "collections.ed.ac.uk")

# scrape artists ---------------------------------------------------------------

artists <- page1 %>%
  html_nodes(".iteminfo") %>%
  html_node(".artist") %>%
  html_text() %>%
  str_squish()

artists_links <- page1 %>%
  html_nodes(".iteminfo") %>%
  html_node(".artist") %>%
  html_attr("href") %>%
  str_replace(".", "collections.ed.ac.uk/art")


# put together in a data frame -------------------------------------------------

first_ten <- tibble(
  title = titles,
  artist = artists,
  link = links
)

# scrape second ten paintings --------------------------------------------------

second_url <- "https://collections.ed.ac.uk/art/search/*:*/Collection:%22edinburgh+college+of+art%7C%7C%7CEdinburgh+College+of+Art%22?offset=10"

page2 <- read_html(second_url)

# scrape titles ----------------------------------------------------------------

titles2 <- page2 %>%
  html_nodes(".iteminfo") %>%
  html_node("h3 a") %>%
  html_text() %>%
  str_squish()

# scrape links -----------------------------------------------------------------

links2 <- page2 %>%
  html_nodes(".iteminfo") %>%
  html_node("h3 a") %>%
  html_attr("href") %>%
  str_replace(".", "collections.ed.ac.uk")

# scrape artists ---------------------------------------------------------------

artists2 <- page2 %>%
  html_nodes(".iteminfo") %>%
  html_node(".artist") %>%
  html_text() %>%
  str_squish()

artists_links2 <- page2 %>%
  html_nodes(".iteminfo") %>%
  html_node(".artist") %>%
  html_attr("href") %>%
  str_replace(".", "collections.ed.ac.uk/art")

# put together in a data frame -------------------------------------------------

second_ten <- tibble(
  title = titles2,
  artist = artists2,
  link = links2
)