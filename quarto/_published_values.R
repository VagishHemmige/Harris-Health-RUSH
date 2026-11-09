# Shared by the published-results pages: reads only the two files of values
# typed in from the article, and defines the table functions. It reads no
# study data and fits nothing.
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(gtsummary)
source("../R/functions.R")
read_printed <- function(file) {
  read_csv(file, col_types = cols(.default = col_character()), na = character())
}
published_table_1 <- read_printed("published/table_1.csv")
published_tables_2_to_4 <- read_printed("published/tables_2_to_4.csv")
published_rows <- function(table_name) {
  published_tables_2_to_4 |>
    filter(table == table_name) |>
    select(-table)
}
